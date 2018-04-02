import html from 'nanohtml'

import debug from './debug'

export default class StateMachine {
  constructor(client, effectMap) {
    this.client = client
    this.effectMap = effectMap
    this.tags = null
    this.rootId = null
  }

  async init() {
    let root = await this.client.get(null)
    this.tags = new Map(Object.entries(root.State.Tags))
    this.rootId = root.Menu.id
    this.prefetch(this.rootId, 2)
  }

  evalTagLogic(whenTree) {
    const evalMap = {
      'Always': () => true,
      'TLAnd': x => x.contents.every(exec),
      'TLOr': x => x.contents.some(exec),
      'TLNot': x => !exec(x.contents),
      'TagSet': x => this.tags.has(x.contents),
      'TagUnset': x => !this.tags.has(x.contents),
    }

    function exec(x) {
      if (!evalMap.hasOwnProperty(x.tag)) {
        throw 'unexpected tag logic operator'
      }
      return evalMap[x.tag](x)
    }

    return exec(whenTree)
  }

  evalSubMenuId(reaction) {
    if (!reaction.tag === 'SubMenu') {
      return
    }

    const {subMenu, subIdPostfix} = reaction
    if (subIdPostfix) {
      return subMenu + this.tags.get(subIdPostfix)
    }
    return subMenu
  }

  async prefetch(id, depth=1) {
    let fetches = []
    let data = await this.client.get(id)
    for (const entry of data.entries) {
      if (!this.evalTagLogic(entry.display)) {
        continue
      }

      if (entry.reaction.subIdPostfix) {
        // don't prefetch ids containing dynamic tags
        continue
      }

      const subMenuId = this.evalSubMenuId(entry.reaction)
      if (!subMenuId) {
        continue
      }

      fetches.push(this.client.get(subMenuId))

      if (depth > 1) {
        fetches.push(this.prefetch(subMenuId, depth - 1))
      }
    }

    return Promise.all(fetches)
  }

  async itemGen(id) {
    if (id === null) {
      const rootData = await this.client.get(this.rootId)

      // the root menu is special. find the first displayed submenu of the root.
      for (const entry of rootData.entries) {
        const {display, reaction} = entry
        if (this.evalTagLogic(display)) {
          id = this.evalSubMenuId(reaction)
          break
        }
      }
    }
  
    const data = await this.client.get(id)

    let menuItems = []
    for (let idx = 0; idx < data.entries.length; idx++) {
      const entry = data.entries[idx]
      const {display, active, reaction} = entry

      if (!this.evalTagLogic(display)) {
        continue
      }

      menuItems.push({
        menuId: id,
        entryIdx: idx,
        idx: menuItems.length,
        label: entry.label,
        disabled: active && !this.evalTagLogic(active),
        subMenuId: this.evalSubMenuId(reaction),
      })
    }

    this.prefetch(id)
    return menuItems
  }

  performAction(act) {
    if (act.tag === 'Nav') {
      window.open(act.url)
    } else if (act.tag === 'Download') {
      try {
        const a = html`
          <a
            href="${act.url}"
            download="${act.filename}"
          />
        `
        a.click()
      } catch (err) {
        window.open(act.url)
      }
    } else if (act.tag === 'JSCall') {
      const effectName = act.jsCall
      if (!this.effectMap.has(effectName)) {
        debug.warn('missing effect:', effectName)
        return
      }
      this.effectMap.get(effectName)()
    }
  }

  updateTags(tagChange) {
    const {setTags, unsetTags} = tagChange
    let changes = {set: [], deleted: []}

    if (setTags) {
      for (const [key, value] of Object.entries(setTags)) {
        this.tags.set(key, value)
        changes.set.push([key, value])
      }
    }

    if (unsetTags) {
      for (const key of unsetTags) {
        this.tags.delete(key)
        changes.deleted.push(key)
      }
    }

    if (changes.set.length || changes.deleted.length) {
      debug.log('tags changed:', changes)
    }
  }

  async handleEnter(menuId, entryIdx, subMenuId) {
    const {entries} = await this.client.get(menuId)
    const {reaction} = entries[entryIdx]

    if (reaction.onAction) {
      this.updateTags(reaction.onAction)
    }

    this.client.logEnter(menuId, subMenuId)
  }

  async handleSelect(menuId, entryIdx) {
    const {entries} = await this.client.get(menuId)
    const {reaction} = entries[entryIdx]

    if (reaction.onAction) {
      this.updateTags(reaction.onAction)
    }

    if (reaction.act) {
      this.performAction(reaction.act)
    }

    this.client.logVisit(menuId, entryIdx)
  }

  async handleLeave(menuId) {
    const {onLeave} = await this.client.get(menuId)

    if (onLeave) {
      this.updateTags(onLeave)
    }
  }
}
