import html from 'nanohtml'

export default class StateMachine {
  constructor(client) {
    this.client = client
    this.tags = null
    this.rootId = null
  }

  async init() {
    let root = await this.client.get()
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
    if (id == null) {
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
    }
  }

  updateTags(tagChange) {
    const {setTags, unsetTags} = tagChange

    if (setTags) {
      for (const [key, value] of Object.entries(setTags)) {
        this.tags.set(key, value)
      }
    }

    if (unsetTags) {
      for (const key of unsetTags) {
        this.tags.delete(key)
      }
    }
  }

  async handleSelect(menuId, entryIdx) {
    const {entries} = await this.client.get(menuId)
    const {reaction} = entries[entryIdx]

    this.client.log(menuId)

    if (reaction.onAction) {
      this.updateTags(reaction.onAction)
    }

    if (reaction.act) {
      this.performAction(reaction.act)
    }

    return !reaction.tag === 'SubMenu'
  }

  async handleLeave(menuId) {
    const {onLeave} = await this.client.get(menuId)

    if (onLeave) {
      this.updateTags(onLeave)
    }
  }
}
