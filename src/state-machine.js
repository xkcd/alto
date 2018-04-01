import Client from './client'

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
        console.log(entry, display, this.evalTagLogic(display))
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

  async handleSelect(menuId, entryIdx) {
    const {entries} = await this.client.get(menuId)
    const {reaction} = entries[entryIdx]

    if (reaction.setTags) {
      for (const [key, value] of Object.entries(reaction.setTags)) {
        this.tags.set(key, value)
      }
    }

    if (reaction.unsetTags) {
      for (const key of reaction.unsetTags) {
        this.tags.delete(key)
      }
    }

    if (reaction.act) {
      if (reaction.act.tag === 'Nav') {
        window.open(reaction.act.url)
      }
    }
  }
}
