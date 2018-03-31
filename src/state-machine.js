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
  }

  evalTagLogic(whenTree) {
    const evalMap = {
      'When': x => exec(x.contents), // TODO: remove?
      'Always': () => true,
      'TLAnd': x => x.contents.every(exec),
      'TLOr': x => x.contents.some(exec),
      'TLNot': x => !exec(x.contents),
      'TagSet': x => this.tags.has(x.contents),
      'TagUnset': x => !this.tags.has(x.contents),  // TODO: remove?
    }

    function exec(x) {
      if (!evalMap.hasOwnProperty(x.tag)) {
        throw 'unexpected tag logic operator'
      }
      return evalMap[x.tag](x)
    }

    return exec(whenTree)
  }

  async prefetch(id, depth=2) {
    let fetches = []
    let data = await this.client.get(id)
    for (const entry of data.entries) {
      const {subMenu, subIdPostfix, setTags} = entry
      if (!subMenu) {
        continue
      }

      if (subIdPostfix) {
        fetches.push(this.client.get(subMenu + this.tags.get(subIdPostfix)))
      } else {
        fetches.push(this.client.get(subMenu))
      }

      if (depth > 1) {
        fetches.push(this.prefetch(entry.id, depth - 1))
      }
    }

    return Promise.all(fetches)
  }

  async itemGen(id) {
    if (id == null) {
      id = this.rootId
    }
  
    let menuItems = []
    let data = await this.client.get(id)
    for (let idx = 0; idx < data.entries.length; idx++) {
      const entry = data.entries[idx]
      const {reaction, display, disabled} = entry

      if (!this.evalTagLogic(display)) {
        continue
      }

      let subMenuId
      if (reaction.subMenu) {
        if (reaction.subMenuPostfix) {
          subMenuId = reaction.subMenu + reaction.subMenuPostfix
        } else {
          subMenuId = reaction.subMenu
        }
      }

      menuItems.push({
        menuId: id,
        idx,
        label: entry.label,
        disabled: disabled && this.evalTagLogic(disabled),
        reaction: entry.reaction,
        subMenuId,
      })
    }

    this.prefetch(id)
    return menuItems
  }

  async handleSelect(menuId, idx) {
    const {entries} = await this.client.get(menuId)
    const {reaction} = entries[idx]

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
