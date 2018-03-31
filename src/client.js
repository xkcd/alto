import fetch from 'unfetch'

export default class Client {
  constructor(baseURL) {
    this.baseURL = baseURL
    this.cache = new Map()
  }

  async get(id) {
    if (this.cache.has(id)) {
      return this.cache.get(id)
    }

    let path
    if (id == null) {
      path = '/root'
    } else {
      path = `/menu/${id}`
    }

    let resp = await fetch(this.baseURL + path)
    let data = await resp.json()

    this.cache.set(id, data)
    if (data.Menu) {
      this.cache.set(data.Menu.id, data.Menu)
    }

    return data
  }
}
