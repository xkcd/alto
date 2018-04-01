import fetch from 'unfetch'

export default class Client {
  constructor(baseURL) {
    this.baseURL = baseURL
    this.cache = new Map()
  }

  async get(id) {
    if (this.cache.has(id)) {
      return await this.cache.get(id)
    }

    let path
    if (id == null) {
      path = '/root'
    } else {
      path = `/menu/${id}`
    }

    const dataFetch = fetch(this.baseURL + path).then(resp => resp.json())
    this.cache.set(id, dataFetch)

    const data = await dataFetch
    if (data.Menu) {
      this.cache.set(data.Menu.id, Promise.resolve(data.Menu))
    }
    return data
  }
}
