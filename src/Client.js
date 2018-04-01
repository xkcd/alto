import fetch from 'unfetch'
import nanoid from 'nanoid/generate'
import nanoidChars from 'nanoid/url'

export default class Client {
  constructor(baseURL) {
    this.baseURL = baseURL
    this.cache = new Map()
    this.sessionId = nanoid(nanoidChars.substr(2), 22)
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

  log(id) {
    fetch(`${this.baseURL}/visit/${this.sessionId}/${id}?${Date.now()}`).catch(e => {})
  }
}
