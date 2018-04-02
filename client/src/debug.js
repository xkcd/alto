import {DEBUG} from './flags'

export default {
  log(...args) {
    if (!DEBUG) {
      return
    }
    console.log(...args)
  },

  warn(...args) {
    if (!DEBUG) {
      return
    }
    console.warn(...args)
  }
}
