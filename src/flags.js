const hash = location.hash.substr(1)
const flags = new Map()
if (hash.length) {
  for (const flag of hash.split(',')) {
    const parts = flag.split('=', 2)
    if (parts.length === 2) {
      flags.set(parts[0], parts[1])
    } else {
      flags.set(flag, true)
    }
  }
}

export const DEBUG = flags.has('debug')
export const ENDPOINT = flags.get('endpoint')
