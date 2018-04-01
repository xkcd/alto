const effectMap = new Map()

const requireEffectModule = require.context('./', false, /\.js$/)
for (const key of requireEffectModule.keys()) {
  if (key === './index.js') {
    continue
  }

  const module = requireEffectModule(key)
  for (const [name, func] of Object.entries(module)) {
    if (effectMap.has(name)) {
      throw `duplicate name: ${name}`
    }

    effectMap.set(name, func)
  }
}

export default effectMap
