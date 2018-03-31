import Client from './client'
import StateMachine from './state-machine'
import showMenu from './menu'

async function main() {
  const client = new Client('http://35.224.103.20:8081')
  const state = new StateMachine(client)

  await state.init()

  showMenu({
    id: null,
    itemGen: id => state.itemGen(id),
    parentEl: document.body,
    parentBox: {right: 0, top: 0},
    attach: {x: 'right', y: 'top'},
  })
}

main()
