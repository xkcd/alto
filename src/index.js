import html from 'nanohtml'

import {ALTO_ENDPOINT} from './config'
import Client from './Client'
import StateMachine from './StateMachine'
import {attachMenuTo} from './ui/menu'

async function main() {
  const client = new Client(ALTO_ENDPOINT)
  const state = new StateMachine(client)

  await state.init()

  attachMenuTo({
    triggerEl: document.querySelector('#comic'),
    id: null,
    itemGen: state.itemGen.bind(state),
    onMenuSelect: state.handleSelect.bind(state),
    onMenuEnter: state.handleEnter.bind(state),
    onMenuLeave: state.handleLeave.bind(state),
  })
}

main()
