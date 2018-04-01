import html from 'nanohtml'

import {ENDPOINT} from './flags'
import {ALTO_ENDPOINT} from './config'
import Client from './Client'
import effectMap from './effects'
import StateMachine from './StateMachine'
import {attachMenuTo} from './ui/menu'

async function main() {
  const client = new Client(ENDPOINT || ALTO_ENDPOINT)
  const state = new StateMachine(client, effectMap)

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
