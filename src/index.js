import html from 'nanohtml'

import {ALTO_ENDPOINT} from './config'
import Client from './client'
import StateMachine from './state-machine'
import {showMenu} from './ui/menu'

async function main() {
  const client = new Client(ALTO_ENDPOINT)
  const state = new StateMachine(client)

  await state.init()

  const comicEl = document.querySelector('#comic')
  let menuEl

  function closeMenu() {
    if (menuEl) {
      document.body.removeChild(menuEl)
      menuEl = null
    }
  }

  async function openMenu(ev) {
    closeMenu()
    menuEl = await showMenu({
      id: null,
      itemGen: id => state.itemGen(id),
      onSelect: handleSelect,
      parentEl: document.body,
      parentBox: {left: ev.clientX, right: ev.clientX, top: ev.clientY},
      attach: {x: 'right', y: 'top'},
    })
    document.body.appendChild(menuEl)
  }

  async function handleSelect(menuId, entryIdx) {
    const menuFinished = await state.handleSelect(menuId, entryIdx)
    if (menuFinished) {
      closeMenu()
    }
  }

  window.addEventListener('mousedown', ev => {
    if (menuEl && menuEl.contains(ev.target)) {
      return
    }
    closeMenu()
  })

  comicEl.addEventListener('contextmenu', ev => {
    ev.preventDefault()
    openMenu(ev)
  })
}

main()
