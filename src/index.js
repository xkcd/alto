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
  let longPressTimeout

  function closeMenu() {
    if (menuEl) {
      document.body.removeChild(menuEl)
      menuEl = null
    }
  }

  function closeMenuIfOutside(ev) {
    if (menuEl && menuEl.contains(ev.target)) {
      return
    }
    closeMenu()
  }

  async function openMenu(pos) {
    closeMenu()
    menuEl = await showMenu({
      id: null,
      itemGen: id => state.itemGen(id),
      onSelect: handleSelect,
      parentEl: document.body,
      parentBox: {left: pos.x, right: pos.x, top: pos.y},
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

  window.addEventListener('mousedown', closeMenuIfOutside)

  window.addEventListener('touchstart', closeMenuIfOutside)

  comicEl.addEventListener('contextmenu', ev => {
    ev.preventDefault()
    openMenu({x: ev.clientX, y: ev.clientY})
  })

  comicEl.addEventListener('touchstart', ev => {
    ev.preventDefault()
    longPressTimeout = setTimeout(() => {
      openMenu({x: ev.touches[0].clientX, y: ev.touches[0].clientY})
    }, 500)
  })

  comicEl.addEventListener('touchend', () => {
    clearTimeout(longPressTimeout)
  })
}

main()
