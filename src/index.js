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
  let isTouching = false
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
    if (!isTouching) {
      // prevent contextmenu from double-triggering on long press in Chrome.
      openMenu({x: ev.clientX, y: ev.clientY})
    }
  })


  // we have to implement our own long press detection because iOS Safari
  // doesn't trigger contextmenu on touch.
  comicEl.addEventListener('touchstart', ev => {
    isTouching = true
    longPressTimeout = setTimeout(() => {
      openMenu({
        x: Math.floor(ev.touches[0].clientX),
        y: Math.floor(ev.touches[0].clientY),
      })
    }, 250)
  })

  comicEl.addEventListener('touchmove', () => {
    clearTimeout(longPressTimeout)
  })

  comicEl.addEventListener('touchend', () => {
    isTouching = false
    clearTimeout(longPressTimeout)
  })
}

main()
