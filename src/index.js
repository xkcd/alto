import html from 'nanohtml'

import {ALTO_ENDPOINT} from './config'
import Client from './Client'
import StateMachine from './StateMachine'
import {showMenu} from './ui/menu'

async function main() {
  const client = new Client(ALTO_ENDPOINT)
  const state = new StateMachine(client)

  await state.init()

  const comicEl = document.querySelector('#comic')
  let menuObj
  let isTouching = false
  let longPressTimeout

  function closeMenu() {
    if (menuObj) {
      menuObj.closeMenu()
      menuObj = null
    }
  }

  function closeMenuIfOutside(ev) {
    if (menuObj && menuObj.el.contains(ev.target)) {
      return
    }
    closeMenu()
  }

  async function openMenu(pos) {
    closeMenu()
    menuObj = await showMenu({
      id: null,
      itemGen: id => state.itemGen(id),
      onMenuSelect: handleSelect,
      onMenuLeave: id => state.handleLeave(id),
      parentBox: {left: pos.x, right: pos.x, top: pos.y},
      attach: {x: 'right', y: 'top'},
    })
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
