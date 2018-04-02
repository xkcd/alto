import html from 'nanohtml'
import morph from 'nanomorph'
import style from 'dom-css'

import indicateLoading from '../indicateLoading'
import hoverMenuButton from './hoverMenuButton'
import menuItem from './menuItem'

const scrollHoverButtonSize = 20

export const styles = css`
  @import "./colors.less";

  .menu {
    position: fixed;
    display: flex;
    max-width: 250px;
    background-color: @itemBackgroundColor;
    border-radius: 3px;
    box-shadow: 0 0 10px rgba(0, 0, 0, .45);
    transition: opacity .15s ease-out;
    text-align: left;
    font-variant: none;

    & > ul {
      margin: 0;
      padding: 0;
      overflow: hidden;
    }

    &.scrolling > ul {
      margin: ${scrollHoverButtonSize}px 0;
    }
  }

  .scrollButton {
    display: flex;
    height: ${scrollHoverButtonSize}px;
    align-items: center;
    justify-content: center;
    background-color: @itemBackgroundColor;

    &:hover{
      background-color: @itemHighlightColor;
    }


    &.top {
      position: absolute;
      top: 0;
      left: 0;
      right: 0;
      border-bottom: 1px ridge rgba(0, 0, 0, .2);
    }

    &.bottom {
      position: absolute;
      bottom: 0;
      left: 0;
      right: 0;
      border-top: 1px groove rgba(0, 0, 0, .2);
    }
  }
`

export default function menu(props) {
  const {items, itemGen, onMenuSelect, onMenuEnter, onMenuLeave, attach, isScrolling} = props
  let itemEls
  let childMenu
  let hoverButtonEls
  let highlightedIdx
  let loadingIndicator

  const getParentEl = el => el.closest('.' + styles.menu)

  function renderItem(item) {
    const isHighlighted = item.idx === highlightedIdx
    return menuItem({
      item: item,
      showArrows: items.some(x => x.subMenuId),
      isHighlighted,
      isLoading: isHighlighted && loadingIndicator && loadingIndicator.isLoading,
      itemGen,
      onMouseEnter: handleItemEnter,
      onMouseLeave: handleItemLeave,
      onItemSelect: handleItemSelect,
      attach,
    })
  }

  function closeSubMenu() {
    if (!childMenu) {
      return
    }
    childMenu.closeMenu()
    childMenu = null
  }

  function handleItemEnter(item, itemEl) {
    updateHighlighted(item, itemEl)
  }

  function handleItemLeave() {
    updateHighlighted(null, null)
  }

  function handleItemSelect(item) {
    onMenuSelect(item.menuId, item.entryIdx)
  }

  function updateHighlighted(item, itemEl) {
    if (item && item.idx === highlightedIdx) {
      return
    }

    if (!item && childMenu) {
      return
    }

    if (loadingIndicator) {
      loadingIndicator.cancel()
      loadingIndicator = null
    }

    const oldHighlightedIdx = highlightedIdx
    highlightedIdx = item && item.idx

    if (oldHighlightedIdx != null) {
      morph(itemEls[oldHighlightedIdx], renderItem(items[oldHighlightedIdx]))
    }

    if (itemEl) {
      morph(itemEl, renderItem(item))

      closeSubMenu()

      const itemBox = itemEl.getBoundingClientRect()
      if (item.subMenuId) {
        loadingIndicator = indicateLoading(() => {
          morph(itemEl, renderItem(item))
        })
        onMenuEnter(item.menuId, item.entryIdx, item.subMenuId)
        childMenu = showMenu({
          id: item.subMenuId,
          itemGen,
          onMenuSelect,
          onMenuEnter,
          onMenuLeave,
          onLoad: loadingIndicator.finished,
          parentBox: itemBox,
          attach,
        })
      }
    }
  }

  function handleScrollWheel(ev) {
    closeSubMenu()
    handleItemLeave()
    ev.currentTarget.scrollTop += ev.deltaY
    ev.preventDefault()
  }

  function scrollUp(ev) {
    closeSubMenu()
    handleItemLeave()
    getParentEl(ev.target).querySelector('ul').scrollTop -= 6
  }

  function scrollDown(ev) {
    closeSubMenu()
    handleItemLeave()
    getParentEl(ev.target).querySelector('ul').scrollTop += 6
  }

  if (items) {
    itemEls = items.map(renderItem)
  }

  if (isScrolling) {
    hoverButtonEls = [
      hoverMenuButton({
        className: `${styles.scrollButton} ${styles.top}`,
        onTrigger: scrollUp,
        direction: 'up',
      }),
      hoverMenuButton({
        className: `${styles.scrollButton} ${styles.bottom}`,
        onTrigger: scrollDown,
        direction: 'down',
      }),
    ]
  }

  const classes = [
    styles.menu,
    isScrolling && styles.scrolling,
  ].filter(x => x)

  const el = html`
    <div
      class="${classes.join(' ')}"
      oncontextmenu=${ev => {ev.preventDefault()}}
    >
      <ul onwheel=${handleScrollWheel}>
        ${itemEls}
      </ul>
      ${hoverButtonEls}
    </div>
  `
  style(el, {
    opacity: items ? 1 : 0,
  })

  return {el, closeSubMenu}
}

function positionMenu(el, parentBox, attach) {
  style(el, {
    left: -9999,
    top: 0,
  })
  document.body.appendChild(el)
  const menuWidth = Math.ceil(el.getBoundingClientRect().width)

  // measure position and flip attach direction if necessary
  const {innerHeight, innerWidth} = window
  const childAttach = {...attach}

  const pos = {}
  const parentTop = Math.ceil(parentBox.top)
  const parentBottom = Math.ceil(parentBox.bottom)
  const parentLeft = Math.ceil(parentBox.left)
  const parentRight = Math.ceil(parentBox.right)
  const leftUnderHang = parentLeft - menuWidth
  const rightOverHang = parentRight + menuWidth - innerWidth

  // if there's x under/overhang swap to side with most space and fill remaining space.
  if (attach.x === 'left' && leftUnderHang < 0 && rightOverHang < -leftUnderHang) {
    childAttach.x = 'right'
  } else if (attach.x === 'right' && rightOverHang > 0 && -leftUnderHang < rightOverHang) {
    childAttach.x = 'left'
  }
  if (childAttach.x === 'left') {
    pos.left = Math.max(0, leftUnderHang)
    pos.width = Math.min(menuWidth, parentLeft - pos.left)
  } else if (childAttach.x === 'right') {
    pos.left = parentRight
    pos.width = Math.min(menuWidth, innerWidth - pos.left)
  }

  // width affects text wrapping which affects height, so we do this now.
  style(el, pos)
  const menuHeight = Math.ceil(el.getBoundingClientRect().height)
  document.body.removeChild(el)

  // y positioning is easier: when it hits the screen edge offset, possibly filling vertical space.
  if (attach.y === 'bottom') {
    pos.top = parentBottom - menuHeight
    if (pos.top < 0) {
      pos.top = 0
      childAttach.y = 'top'
    }
  } else if (attach.y === 'top') {
    pos.top = parentTop
    const overHang = parentTop + menuHeight - innerHeight
    if (overHang > 0) {
      pos.top = Math.max(0, pos.top - overHang)
      childAttach.y = 'bottom'
    }
  }
  pos.maxHeight = innerHeight - pos.top

  const isScrolling = menuHeight > pos.maxHeight

  return {pos, childAttach, isScrolling}
}

export function showMenu(props) {
  const {itemGen, onMenuSelect, onMenuEnter, onMenuLeave, onLoad, id, parentBox, attach} = props

  let menuObj
  let menuEl

  async function loadMenu() {
    const items = await itemGen(id)

    // render for size measurement
    let {el: sizingEl} = menu({
      items,
      itemGen,
      attach
    })
    const {pos, childAttach, isScrolling} = positionMenu(sizingEl, parentBox, attach)

    // render
    menuObj = menu({
      items,
      itemGen,
      onMenuSelect,
      onMenuEnter,
      onMenuLeave,
      attach: childAttach,
      isScrolling,
    })
    style(menuObj.el, pos)
    morph(menuEl, menuObj.el)

    if (onLoad) {
      onLoad()
    }
  }

  function closeMenu() {
    menuObj.closeSubMenu()
    document.body.removeChild(menuEl)
    onMenuLeave(id)
  }

  menuObj = menu({})
  menuEl = menuObj.el
  document.body.appendChild(menuEl)

  setTimeout(loadMenu, 0)

  return {el: menuEl, closeMenu}
}

export function attachMenuTo(props) {
  const {triggerEl, menuProps, id, itemGen, onMenuSelect, onMenuEnter, onMenuLeave} = props
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
    if (menuObj && ev.target.closest('.' + styles.menu)) {
      return
    }
    closeMenu()
  }

  function handleMenuSelect(menuId, entryIdx) {
    closeMenu()
    onMenuSelect(menuId, entryIdx)
  }

  async function openMenu(pos) {
    closeMenu()
    menuObj = await showMenu({
      id,
      itemGen,
      onMenuSelect: handleMenuSelect,
      onMenuEnter,
      onMenuLeave,
      parentBox: {left: pos.x, right: pos.x, top: pos.y},
      attach: {x: 'right', y: 'top'},
    })
  }

  window.addEventListener('mousedown', closeMenuIfOutside)

  window.addEventListener('touchstart', closeMenuIfOutside)

  triggerEl.addEventListener('contextmenu', ev => {
    ev.preventDefault()
    if (!isTouching) {
      // prevent contextmenu from double-triggering on long press in Chrome.
      openMenu({x: ev.clientX, y: ev.clientY})
    }
  })

  // we have to implement our own long press detection because iOS Safari
  // doesn't trigger contextmenu on touch.
  triggerEl.addEventListener('touchstart', ev => {
    isTouching = true
    longPressTimeout = setTimeout(() => {
      openMenu({
        x: Math.floor(ev.touches[0].clientX),
        y: Math.floor(ev.touches[0].clientY),
      })
    }, 250)
  })

  triggerEl.addEventListener('touchmove', () => {
    clearTimeout(longPressTimeout)
  })

  triggerEl.addEventListener('touchend', () => {
    isTouching = false
    clearTimeout(longPressTimeout)
  })
}
