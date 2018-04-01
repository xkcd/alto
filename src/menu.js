import html from 'nanohtml'
import morph from 'nanomorph'
import style from 'dom-css'

import indicateLoading from './indicate-loading'

const itemBackgroundColor = '#e4e4e4'
const itemHighlightColor = '#5544ff'
const menuShadow = '0 0 10px rgba(0, 0, 0, .45)'
const menuTransition = 'opacity .15s ease-out'
const scrollHoverButtonSize = 20

const arrowStyles = css`
  .arrow {
    width: 0;
    height: 0;
    opacity: .65;
    border: 5px solid transparent;
  }

  .arrow.up {
    border-bottom: 5px solid black;
  }

  .arrow.down {
    border-top: 5px solid black;
  }

  .arrow.left {
    border-left: 5px solid black;
  }

  .arrow.right {
    border-right: 5px solid black;
  }
`

function arrow(props) {
  const {direction, size = 5, color = 'black'} = props

  const directionStyles = {
    'up': {
      borderBottom: `${size}px solid ${color}`,
    },
    'down': {
      borderTop: `${size}px solid ${color}`,
    },
    'left': {
      borderRight: `${size}px solid ${color}`,
    },
    'right': {
      borderLeft: `${size}px solid ${color}`,
    },
  }

  if (!directionStyles.hasOwnProperty(direction)) {
    throw 'invalid direction'
  }

  const el = html`<div class="${arrowStyles.arrow}" />`
  style(el, directionStyles[direction])

  return el
}

const spinnerStyles = css`
  @keyframes spin {
    from {
      transform: rotate(0deg);
    }
    to {
      transform: rotate(360deg);
    }
  }

  .spinner {
    display: flex;
    margin: 0 -2px;
    opacity: .5;
    animation: spin 1s linear infinite;
  }
`

function spinner(props) {
  const {size = 17, strokeWidth = 2} = props
  const r = size / 2 - strokeWidth

  const el = html`
    <svg class="${spinnerStyles.spinner}" viewBox="0 0 ${size} ${size}" width="${size}" height="${size}">
      <circle
        cx="${size / 2}"
        cy="${size / 2}"
        r="${r}"
        stroke="black"
        stroke-linecap="butt"
        stroke-width="${strokeWidth}"
        stroke-dasharray="${2 * Math.PI * r * .9}"
        fill="transparent"
      />
    </svg>
  `
  return el
}

function hoverMenuButton(props) {
  const {direction, onTrigger, className, intervalMs = 10} = props

  let el
  let hoverInterval
  let isHighlighted = false

  function handleMouseEnter(ev) {
    hoverInterval = setInterval(() => onTrigger(ev), intervalMs)
    isHighlighted = true
    morph(el, render())
  }

  function handleMouseLeave() {
    clearInterval(hoverInterval)
    isHighlighted = false
    morph(el, render())
  }

  function render() {
    return html`
      <div
        class="${className}"
        onclick=${onTrigger}
        onmouseenter=${handleMouseEnter}
        onmouseleave=${handleMouseLeave}
      >
        ${arrow({direction, color: isHighlighted ? 'white' : 'black'})}
      </div>
    `
  }

  el = render()

  return el
}

const menuItemStyles = css`
  .item {
    display: flex;
    font-family: sans-serif;
    font-size: 11.5pt;
    color: black;
    border-radius: 2px;
    align-items: center;
    cursor: default;
    padding: 8px 10px;
    user-select: none;
  }

  .highlight {
    background-color: ${itemHighlightColor};
    color: white;
  }

  .disabled {
    opacity: .5;
  }

  .item > span {
    flex: 1;
  }

  .item.disabled > span {
    text-shadow: 0 1px white;
  }

  .left > span {
    margin-left: 10px;
  }

  .left .spacer {
    justify-content: flex-start;
  }

  .right .spacer {
    justify-content: flex-end;
  }

  .right > span {
    margin-right: 10px;
  }

  .spacer {
    display: flex;
    width: 15px;
  }
`

function menuItem(props) {
  const {item, showArrows, isLoading, isHighlighted, onSelect, onMouseEnter, onMouseLeave, attach} = props

  let preEdgeEl
  let postEdgeEl
  if (showArrows) {
    let edgeEl
    if (isLoading) {
      edgeEl = spinner({})
    } else if (item.subMenuId) {
      edgeEl = arrow({direction: attach.x, color: isHighlighted ? 'white' : 'black'})
    }

    const edgeSpacer = html`
      <div class="${menuItemStyles.spacer}">
        ${edgeEl}
      </div>
    `
    if (attach.x === 'left') {
      preEdgeEl = edgeSpacer
    } else if (attach.x === 'right') {
      postEdgeEl = edgeSpacer
    }
  }

  const classes = [
    menuItemStyles.item,
    showArrows && menuItemStyles[attach.x],
    isHighlighted && menuItemStyles.highlight,
    item.disabled && menuItemStyles.disabled,
  ].filter(x => x)

  const el = html`
    <li
      class="${classes.join(' ')}"
      onclick=${item.disabled ? null : () => onSelect(item.menuId, item.entryIdx)}
      onmouseenter=${item.disabled ? null : ev => onMouseEnter(item, ev.target)}
      onmouseleave=${item.disabled ? null : ev => onMouseLeave(item, ev.target)}
    >
      ${preEdgeEl}
      <span>${item.label}</span>
      ${postEdgeEl}
    </li>
  `

  return el
}

const menuStyles = css`
  .menu {
    position: fixed;
    display: flex;
    background-color: ${itemBackgroundColor};
    border-radius: 3px;
    box-shadow: ${menuShadow};
    transition: ${menuTransition};
  }

  .menu > ul {
    margin: 0;
    padding: 0;
    overflow: hidden;
  }

  .menu.scrolling > ul {
    margin: ${scrollHoverButtonSize}px 0;
  }

  .scrollButton {
    display: flex;
    height: ${scrollHoverButtonSize}px;
    align-items: center;
    justify-content: center;
    background-color: ${itemBackgroundColor};
  }

  .scrollButton:hover {
    background-color: ${itemHighlightColor};
  }

  .scrollButton.top {
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    border-bottom: 1px ridge rgba(0, 0, 0, .2);
  }

  .scrollButton.bottom {
    position: absolute;
    bottom: 0;
    left: 0;
    right: 0;
    border-top: 1px groove rgba(0, 0, 0, .2);
  }
`

function menu(props) {
  const {items, itemGen, onSelect, attach, isScrolling} = props
  let itemEls
  let childMenuEl
  let hoverButtonEls
  let highlightedIdx
  let loadingIndicator

  const getParentEl = el => el.closest('.' + menuStyles.menu)

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
      onSelect,
      attach,
    })
  }

  function handleItemEnter(item, itemEl) {
    updateHighlighted(item, itemEl)
  }

  function handleItemLeave() {
    updateHighlighted(null, null)
  }

  function updateHighlighted(item, itemEl) {
    if (item && item.idx === highlightedIdx) {
      return
    }

    if (!item && childMenuEl) {
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

      if (childMenuEl) {
        childMenuEl.parentNode.removeChild(childMenuEl)
        childMenuEl = null
      }

      const itemBox = itemEl.getBoundingClientRect()
      if (item.subMenuId) {
        loadingIndicator = indicateLoading(() => {
          morph(itemEl, renderItem(item))
        })
        onSelect(item.menuId, item.idx)
        childMenuEl = showMenu({
          id: item.subMenuId,
          itemGen,
          onSelect,
          onLoad: loadingIndicator.finished,
          parentBox: itemBox,
          attach,
        })
        getParentEl(itemEl).appendChild(childMenuEl)
      }
    }
  }

  function handleScrollWheel(ev) {
    ev.currentTarget.scrollTop += ev.deltaY
  }

  function scrollUp(ev) {
    getParentEl(ev.target).querySelector('ul').scrollTop -= 6
  }

  function scrollDown(ev) {
    getParentEl(ev.target).querySelector('ul').scrollTop += 6
  }

  if (items) {
    itemEls = items.map(renderItem)
  }

  if (isScrolling) {
    hoverButtonEls = [
      hoverMenuButton({
        className: `${menuStyles.scrollButton} ${menuStyles.top}`,
        onTrigger: scrollUp,
        direction: 'up',
      }),
      hoverMenuButton({
        className: `${menuStyles.scrollButton} ${menuStyles.bottom}`,
        onTrigger: scrollDown,
        direction: 'down',
      }),
    ]
  }

  const classes = [
    menuStyles.menu,
    isScrolling && menuStyles.scrolling,
  ].filter(x => x)

  const el = html`
    <div class="${classes.join(' ')}">
      <ul onwheel=${handleScrollWheel}>
        ${itemEls}
      </ul>
      ${hoverButtonEls}
    </div>
  `
  style(el, {
    opacity: items ? 1 : 0,
  })

  return el
}

function positionMenu(el, parentBox, attach) {
  style(el, {
    left: -9999,
    top: 0,
  })
  document.body.appendChild(el)
  const menuBox = el.getBoundingClientRect()
  const menuWidth = Math.ceil(menuBox.width)
  const menuHeight = Math.ceil(menuBox.height)
  document.body.removeChild(el)

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

function showMenu(props) {
  const {itemGen, onSelect, onLoad, id, parentBox, attach} = props

  let el

  async function loadMenu() {
    const items = await itemGen(id)

    // render for size measurement
    let sizingEl = menu({
      items,
      itemGen,
      attach
    })
    const {pos, childAttach, isScrolling} = positionMenu(sizingEl, parentBox, attach)

    // render
    const finalEl = menu({
      items,
      itemGen,
      onSelect,
      attach: childAttach,
      isScrolling,
    })
    style(finalEl, pos)

    morph(el, finalEl)

    if (onLoad) {
      onLoad()
    }
  }

  el = menu({})
  loadMenu()

  return el
}

// XXX work around https://github.com/babel/babylon/issues/257
export default showMenu
