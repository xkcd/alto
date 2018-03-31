import classNames from 'classnames'
import html from 'nanohtml'
import morph from 'nanomorph'
import style from 'dom-css'

import indicateLoading from './indicate-loading'

const itemBackgroundColor = '#e4e4e4'
const itemHighlightColor = '#f4f4f4'
const menuShadow = '0 0 15px rgba(0, 0, 0, .5)'
const menuTransition = 'opacity .15s ease-out'

function arrow(props) {
  const {direction, size = 5} = props

  const directionStyles = {
    'right': {
      borderTop: `${size}px solid transparent`,
      borderBottom: `${size}px solid transparent`,
      borderLeft: `${size}px solid black`,
    },
    'left': {
      borderTop: `${size}px solid transparent`,
      borderBottom: `${size}px solid transparent`,
      borderRight: `${size}px solid black`,
    }
  }

  if (!directionStyles.hasOwnProperty(direction)) {
    throw 'invalid direction'
  }

  const el = html`<div />`
  style(el, {
    width: 0,
    height: 0,
    opacity: .5,
    ...directionStyles[direction],
  })

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
    margin: 0 -6px;
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

const menuItemStyles = css`
  .item {
    display: flex;
    align-items: center;
    cursor: default;
    padding: 8px 10px;
    user-select: none;
  }

  .item > span {
    flex: 1;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .left > span {
    margin-left: 20px;
  }

  .right > span {
    margin-right: 20px;
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
      edgeEl = arrow({direction: attach.x})
    }
    if (edgeEl) {
      if (attach.x === 'left') {
        preEdgeEl = edgeEl
      } else if (attach.x === 'right') {
        postEdgeEl = edgeEl
      }
    }
  }

  const el = html`
    <li
      class="${classNames(menuItemStyles.item, showArrows && menuItemStyles[attach.x])}"
      onclick=${() => onSelect(item.menuId, item.idx)}
      onmouseenter=${ev => onMouseEnter(item, ev.target)}
      onmouseleave=${ev => onMouseLeave(item, ev.target)}
    >
      ${preEdgeEl}
      <span>${item.label}</span>
      ${postEdgeEl}
    </li>
  `
  style(el, {
    backgroundColor: isHighlighted ? itemHighlightColor : itemBackgroundColor,
  })

  return el
}

const menuStyles = css`
  .menu {
    position: fixed;
    overflow: hidden;
    box-shadow: ${menuShadow};
    transition: ${menuTransition};
  }

  .menu > ul {
    margin: 0;
    padding: 0;
  }
`

function menu(props) {
  const {items, itemGen, onSelect, attach} = props
  let itemEls
  let childMenuEl
  let highlightedIdx
  let loadingIndicator

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

      if (loadingIndicator) {
        loadingIndicator.cancel()
        loadingIndicator = null
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
        itemEl.parentNode.parentNode.appendChild(childMenuEl)
      }
    }
  }

  if (items) {
    itemEls = items.map(renderItem)
  }

  const el = html`
    <div class="${menuStyles.menu}">
      <ul>
        ${itemEls}
      </ul>
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
  const menuWidth = Math.floor(menuBox.width)
  const menuHeight = Math.floor(menuBox.height)
  document.body.removeChild(el)

  // measure position and flip attach direction if necessary
  const {innerHeight, innerWidth} = window
  const childAttach = {...attach}

  const pos = {}
  const leftUnderHang = parentBox.left - menuWidth - 1
  const rightOverHang = parentBox.right + menuWidth - innerWidth

  // if there's x under/overhang swap to side with most space and fill remaining space.
  if (attach.x === 'left' && leftUnderHang < 0 && rightOverHang < -leftUnderHang) {
    childAttach.x = 'right'
  } else if (attach.x === 'right' && rightOverHang > 0 && -leftUnderHang < rightOverHang) {
    childAttach.x = 'left'
  }
  if (childAttach.x === 'left') {
    pos.left = Math.max(0, leftUnderHang)
    pos.maxWidth = parentBox.left - pos.left
  } else if (childAttach.x === 'right') {
    pos.left = parentBox.right
    pos.maxWidth = innerWidth - pos.left
  }

  // y positioning is easier: when it hits the screen edge offset, possibly filling vertical space.
  if (attach.y === 'bottom') {
    pos.top = parentBox.bottom - menuHeight
    if (pos.top < 0) {
      pos.top = 0
      childAttach.y = 'top'
    }
  } else if (attach.y === 'top') {
    pos.top = parentBox.top
    const overHang = parentBox.top + menuHeight - innerHeight
    if (overHang > 0) {
      pos.top = Math.max(0, pos.top - overHang)
      childAttach.y = 'bottom'
    }
  }
  pos.maxHeight = innerHeight - pos.top

  return {pos, childAttach}
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
    const {pos, childAttach} = positionMenu(sizingEl, parentBox, attach)

    // render
    const finalEl = menu({
      items,
      itemGen,
      onSelect,
      attach: childAttach,
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
