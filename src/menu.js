import html from 'nanohtml'
import style from 'dom-css'

const itemBackgroundColor = '#e4e4e4'
const itemHighlightColor = '#f4f4f4'

function menuItem(props) {
  const {item, itemGen, onSelect, onMouseEnter, onMouseLeave, attach} = props

  const textEl = html`
    <span>${item.label}</span>
  `
  style(textEl, {
    overflow: 'hidden',
    textOverflow: 'ellipsis',
    whiteSpace: 'nowrap',
  })

  const el = html`
    <li
      onclick=${() => onSelect(item.menuId, item.idx)}
      onmouseenter=${ev => onMouseEnter(item, ev.target)}
      onmouseleave=${ev => onMouseLeave(item, ev.target)}
    >${textEl}</li>
  `
  style(el, {
    display: 'flex',
    alignItems: 'center',
    cursor: 'default',
    padding: '8px 10px',
    backgroundColor: itemBackgroundColor,
    userSelect: 'none',
  })

  if (item.subMenuId) {
    const caretEl = html`<div />`
    style(caretEl, {
      width: 0,
      height: 0,
      opacity: .5,
      borderTop: '5px solid transparent',
      borderBottom: '5px solid transparent',
    })
    if (attach.x === 'left') {
      style(caretEl, {
        marginRight: 10,
        borderRight: '5px solid black',
      })
      el.insertBefore(caretEl, el.firstChild)
    } else if (attach.x === 'right') {
      style(caretEl, {
        marginLeft: 10,
        borderLeft: '5px solid black',
      })
      el.appendChild(caretEl)
    }
  }

  return el
}

function menu(props) {
  const {items, itemGen, onSelect, attach} = props

  let childMenuEl
  let highlightedItemEl

  const itemEls = items.map(item => menuItem({
    item,
    itemGen,
    onMouseEnter: handleItemEnter,
    onMouseLeave: handleItemLeave,
    onSelect,
    attach,
  }))

  const el = html`
    <ul>
      ${itemEls}
    </ul>
  `
  style(el, {
    position: 'fixed',
    display: 'block',
    margin: 0,
    padding: 0,
    boxShadow: '0 0 15px rgba(0, 0, 0, .5)',
    overflow: 'hidden',
  })

  function handleItemEnter(item, itemEl) {
    updateHighlighted(item, itemEl)
  }

  function handleItemLeave() {
    updateHighlighted(null, null)
  }

  async function updateHighlighted(item, itemEl) {
    if (itemEl === highlightedItemEl) {
      return
    }

    if (!item && childMenuEl) {
      return
    }

    if (highlightedItemEl) {
      style(highlightedItemEl, {
        backgroundColor: itemBackgroundColor,
      })
    }

    highlightedItemEl = itemEl

    if (itemEl) {
      style(itemEl, {
        backgroundColor: itemHighlightColor,
      })

      if (childMenuEl) {
        childMenuEl.parentNode.removeChild(childMenuEl)
        childMenuEl = null
      }

      const itemBox = itemEl.getBoundingClientRect()
      if (item.subMenuId) {
        onSelect(item.menuId, item.idx)
        childMenuEl = await showMenu({
          id: item.subMenuId,
          itemGen,
          onSelect,
          parentEl: el,
          parentBox: itemBox,
          attach,
        })
      }
    }
  }

  return el
}

async function showMenu(props) {
  const {itemGen, onSelect, id, parentEl, parentBox, attach} = props
  const items = await itemGen(id)

  // render for size measurement
  let el = menu({
    items,
    itemGen,
    attach
  })
  style(el, {
    left: -9999,
    top: 0,
  })
  parentEl.appendChild(el)
  const menuBox = el.getBoundingClientRect()
  const menuWidth = Math.floor(menuBox.width)
  const menuHeight = Math.floor(menuBox.height)
  parentEl.removeChild(el)

  // measure position and flip attach direction if necessary
  const {innerHeight, innerWidth} = window
  const childAttach = {...attach}

  const pos = {}
  const leftUnderHang = parentBox.left - menuWidth
  const rightOverHang = parentBox.right + menuWidth - innerWidth
  // if there's x under/overhang swap to side with most space
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

  // render
  el = menu({
    items,
    itemGen,
    onSelect,
    attach: childAttach,
  })
  style(el, pos)
  parentEl.appendChild(el)

  return el
}

// XXX work around https://github.com/babel/babylon/issues/257
export default showMenu
