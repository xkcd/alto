import html from 'nanohtml'
import style from 'dom-css'

function menuItem(props) {
  const {item, itemGen, attach} = props
  let childMenu
  const backgroundColor = '#e4e4e4'
  const highlightColor = '#f4f4f4'
  const el = html`
    <li
      onclick=${handleMouseClick}
      onmouseenter=${handleMouseEnter}
      onmouseleave=${handleMouseLeave}
    >${item.label}</li>
  `
  style(el, {
    position: 'relative',
    display: 'flex',
    alignItems: 'center',
    cursor: 'default',
    padding: '8px 10px',
    backgroundColor,
    userSelect: 'none',
  })

  if (!!item.subMenuId) {
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

  async function handleMouseEnter() {
    style(el, {
      backgroundColor: highlightColor,
      zIndex: 100,
    })
    const itemBox = el.getBoundingClientRect()
    if (item.subMenuId) {
      childMenu = await showMenu({
        id: item.subMenuId,
        itemGen,
        parentEl: el,
        parentBox: itemBox,
        attach,
      })
    }
  }

  function handleMouseLeave() {
    style(el, {
      backgroundColor,
      zIndex: null,
    })
    if (childMenu) {
      el.removeChild(childMenu)
      childMenu = null
    }
  }

  function handleMouseClick() {
    if (item.onSelect) {
      item.onSelect()
    }
  }

  return el
}

function menu(props) {
  const {items, itemGen, attach} = props
  const el = html`
    <ul>
      ${items.map(item => menuItem({
        item,
        itemGen,
        attach,
      }))}
    </ul>
  `
  style(el, {
    position: 'fixed',
    display: 'block',
    margin: 0,
    padding: 0,
    boxShadow: '0 0 15px rgba(0, 0, 0, .5)',
  })
  return el
}

async function showMenu(props) {
  const {itemGen, id, parentEl, parentBox, attach} = props
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
  let pos
  const childAttach = {...attach}

  if (attach.x === 'left') {
    pos = {left: parentBox.left - menuWidth}
    if (pos.left < 0) {
      pos = {left: parentBox.right}
      childAttach.x = 'right'
    }
  } else if (attach.x === 'right') {
    pos = {left: parentBox.right}
    const overHang = parentBox.right + menuWidth - innerWidth
    if (overHang > 0) {
      pos = {left: parentBox.left - menuWidth}
      childAttach.x = 'left'
    }
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
      pos.top -= overHang
      childAttach.y = 'bottom'
    }
  }

  // render
  el = menu({
    items,
    itemGen,
    attach: childAttach,
  })
  style(el, pos)
  parentEl.appendChild(el)

  return el
}

// XXX work around https://github.com/babel/babylon/issues/257
export default showMenu
