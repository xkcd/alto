import html from 'nanohtml'
import morph from 'nanomorph'

import arrow from './arrow'

export default function hoverMenuButton(props) {
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

