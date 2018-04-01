import html from 'nanohtml'

import arrow from './arrow'
import spinner from './spinner'

const styles = css`
  @import "./colors.less";

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
    -webkit-tap-highlight-color: transparent;

    &.highlight {
      background-color: @itemHighlightColor;
      color: white;
    }

    &.disabled {
      opacity: .5;
    }

    & > span {
      flex: 1;
    }

    &.disabled > span {
      text-shadow: 0 1px white;
    }
  }

  &.left {
    & > span {
      margin-left: 10px;
    }

    & > .spacer {
      justify-content: flex-start;
    }
  }

  &.right  {
    & > .spacer {
      justify-content: flex-end;
    }

    & > span {
      margin-right: 10px;
    }
  }

  .spacer {
    display: flex;
    width: 15px;
  }
`

export default function menuItem(props) {
  const {item, showArrows, isLoading, isHighlighted, onItemSelect, onMouseEnter, onMouseLeave, attach} = props

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
      <div class="alto-spacer ${styles.spacer}">
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
    styles.item,
    showArrows && styles[attach.x],
    isHighlighted && styles.highlight,
    item.disabled && styles.disabled,
  ].filter(x => x)

  const el = html`
    <li
      class="alto-menuitem ${classes.join(' ')}"
      onclick=${item.disabled || item.subMenuId ? null : ev => onItemSelect(item, ev.target)}
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
