import html from 'nanohtml'
import style from 'dom-css'

const styles = css`
  .arrow {
    width: 0;
    height: 0;
    opacity: .65;
    border: 5px solid transparent;

    &.up {
      border-bottom: 5px solid black;
    }

    &.down {
      border-top: 5px solid black;
    }

    &.left {
      border-left: 5px solid black;
    }

    &.right {
      border-right: 5px solid black;
    }
  }
`

export default function arrow(props) {
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

  const el = html`<div class="alto-arrow ${styles.arrow}" />`
  style(el, directionStyles[direction])

  return el
}

