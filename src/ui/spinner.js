import html from 'nanohtml'

export const styles = css`
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

export default function spinner(props) {
  const {size = 17, strokeWidth = 2} = props
  const r = size / 2 - strokeWidth

  const el = html`
    <svg class="${styles.spinner}" viewBox="0 0 ${size} ${size}" width="${size}" height="${size}">
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
