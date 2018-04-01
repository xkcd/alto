export function editMode() {

  var canvas = document.createElement('canvas');
  canvas.style.position = "fixed";
  canvas.style.left = "0";
  canvas.style.top = "0";
  canvas.style.width = "100%";
  canvas.style.height = "100%";
  document.body.appendChild(canvas);

  var context = canvas.getContext('2d');
  context.strokeStyle = "black";
  context.lineJoin = "round";
  context.lineWidth = 3;

  function resumePaint(e) {
    context.beginPath();
    context.moveTo(e.clientX, e.clientY);
  }

  function paint(e) {
    context.lineTo(e.clientX, e.clientY);
    context.stroke();
  }

  function startPaint(e) {
    if (e.which === 1) { // left button
      document.body.addEventListener('mouseover', resumePaint);
      document.body.addEventListener('mousemove', paint);
      document.body.addEventListener('mouseup', endPaint);
    }
  }

  function endPaint(e) {
    document.body.removeEventListener('mouseover', resumePaint);
    document.body.removeEventListener('mousemove', paint);
    document.body.removeEventListener('mouseup', endPaint);
  }

  document.body.addEventListener('mousedown', startPaint);

  window.addEventListener('keyup', function escapeOut(e) {
    if (e.which === 27 && window.confirm('Aw, that looks nice though. Really delete?')) {
      document.body.style.cursor = "auto";
      document.body.style.backgroundImage = "";
      document.body.removeChild(canvas);
      window.removeEventListener('keyup', escapeOut);
    }
  })
}