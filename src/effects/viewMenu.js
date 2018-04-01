/* sure, minimize the mouse cursor, that's what we meant */
/* TODO ask folks how to upload a static asset */
export function viewMinimize() {
  document.body.style.cursor = "url(//xkcd.com/1975/Tiny.cur), default";
}

/* full screen it is! */
export function fullScreen() {
  const comic  = document.getElementById('comic');
  const methodName = [
    "requestFullscreen",
    "webkitRequestFullscreen",
    "mozRequestFullScreen",
    "msRequestFullscreen"
  ].find(fname => typeof comic[fname] === "function");
  if (methodName) {
    comic[methodName]();
  }
}