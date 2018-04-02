import { fullScreen } from './viewMenu';

export function shutDown() {
  const hasUpdates = Math.random() > 0.1;
  const shutdownScreen = hasUpdates ? 'pdnpoouym.jpg' : 'e05.jpg';
  const img = new Image(window.innerWidth, window.innerHeight);
  img.src = '//xkcd.com/1975/' + shutdownScreen;
  document.body.innerHTML = '';
  document.body.appendChild(img);
}