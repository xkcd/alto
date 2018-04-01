/* themes */
export function darkTheme () {
  // TODO
}

/* make screen black */
export function darkness() {
  document.body.style.background = "#000";
  document.body.style.color = "#000";
  var s= document.getElementById("comic").childNodes[0].style;
  s.opacity = 0;
  s.MozOpacity = 0;
  s.KhtmlOpacity = 0;
  s.filter = 'alpha(opacity=0)';
  // TODO? this doesn't do anything to the menu
  // TODO setInterval to change it back
}

/* grayscale */
export function darkVision() {
  document.body.style.background = "darkgray";
  document.body.style.color = "lightgray";
  var s= document.getElementById("comic").childNodes[0].style;
  s.opacity = .25;
  s.MozOpacity = .25;
  s.KhtmlOpacity = .25;
  s.filter = 'alpha(opacity=.25)';
  // TODO grayscale the menu as well
}

/* increase contrast */
export function daylight() {
	// TODO
  /* -webkit-filter: contrast(200%); // on the img */
}

/* change the font to Symbol */
export function symbol() {
  document.body.style.fontFamily = "wingdings"
  // TODO are there other things we can use here
  // TODO change the menu font
}

/* fill the screen with an acid pattern */
export function acidSplash() {
  var s= document.getElementById("comic").childNodes[0].style;
  s.opacity = 0.5;
  s.MozOpacity = 0.5;
  s.KhtmlOpacity = 0.5;
  s.filter = 'alpha(opacity=0.5)';

  var colors = [ "red", "yellow", "chartreuse", "blue", "fuschia", "blueviolet" ];
  var color = colors[Math.floor(Math.random()*colors.length)];
  document.body.style.background = color;
  setInterval(function() {
    var color = colors[Math.floor(Math.random()*colors.length)];
    document.body.style.background = color;
  }, 300);
}

/* add dancing brooms */
export function animateObjects() {
  // TODO
}

/* make the screen fuzzy and hard to read */
export function blur() {
  var s= document.getElementById("comic").childNodes[0].style;
  s.webkitFilter = "blur(10px)";
  document.body.style.textShadow = "0 0 3px #000, 3px 0 3px #000, 0 3px 3px #000, -3px 0 3px #000, 0 -3px 3px #000";
  // TODO menu blur
}

/* mirror everything */
export function mirrorImage() {
  document.body.style.webkitTransform = "scale(-1,1)";
  document.body.style.mozTransform = "scale(-1,1)";
  document.body.style.oTransform = "scale(-1,1)";
  document.body.style.msTransform = "scale(-1,1)";
  document.body.style.transform = "scale(-1,1)";
}

/* freeze for 6s */
export function timeStop() {
  var now = new Date().getTime();
  while(new Date().getTime() < now + 6000 ){ /* do nothing */ }
}
