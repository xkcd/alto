function setupEditMode() {

  var comic  = document.getElementById('comic');
  var konvaStage = document.createElement('div');
  comic.style.position = "relative";
  konvaStage.style.position = "absolute";
  konvaStage.style.left = "0";
  konvaStage.style.top = "0";
  konvaStage.style.width = "100%";
  konvaStage.style.height = "100%";
  comic.appendChild(konvaStage);

  var stage = new Konva.Stage({
    container: konvaStage,
    width: comic.clientWidth,
    height: comic.clientHeight
  });

  var layer = new Konva.Layer();
  stage.add(layer);


  // then we are going to draw into special canvas element
  var canvas = document.createElement('canvas');
  canvas.width = stage.width();
  canvas.height = stage.height();

  // creted canvas we can add to layer as "Konva.Image" element
  var image = new Konva.Image({
      image: canvas,
      x : 0,
      y : 0,
      stroke: 'white',
      shadowEnabled: false
  });
  layer.add(image);
  stage.draw();

  // Good. Now we need to get access to context element
  var context = canvas.getContext('2d');
  context.strokeStyle = "#black";
  context.lineJoin = "round";
  context.lineWidth = 3;


  var isPaint = false;
  var lastPointerPosition;

  // now we need to bind some events
  // we need to start drawing on mousedown
  // and stop drawing on mouseup
  stage.on('contentMousedown.proto', function() {
    isPaint = true;
    lastPointerPosition = stage.getPointerPosition();

  });

  stage.on('contentMouseup.proto', function() {
      isPaint = false;
  });

  // and core function - drawing
  stage.on('contentMousemove.proto', function() {

    if (!isPaint) {
      return;
    }

    context.globalCompositeOperation = 'source-over';
    context.beginPath();

    var localPos = {
      x: lastPointerPosition.x - image.x(),
      y: lastPointerPosition.y - image.y()
    };
    context.moveTo(localPos.x, localPos.y);
    var pos = stage.getPointerPosition();
    localPos = {
      x: pos.x - image.x(),
      y: pos.y - image.y()
    };
    context.lineTo(localPos.x, localPos.y);
    context.closePath();
    context.stroke();


    lastPointerPosition = pos;
    layer.draw();
  });


  window.addEventListener('keyup', function escapeOut(e) {
    if (e.which === 27 && window.confirm('Aw, that looks nice though. Really delete?')) {
      document.body.style.cursor = "auto";
      document.body.style.backgroundImage = "";
      stage.destroy();
      window.removeEventListener('keyup', escapeOut);
    }
  })
}

export function editMode() {
  var poll = setInterval(function() {
    if (window.Konva) {
      document.body.style.cursor = "url(http://colonpipe.org/brush.cur), crosshair";
      document.body.style.backgroundImage = "url(http://colonpipe.org/transparency.png";
      clearInterval(poll);
      setupEditMode();
    }
  }, 1000);
  var konvaImport = document.createElement('script');
  konvaImport.setAttribute('src', 'https://cdn.rawgit.com/konvajs/konva/1.3.0/konva.js');
  document.head.appendChild(konvaImport);
}