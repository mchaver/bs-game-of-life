'use strict';

var $$Array = require("bs-platform/lib/js/array.js");
var Random = require("bs-platform/lib/js/random.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Js_primitive = require("bs-platform/lib/js/js_primitive.js");

var Element = /* module */[];

var Context = /* module */[];

var Window = /* module */[];

var Mouse = /* module */[];

var MouseEvent = /* module */[];

var Canvas = /* module */[];

var CanvasEvent = /* module */[];

var Option = /* module */[];

var Select = /* module */[];

var SelectEvent = /* module */[];

function isNone(o) {
  if (o) {
    return false;
  } else {
    return true;
  }
}

function isSome(o) {
  if (o) {
    return true;
  } else {
    return false;
  }
}

function relMouseCoords(c, e) {
  var totalOffsetX = c.offsetLeft - c.scrollLeft | 0;
  var totalOffsetY = c.offsetTop - c.scrollTop | 0;
  var currentElement = Js_primitive.null_to_opt(c.offsetParent);
  while(isSome(currentElement)) {
    var match = currentElement;
    if (match) {
      var ec = match[0];
      totalOffsetX = (totalOffsetX + ec.offsetLeft | 0) - ec.scrollLeft | 0;
      totalOffsetY = (totalOffsetY + ec.offsetTop | 0) - ec.scrollTop | 0;
      currentElement = Js_primitive.null_to_opt(ec.offsetParent);
    }
    
  };
  var canvasX = e.pageX - totalOffsetX | 0;
  var canvasY = e.pageY - totalOffsetY | 0;
  return /* tuple */[
          canvasX,
          canvasY
        ];
}

function parse_speed(v) {
  switch (v) {
    case "fast" : 
        return /* Some */[/* Fast */3];
    case "normal" : 
        return /* Some */[/* Normal */2];
    case "slow" : 
        return /* Some */[/* Slow */1];
    case "very-fast" : 
        return /* Some */[/* VeryFast */4];
    case "very-slow" : 
        return /* Some */[/* VerySlow */0];
    default:
      return /* None */0;
  }
}

function speed_to_int(s) {
  switch (s) {
    case 0 : 
        return 1000;
    case 1 : 
        return 500;
    case 2 : 
        return 100;
    case 3 : 
        return 50;
    case 4 : 
        return 0;
    
  }
}

var foregroundColor = "#F36";

var backgroundColor = "#FFFFFF";

var gridColor = "#C0C0C0";

var state = [/* record */[
    /* run */true,
    /* grid : array */[],
    /* rows */0,
    /* columns */0,
    /* speed : Normal */2
  ]];

function clear(canvas, context) {
  context.fillStyle = backgroundColor;
  context.fillRect(0, 0, canvas.width, canvas.height);
  return /* () */0;
}

function drawGrid(canvas, rows, columns) {
  var context = canvas.getContext("2d");
  clear(canvas, context);
  context.lineWidth = 2;
  context.strokeStyle = gridColor;
  context.beginPath();
  var maxHeight = Caml_int32.imul(rows, 20);
  var maxWidth = Caml_int32.imul(columns, 20);
  var width = 2;
  while(width < maxWidth) {
    context.moveTo(width, 0);
    context.lineTo(width, maxHeight + 2 | 0);
    width = (width + 2 | 0) + 18 | 0;
  };
  context.moveTo(width, 0);
  context.lineTo(width, maxHeight + 2 | 0);
  var height = 1;
  while(height < maxHeight) {
    context.moveTo(2, height);
    context.lineTo(maxWidth + 2 | 0, height);
    height = (height + 2 | 0) + 18 | 0;
  };
  context.moveTo(2, height);
  context.lineTo(maxWidth + 2 | 0, height);
  context.stroke();
  context.fillStyle = foregroundColor;
  return /* () */0;
}

function calcXOffset(x) {
  return 3 + Caml_int32.imul(x, 20) | 0;
}

function calcYOffset(y) {
  return 2 + Caml_int32.imul(y, 20) | 0;
}

function draw(grid, context) {
  context.fillStyle = foregroundColor;
  return $$Array.iteri((function (x, cells) {
                return $$Array.iteri((function (y, cell) {
                              if (cell) {
                                context.fillRect(calcXOffset(x), calcYOffset(y), 18, 18);
                                return /* () */0;
                              } else {
                                return /* () */0;
                              }
                            }), cells);
              }), grid);
}

function calcIndex(x, y, rows, columns) {
  var o_xIndex = /* None */0;
  var xCount = 0;
  var maxWidth = Caml_int32.imul(columns, 20);
  var width = 2;
  while(width < maxWidth && isNone(o_xIndex)) {
    if (x >= width && x <= ((width + 2 | 0) + 18 | 0)) {
      o_xIndex = /* Some */[xCount];
    } else {
      xCount = xCount + 1 | 0;
    }
    width = (width + 2 | 0) + 18 | 0;
  };
  var match = o_xIndex;
  if (match) {
    var o_yIndex = /* None */0;
    var yCount = 0;
    var maxHeight = Caml_int32.imul(rows, 20);
    var height = 1;
    while(height < maxHeight && isNone(o_yIndex)) {
      if (y >= height && y <= ((height + 2 | 0) + 18 | 0)) {
        o_yIndex = /* Some */[yCount];
      } else {
        yCount = yCount + 1 | 0;
      }
      height = (height + 2 | 0) + 18 | 0;
    };
    var match$1 = o_yIndex;
    if (match$1) {
      return /* Some */[/* tuple */[
                match[0],
                match$1[0]
              ]];
    } else {
      return /* None */0;
    }
  } else {
    return /* None */0;
  }
}

function rows(grid) {
  return grid.length;
}

function columns(grid) {
  var length = grid.length;
  if (0 < length) {
    return Caml_array.caml_array_get(grid, 0).length;
  } else {
    return -1;
  }
}

function index(min, max, n) {
  if (Caml_obj.caml_lessthan(n, min)) {
    return max;
  } else if (Caml_obj.caml_lessthan(max, n)) {
    return min;
  } else {
    return n;
  }
}

function neighbours(x, y, grid) {
  return $$Array.map((function (param) {
                var m = index(0, grid.length - 1 | 0, x + param[0] | 0);
                var n = index(0, columns(grid) - 1 | 0, y + param[1] | 0);
                return Caml_array.caml_array_get(Caml_array.caml_array_get(grid, m), n);
              }), /* array */[
              /* tuple */[
                -1,
                -1
              ],
              /* tuple */[
                -1,
                0
              ],
              /* tuple */[
                -1,
                1
              ],
              /* tuple */[
                0,
                -1
              ],
              /* tuple */[
                0,
                1
              ],
              /* tuple */[
                1,
                -1
              ],
              /* tuple */[
                1,
                0
              ],
              /* tuple */[
                1,
                1
              ]
            ]);
}

function next(grid) {
  return $$Array.mapi((function (x, cells) {
                return $$Array.mapi((function (y, cell) {
                              var match = neighbours(x, y, grid).filter((function (param) {
                                      return Caml_obj.caml_equal(/* Alive */1, param);
                                    })).length;
                              if (match !== 2) {
                                if (match !== 3) {
                                  return /* Dead */0;
                                } else {
                                  return /* Alive */1;
                                }
                              } else {
                                return cell;
                              }
                            }), cells);
              }), grid);
}

function $$void() {
  return /* () */0;
}

function run(canvas) {
  var context = canvas.getContext("2d");
  drawGrid(canvas, state[0][/* rows */2], state[0][/* columns */3]);
  draw(state[0][/* grid */1], context);
  setTimeout((function () {
          if (state[0][/* run */0]) {
            var init = state[0];
            state[0] = /* record */[
              /* run */init[/* run */0],
              /* grid */next(state[0][/* grid */1]),
              /* rows */init[/* rows */2],
              /* columns */init[/* columns */3],
              /* speed */init[/* speed */4]
            ];
            requestAnimationFrame((function () {
                    return run(canvas);
                  }));
            return /* () */0;
          } else {
            return /* () */0;
          }
        }), speed_to_int(state[0][/* speed */4]));
  return /* () */0;
}

function resetGrid(rows, columns) {
  Random.self_init(/* () */0);
  var grid = $$Array.map((function (cells) {
          return $$Array.map((function () {
                        if (Random.bool(/* () */0)) {
                          return /* Alive */1;
                        } else {
                          return /* Dead */0;
                        }
                      }), cells);
        }), $$Array.make_matrix(rows, columns, /* () */0));
  var init = state[0];
  state[0] = /* record */[
    /* run */init[/* run */0],
    /* grid */grid,
    /* rows */init[/* rows */2],
    /* columns */init[/* columns */3],
    /* speed */init[/* speed */4]
  ];
  return /* () */0;
}

var match = document.getElementById("canvas");

var canvas = match !== null ? match : Pervasives.failwith("Cannot find the canvas");

var match$1 = document.getElementById("play-button");

var play_button = match$1 !== null ? match$1 : Pervasives.failwith("Cannot find the play-button");

var match$2 = document.getElementById("random-reset-button");

var random_reset_button = match$2 !== null ? match$2 : Pervasives.failwith("Cannot find the random-reset-button");

var match$3 = document.getElementById("speed-select");

var speed_select = match$3 !== null ? match$3 : Pervasives.failwith("Cannot find the speed-select");

canvas.addEventListener("click", (function (e) {
        var e$1 = e;
        console.log("handleClickGrid");
        var match = relMouseCoords(canvas, e$1);
        var y = match[1];
        var x = match[0];
        console.log(x);
        console.log(y);
        var o = calcIndex(x, y, state[0][/* rows */2], state[0][/* columns */3]);
        if (o) {
          var match$1 = o[0];
          var y$1 = match$1[1];
          var x$1 = match$1[0];
          var g = state[0][/* grid */1];
          var c = Caml_array.caml_array_get(Caml_array.caml_array_get(g, x$1), y$1);
          var newC = c ? /* Dead */0 : /* Alive */1;
          Caml_array.caml_array_set(Caml_array.caml_array_get(g, x$1), y$1, newC);
          var init = state[0];
          state[0] = /* record */[
            /* run */init[/* run */0],
            /* grid */g,
            /* rows */init[/* rows */2],
            /* columns */init[/* columns */3],
            /* speed */init[/* speed */4]
          ];
          var context = canvas.getContext("2d");
          drawGrid(canvas, state[0][/* rows */2], state[0][/* columns */3]);
          return draw(state[0][/* grid */1], context);
        } else {
          return /* () */0;
        }
      }));

play_button.addEventListener("click", (function () {
        var init = state[0];
        state[0] = /* record */[
          /* run */!state[0][/* run */0],
          /* grid */init[/* grid */1],
          /* rows */init[/* rows */2],
          /* columns */init[/* columns */3],
          /* speed */init[/* speed */4]
        ];
        if (state[0][/* run */0]) {
          play_button.innerHTML = "Pause";
          return run(canvas);
        } else {
          play_button.innerHTML = "Play";
          return /* () */0;
        }
      }));

function handleSpeedSelectChange() {
  var selectedIndex = speed_select.selectedIndex;
  var option = Caml_array.caml_array_get(speed_select.options, selectedIndex);
  var match = parse_speed(option.value);
  if (match) {
    var init = state[0];
    state[0] = /* record */[
      /* run */init[/* run */0],
      /* grid */init[/* grid */1],
      /* rows */init[/* rows */2],
      /* columns */init[/* columns */3],
      /* speed */match[0]
    ];
    return /* () */0;
  } else {
    return /* () */0;
  }
}

speed_select.addEventListener("change", handleSpeedSelectChange);

random_reset_button.addEventListener("click", (function () {
        resetGrid(state[0][/* rows */2], state[0][/* columns */3]);
        var init = state[0];
        state[0] = /* record */[
          /* run */false,
          /* grid */init[/* grid */1],
          /* rows */init[/* rows */2],
          /* columns */init[/* columns */3],
          /* speed */init[/* speed */4]
        ];
        return run(canvas);
      }));

var columns$1 = (canvas.width - 2 | 0) / 20 | 0;

var rows$1 = (canvas.height - 2 | 0) / 20 | 0;

Random.self_init(/* () */0);

var grid = $$Array.map((function (cells) {
        return $$Array.map((function () {
                      if (Random.bool(/* () */0)) {
                        return /* Alive */1;
                      } else {
                        return /* Dead */0;
                      }
                    }), cells);
      }), $$Array.make_matrix(rows$1, columns$1, /* () */0));

var init = state[0];

state[0] = /* record */[
  /* run */init[/* run */0],
  /* grid */grid,
  /* rows */rows$1,
  /* columns */columns$1,
  /* speed */init[/* speed */4]
];

var main = run(canvas);

var size = 8;

var pieceColor = "#FFFFFF";

var strokeWidth = 2;

var cellSize = 18;

exports.Element = Element;
exports.Context = Context;
exports.Window = Window;
exports.Mouse = Mouse;
exports.MouseEvent = MouseEvent;
exports.Canvas = Canvas;
exports.CanvasEvent = CanvasEvent;
exports.Option = Option;
exports.Select = Select;
exports.SelectEvent = SelectEvent;
exports.isNone = isNone;
exports.isSome = isSome;
exports.relMouseCoords = relMouseCoords;
exports.parse_speed = parse_speed;
exports.speed_to_int = speed_to_int;
exports.size = size;
exports.foregroundColor = foregroundColor;
exports.backgroundColor = backgroundColor;
exports.pieceColor = pieceColor;
exports.gridColor = gridColor;
exports.strokeWidth = strokeWidth;
exports.cellSize = cellSize;
exports.state = state;
exports.clear = clear;
exports.drawGrid = drawGrid;
exports.calcXOffset = calcXOffset;
exports.calcYOffset = calcYOffset;
exports.draw = draw;
exports.calcIndex = calcIndex;
exports.rows = rows;
exports.columns = columns;
exports.index = index;
exports.neighbours = neighbours;
exports.next = next;
exports.$$void = $$void;
exports.run = run;
exports.resetGrid = resetGrid;
exports.main = main;
/* match Not a pure module */
