'use strict';

var $$Array = require("bs-platform/lib/js/array.js");
var Random = require("bs-platform/lib/js/random.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");

var Element = /* module */[];

var Context = /* module */[];

var Canvas = /* module */[];

var Document = /* module */[];

var Window = /* module */[];

var foregroundColor = "#F36";

var backgroundColor = "#000";

function clear(canvas, context) {
  context.fillStyle = backgroundColor;
  context.fillRect(0, 0, canvas.width, canvas.height);
  return /* () */0;
}

function draw(grid, context) {
  context.fillStyle = foregroundColor;
  return $$Array.iteri((function (x, cells) {
                return $$Array.iteri((function (y, cell) {
                              if (cell) {
                                context.fillRect((x << 1), (y << 1), 2, 2);
                                return /* () */0;
                              } else {
                                return /* () */0;
                              }
                            }), cells);
              }), grid);
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

function run(canvas, grid) {
  var context = canvas.getContext("2d");
  clear(canvas, context);
  draw(grid, context);
  requestAnimationFrame((function () {
          return run(canvas, next(grid));
        }));
  return /* () */0;
}

var match = document.getElementById("canvas");

var canvas = match !== null ? match : Pervasives.failwith("Cannot find the canvas");

console.log("got the canvas");

var rows$1 = canvas.width / 2 | 0;

var columns$1 = canvas.height / 2 | 0;

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

var main = run(canvas, grid);

var size = 2;

exports.Element = Element;
exports.Context = Context;
exports.Canvas = Canvas;
exports.Document = Document;
exports.Window = Window;
exports.size = size;
exports.foregroundColor = foregroundColor;
exports.backgroundColor = backgroundColor;
exports.clear = clear;
exports.draw = draw;
exports.rows = rows;
exports.columns = columns;
exports.index = index;
exports.neighbours = neighbours;
exports.next = next;
exports.run = run;
exports.main = main;
/* match Not a pure module */
