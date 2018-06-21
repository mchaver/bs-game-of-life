(* https://gist.github.com/woxtu/610b88dd09a3856bcb77899314e16039 *)
(* https://qiita.com/woxtu/items/4e073e8e8dbf8596388e *)
module Element = struct
  type t
end

module Context = struct
  type t
  external setFillStyle : t -> string -> unit = "fillStyle" [@@bs.set]
  external setStrokeStyle : t -> string -> unit = "strokeStyle" [@@bs.set]
  external setLineWidth : t -> float -> unit = "lineWidth" [@@bs.set]

  external fillRect : int -> int -> int -> int -> unit = "" [@@bs.send.pipe: t]
  external moveTo : float -> float -> unit = "" [@@bs.send.pipe: t]
  external lineTo : float -> float -> unit = "" [@@bs.send.pipe: t]
  external beginPath : unit = "" [@@bs.send.pipe: t]
  external closePath : unit = "" [@@bs.send.pipe: t]
  external stroke : unit = "" [@@bs.send.pipe: t]                       
    (* external fillRect : int -> int -> int -> int -> unit = "" [@@bs.send] *)
end

module Canvas = struct
  type t = Element.t
  external width : t -> int = "" [@@bs.get]
  external height : t -> int = "" [@@bs.get]
(*  external getContext : string -> Context.t = "" [@@bs.send] *)
  external getContext : string -> Context.t = "" [@@bs.send.pipe: t]
end

module Document = struct
(* external getElementById : string -> Dom.element option = "document.getElementById" [@@bs.val] [@@bs.return nullable] *)
  external getElementById : string -> Element.t option = "document.getElementById" [@@bs.val] [@@bs.return null_to_opt]
end

module Window = struct
  external requestAnimationFrame: (unit -> unit) -> unit = "" [@@bs.val]
end

type cell =
  | Dead
  | Alive

let size = 8
let foregroundColor = "#F36"
let backgroundColor = "#000"
let pieceColor = "#FFFFFF"                    
let gridColor = "#C0C0C0"

let clear canvas context = (
  Context.setFillStyle context backgroundColor;
  context |> Context.fillRect 0 0 (canvas |> Canvas.width) (canvas |> Canvas.height);
)
  
let draw grid context = (
  Context.setFillStyle context foregroundColor;
  grid |> Array.iteri @@ fun x cells ->
  cells |> Array.iteri @@ fun y cell -> 
    match cell with 
    | Alive -> context |> Context.fillRect (x * size) (y * size) size size;
    | _ -> ();
)

let rows grid = Array.length grid

let columns grid =
  match (Array.length grid) with
  | length when 0 < length -> Array.length @@ Array.get grid 0
  | _ -> -1 

let index min max n =
  match n with
  | n when n < min -> max
  | n when max < n -> min
  | n -> n

let neighbours x y grid =
  [|(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)|]
  |> Array.map @@ fun (a, b) -> (
    let m = index 0 (rows grid - 1) (x + a) in
    let n = index 0 (columns grid - 1) (y + b) in
    Array.get (Array.get grid m) n
  )

let next grid =
  grid |> Array.mapi @@ fun x cells ->
    cells |> Array.mapi @@ fun y cell ->
      match (neighbours x y grid |> Js.Array.filter ((=) Alive) |> Array.length) with
      | 3 -> Alive
      | 2 -> cell
      | _ -> Dead

let rec run canvas grid =
  let context = canvas |> Canvas.getContext "2d" in
  context |> clear canvas;
  context |> draw grid;
  Window.requestAnimationFrame @@ fun () -> run canvas @@ next grid


let strokeWidth = 2
let cellDimension = 18

(* draw each line, move*)
                    
let drawGrid canvas =
  let context = canvas |> Canvas.getContext "2d" in
  (* context |> clear canvas; *)
  Context.setLineWidth context (float_of_int strokeWidth);
  Context.setStrokeStyle context gridColor;
  context |> Context.beginPath;
  let maxHeight = canvas |> Canvas.height in
  let maxWidth = canvas |> Canvas.width in
  let width = ref strokeWidth in
  while !width + strokeWidth + cellDimension < maxWidth do
    context |> Context.moveTo (float_of_int !width) (float_of_int 0);
    context |> Context.lineTo (float_of_int !width) (float_of_int maxHeight);
    width := !width + strokeWidth + cellDimension;
  done;
  let height = ref (strokeWidth - 1) in
  while !height + strokeWidth + cellDimension < maxHeight do
    context |> Context.moveTo (float_of_int strokeWidth) (float_of_int !height);
    context |> Context.lineTo (float_of_int maxWidth) (float_of_int !height);
    height := !height + strokeWidth + cellDimension;
  done;
  context |> Context.stroke;
  Context.setFillStyle context foregroundColor;
  context |> Context.fillRect ((strokeWidth + 1) * 1 + (cellDimension * 0)) (strokeWidth * 1 + (cellDimension * 0)) (cellDimension * 1) (cellDimension * 1);
  context |> Context.fillRect ((strokeWidth * 2 + 1) * 1 + (cellDimension * 1)) (strokeWidth * 1 + (cellDimension * 0)) (cellDimension * 1) (cellDimension * 1)
  
                                          
let runonce canvas = 
  let context = canvas |> Canvas.getContext "2d" in
  (* context |> clear canvas; *)
  Context.setLineWidth context 2.0;
  Context.setStrokeStyle context gridColor;
  context |> Context.beginPath;
  context |> Context.moveTo 15.0 200.0;
  context |> Context.lineTo 0.0 200.0;
  (* Context.setStrokeStyle context "black"; *)
  context |> Context.stroke
                                          
let main =
  let canvas = 
    (match (Document.getElementById "canvas") with
    | None -> failwith "Cannot find the canvas"
    | Some canvas -> canvas) in
  
  Js.log("got the canvas");

  let rows = (canvas |> Canvas.width) / size in
  let columns = (canvas |> Canvas.height) / size in

  Random.self_init ();
  
  let grid =
    (Array.make_matrix rows columns () |> Array.map @@ fun cells ->
      cells |> Array.map @@ fun _ -> if (Random.bool ()) then Alive else Dead) in

  (* runonce canvas *)
  drawGrid canvas
  (* run canvas grid *)

      (*

//grid width and height
var bw = 400;
var bh = 400;
//padding around grid
var p = 10;
//size of canvas
var cw = bw + (p*2) + 1;
var ch = bh + (p*2) + 1;

var canvas = $('<canvas/>').attr({width: cw, height: ch}).appendTo('body');

var context = canvas.get(0).getContext("2d");

function drawBoard(){
    for (var x = 0; x <= bw; x += 40) {
        context.moveTo(0.5 + x + p, p);
        context.lineTo(0.5 + x + p, bh + p);
    }


    for (var x = 0; x <= bh; x += 40) {
        context.moveTo(p, 0.5 + x + p);
        context.lineTo(bw + p, 0.5 + x + p);
    }

    context.strokeStyle = "black";
    context.stroke();
}

drawBoard();
*)
    
(*
http://blog.klipse.tech/reason/2017/10/17/externals-js-ffi-reason.html

https://stackoverflow.com/questions/2481350/how-to-get-scrollbar-position-with-javascript
https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
https://en.wikipedia.org/wiki/Gun_(cellular_automaton)
https://codereview.stackexchange.com/questions/114702/drawing-a-grid-on-canvas

module StillLife = struct
  let block
  let beehive
  let loaf
  let boat
  let tub
end

module Oscillators = struct
  let blinker1
  let blinker2
  let toad1
  let toad2
  let beacon1
  let beacon2
  let pulsar1
  let pulsar2
  let pulsar3
  let pentadecathlon1
  let pentadecathlon2
  let pentadecathlon3
  let pentadecathlon4
  let pentadecathlon5
  let pentadecathlon6
  let pentadecathlon7
  let pentadecathlon8
  let pentadecathlon9
  let pentadecathlon10
  let pentadecathlon11
  let pentadecathlon12
  let pentadecathlon13
  let pentadecathlon14
  let pentadecathlon15
end

module Spaceships = struct
  let glider1
  let glider2
  let gilder3
  let glider4
  let lightweightSpaceship
end
*)

    
