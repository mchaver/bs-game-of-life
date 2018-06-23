module Element = struct
  type t
  external setInnerHTML : t -> string -> unit = "innerHTML" [@@bs.set]
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
end

module Canvas = struct
  type t = Element.t
  external width : t -> int = "" [@@bs.get]
  external height : t -> int = "" [@@bs.get]
  external getContext : string -> Context.t = "" [@@bs.send.pipe: t]
end

module Document = struct
  external getElementById : string -> Element.t option = "document.getElementById" [@@bs.val] [@@bs.return null_to_opt]
end

module Window = struct
  external requestAnimationFrame: (unit -> unit) -> unit = "" [@@bs.val]
end

module Dom = struct
  (* external addEventListener: (string * Element.t -> unit) -> unit = "" *)
  external addEventListener : string -> (unit -> unit) -> unit = "" [@@bs.send.pipe: Element.t]

  (* external addClickEventListener : ((_ [@bs.as "click"]) * t -> unit) -> unit = "addEventListener" [@@bs.send.pipe : t]  *)
end
           
type cell =
  | Dead
  | Alive

let size = 8
let foregroundColor = "#F36"
let backgroundColor = "#FFFFFF"
let pieceColor = "#FFFFFF"                    
let gridColor = "#C0C0C0"
let strokeWidth = 2
let cellSize = 18

type state =
  { run     : bool
  ; grid    : (cell array) array
  ; rows    : int
  ; columns : int
  }

let state: state ref =
  ref
    { run = true
    ; grid = [||]
    ; rows = 0
    ; columns = 0
    }
                     
let clear canvas context = (
  Context.setFillStyle context backgroundColor;
  context |> Context.fillRect 0 0 (canvas |> Canvas.width) (canvas |> Canvas.height);
)

let drawGrid canvas rows columns =
  let context = canvas |> Canvas.getContext "2d" in
  context |> clear canvas;
  Context.setLineWidth context (float_of_int strokeWidth);
  Context.setStrokeStyle context gridColor;
  context |> Context.beginPath;
  let maxHeight = rows * (strokeWidth + cellSize) in
  let maxWidth = columns * (strokeWidth + cellSize) in
  let width = ref strokeWidth in
  while !width < maxWidth do
    context |> Context.moveTo (float_of_int !width) (float_of_int 0);
    context |> Context.lineTo (float_of_int !width) (float_of_int (maxHeight + strokeWidth));
    width := !width + strokeWidth + cellSize;
  done;
  context |> Context.moveTo (float_of_int !width) (float_of_int 0);
  context |> Context.lineTo (float_of_int !width) (float_of_int (maxHeight + strokeWidth));
  let height = ref (strokeWidth - 1) in
  while !height < maxHeight do
    context |> Context.moveTo (float_of_int strokeWidth) (float_of_int !height);
    context |> Context.lineTo (float_of_int (maxWidth + strokeWidth)) (float_of_int !height);
    height := !height + strokeWidth + cellSize;
  done;
  context |> Context.moveTo (float_of_int strokeWidth) (float_of_int !height);
  context |> Context.lineTo (float_of_int (maxWidth + strokeWidth)) (float_of_int !height);
  context |> Context.stroke;
  Context.setFillStyle context foregroundColor

let calcXOffset x =
  (strokeWidth + 1) + (x * (strokeWidth + cellSize))

let calcYOffset y =
  ((strokeWidth / 2) + 1) + (y * (strokeWidth + cellSize))
  
let draw grid context = (
  Context.setFillStyle context foregroundColor;
  grid |> Array.iteri @@ fun x cells ->
  cells |> Array.iteri @@ fun y cell -> 
    match cell with 
    | Alive -> context |> Context.fillRect (calcXOffset x) (calcYOffset y) cellSize cellSize;
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

let rec run canvas =
  let context = canvas |> Canvas.getContext "2d" in
  drawGrid canvas !state.rows !state.columns;
  context |> draw !state.grid;
  state := {!state with grid = next !state.grid};
  if (!state.run)
  then Window.requestAnimationFrame @@ fun () -> run canvas
  else ()

let resetGrid rows columns =
  Random.self_init ();
  
  let grid =
    (Array.make_matrix rows columns () |> Array.map @@ fun cells ->
      cells |> Array.map @@ fun _ -> if (Random.bool ()) then Alive else Dead) in

  state := { !state with grid = grid }
  
(* draw each line, move*)

let main =
  let canvas = 
    match (Document.getElementById "canvas") with
    | None -> failwith "Cannot find the canvas"
    | Some canvas -> canvas in

  let play_button =
    match (Document.getElementById "play-button") with
    | None -> failwith "Cannot find the play-button"
    | Some play_button -> play_button in

  let random_reset_button =
    match (Document.getElementById "random-reset-button") with
    | None -> failwith "Cannot find the random-reset-button"
    | Some random_reset_button -> random_reset_button in
  

  let toggleRun _unit =
    state := { !state with run = not !state.run };
    if !state.run
    then
      begin
        Element.setInnerHTML play_button "Pause";
        run canvas
      end
    else Element.setInnerHTML play_button "Play" in

  play_button |> Dom.addEventListener "click" (fun _unit -> toggleRun ());

  let randomGrid _unit =
    resetGrid !state.rows !state.columns;
    state := { !state with run = false };
    run canvas in
  
  random_reset_button |> Dom.addEventListener "click" (fun _unit -> randomGrid ());
  
  let columns = ((canvas |> Canvas.width) - strokeWidth) / (strokeWidth + cellSize) in
  let rows = ((canvas |> Canvas.height) - strokeWidth) / (strokeWidth + cellSize) in
  
  Random.self_init ();
  
  let grid =
    (Array.make_matrix rows columns () |> Array.map @@ fun cells ->
      cells |> Array.map @@ fun _ -> if (Random.bool ()) then Alive else Dead) in

  state := { !state with rows = rows ; columns = columns; grid = grid };

  run canvas
