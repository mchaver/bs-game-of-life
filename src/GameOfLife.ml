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

let clear canvas context = (
  Context.setFillStyle context backgroundColor;
  context |> Context.fillRect 0 0 (canvas |> Canvas.width) (canvas |> Canvas.height);
)

let drawGrid canvas =
  let context = canvas |> Canvas.getContext "2d" in
  context |> clear canvas;
  Context.setLineWidth context (float_of_int strokeWidth);
  Context.setStrokeStyle context gridColor;
  context |> Context.beginPath;
  let maxHeight = canvas |> Canvas.height in
  let maxWidth = canvas |> Canvas.width in
  let width = ref strokeWidth in
  while !width + strokeWidth + cellSize < maxWidth do
    context |> Context.moveTo (float_of_int !width) (float_of_int 0);
    context |> Context.lineTo (float_of_int !width) (float_of_int maxHeight);
    width := !width + strokeWidth + cellSize;
  done;
  let height = ref (strokeWidth - 1) in
  while !height + strokeWidth + cellSize < maxHeight do
    context |> Context.moveTo (float_of_int strokeWidth) (float_of_int !height);
    context |> Context.lineTo (float_of_int maxWidth) (float_of_int !height);
    height := !height + strokeWidth + cellSize;
  done;
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

let rec run canvas grid =
  let context = canvas |> Canvas.getContext "2d" in
  drawGrid canvas;
  context |> draw grid;
  Window.requestAnimationFrame @@ fun () -> run canvas @@ next grid

(* draw each line, move*)
                                          
let main =
  let canvas = 
    (match (Document.getElementById "canvas") with
    | None -> failwith "Cannot find the canvas"
    | Some canvas -> canvas) in

  let rows = (canvas |> Canvas.width) / (strokeWidth + cellSize) in
  let columns = (canvas |> Canvas.height) / (strokeWidth + cellSize) in

  Random.self_init ();
  
  let grid =
    (Array.make_matrix rows columns () |> Array.map @@ fun cells ->
      cells |> Array.map @@ fun _ -> if (Random.bool ()) then Alive else Dead) in

  (* drawGrid canvas *)
  run canvas grid
