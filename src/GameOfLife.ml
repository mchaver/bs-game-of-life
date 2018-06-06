(* https://gist.github.com/woxtu/610b88dd09a3856bcb77899314e16039 *)
(* https://qiita.com/woxtu/items/4e073e8e8dbf8596388e *)
module Element = struct
  type t
end

module Context = struct
  type t
  external setFillStyle : t -> string -> unit = "fillStyle" [@@bs.set]
  external fillRect : int -> int -> int -> int -> unit = "" [@@bs.send.pipe: t]
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

let size = 2
let foregroundColor = "#F36"
let backgroundColor = "#000"

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
  
  run canvas grid
