module Element = struct
  type t
  external setInnerHTML : t -> string -> unit = "innerHTML" [@@bs.set]

  external offsetLeft : t -> int = "" [@@bs.get]
  external offsetTop : t -> int = "" [@@bs.get]
  external scrollLeft : t -> int = "" [@@bs.get]
  external scrollTop : t -> int = "" [@@bs.get]
  external offsetParent : t -> t option = "" [@@bs.get] [@@bs.return null_to_opt]
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

module Window = struct
  external requestAnimationFrame: (unit -> unit) -> unit = "" [@@bs.val]
end

module Mouse = struct
  type t
  external getElementById : string -> t option = "document.getElementById" [@@bs.val] [@@bs.return null_to_opt]
  external setInnerHTML : t -> string -> unit = "innerHTML" [@@bs.set]
end               
              
module MouseEvent = struct
  type t
  external addEventListener : string -> (t -> unit) -> unit = "" [@@bs.send.pipe: Mouse.t]
  external addClickEventListener : (_ [@bs.as "click"]) -> (t -> unit) -> unit = "addEventListener" [@@bs.send.pipe : Mouse.t]
end              

module Canvas = struct
  type t
  external width : t -> int = "" [@@bs.get]
  external height : t -> int = "" [@@bs.get]
  external getContext : string -> Context.t = "" [@@bs.send.pipe: t]
  external getElementById : string -> t option = "document.getElementById" [@@bs.val] [@@bs.return null_to_opt]

  external offsetLeft : t -> int = "" [@@bs.get]
  external offsetTop : t -> int = "" [@@bs.get]
  external scrollLeft : t -> int = "" [@@bs.get]
  external scrollTop : t -> int = "" [@@bs.get]
  external offsetParent : t -> Element.t option = "" [@@bs.get] [@@bs.return null_to_opt]
end

module CanvasEvent = struct
  type t
  external clientX : t -> int = "" [@@bs.get]
  external clientY : t -> int = "" [@@bs.get]

  external pageX : t -> int = "" [@@bs.get]
  external pageY : t -> int = "" [@@bs.get]                              
  external addEventListener : string -> (t -> unit) -> unit = "" [@@bs.send.pipe: Canvas.t]
  external addClickEventListener : (_ [@bs.as "click"]) -> (t -> unit) -> unit = "addEventListener" [@@bs.send.pipe : Canvas.t]
end

module Option = struct
  type t
  external value : t -> string = "" [@@bs.get]
end
                   
module Select = struct
  type t
  external getElementById : string -> t option = "document.getElementById" [@@bs.val] [@@bs.return null_to_opt]
  external selectedIndex : t -> int = "" [@@bs.get]
  external options : t -> Option.t array = "" [@@bs.get]
end               

module SelectEvent = struct
  type t
  external addEventListener : string -> (t -> unit) -> unit = "" [@@bs.send.pipe: Select.t]
  external addChangeEventListener : (_ [@bs.as "change"]) -> (t -> unit) -> unit = "addEventListener" [@@bs.send.pipe : Select.t]
end              

let isNone o =
  match o with
  | Some _o -> false
  | None -> true       

let isSome o =
  match o with
  | Some _o -> true
  | None -> false    

let relMouseCoords (c: Canvas.t) (e: CanvasEvent.t) =
  let totalOffsetX = ref @@ (Canvas.offsetLeft c) - (Canvas.scrollLeft c) in
  let totalOffsetY = ref @@ (Canvas.offsetTop c) - (Canvas.scrollTop c) in
  let currentElement = ref (Canvas.offsetParent c) in

  while isSome !currentElement do
    match !currentElement with
    | None -> ()
    | Some ec ->
       totalOffsetX := !totalOffsetX + (Element.offsetLeft ec) - (Element.scrollLeft ec);
       totalOffsetY := !totalOffsetY + (Element.offsetTop ec) - (Element.scrollTop ec);
       currentElement := Element.offsetParent ec
  done;

  let canvasX = (CanvasEvent.pageX e) - !totalOffsetX in
  let canvasY = (CanvasEvent.pageY e) - !totalOffsetY in

  (canvasX, canvasY)
  (* while  *)
(*
function relMouseCoords(event){
    var totalOffsetX = 0;
    var totalOffsetY = 0;
    var canvasX = 0;
    var canvasY = 0;
    var currentElement = this;

    do{
        totalOffsetX += currentElement.offsetLeft - currentElement.scrollLeft;
        totalOffsetY += currentElement.offsetTop - currentElement.scrollTop;
    }
    while(currentElement = currentElement.offsetParent)

    canvasX = event.pageX - totalOffsetX;
    canvasY = event.pageY - totalOffsetY;

    return {x:canvasX, y:canvasY}
}
         *)                  

type speed =
  | VerySlow
  | Slow
  | Normal
  | Fast
  | VeryFast

let parse_speed v =
  match v with
  | "very-slow" -> Some VerySlow
  | "slow" -> Some Slow
  | "normal" -> Some Normal
  | "fast" -> Some Fast
  | "very-fast" -> Some VeryFast
  | _ -> None

let speed_to_int s =
  match s with
  | VerySlow -> 1000
  | Slow -> 500
  | Normal -> 100
  | Fast -> 50
  | VeryFast -> 0
                 
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
  ; speed   : speed
  }

let state: state ref =
  ref
    { run = true
    ; grid = [||]
    ; rows = 0
    ; columns = 0
    ; speed = Normal
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
                      
let calcIndex (x: int) (y: int) rows columns =
  let o_xIndex = ref None in
  let xCount = ref 0 in
  let maxWidth = columns * (strokeWidth + cellSize) in
  let width = ref strokeWidth in
  while !width < maxWidth && isNone !o_xIndex do
    if x >= !width && x <= !width + strokeWidth + cellSize
    then
      o_xIndex := Some !xCount
    else
      xCount := !xCount + 1;
      width := !width + strokeWidth + cellSize;
  done;
  match !o_xIndex with
  | None -> None
  | Some xIndex ->
     let o_yIndex = ref None in
     let yCount = ref 0 in
     let maxHeight = rows * (strokeWidth + cellSize) in     
     let height = ref (strokeWidth - 1) in
     while !height < maxHeight && isNone !o_yIndex do
       if y >= !height && y <= !height + strokeWidth + cellSize
       then
         o_yIndex := Some !yCount
       else
         yCount := !yCount + 1;
       height := !height + strokeWidth + cellSize;
     done;
     match !o_yIndex with
     | None -> None
     | Some yIndex -> Some (xIndex, yIndex)
         
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

(* throw away result and return unit *)
let void _x = ()
           
let rec run canvas =
  let context = canvas |> Canvas.getContext "2d" in
  drawGrid canvas !state.rows !state.columns;
  context |> draw !state.grid;
  void @@ Js.Global.setTimeout (fun () ->
  if (!state.run)
  then
    begin
      state := {!state with grid = next !state.grid};
      Window.requestAnimationFrame @@ fun () -> run canvas
    end
  else ()
    ) @@ speed_to_int !state.speed

let resetGrid rows columns =
  Random.self_init ();
  
  let grid =
    (Array.make_matrix rows columns () |> Array.map @@ fun cells ->
      cells |> Array.map @@ fun _ -> if (Random.bool ()) then Alive else Dead) in

  state := { !state with grid = grid }
  
(* draw each line, move*)
(* touchend *)
let main =
  let canvas = 
    match (Canvas.getElementById "canvas") with
    | None -> failwith "Cannot find the canvas"
    | Some canvas -> canvas in

  let play_button =
    match (Mouse.getElementById "play-button") with
    | None -> failwith "Cannot find the play-button"
    | Some play_button -> play_button in

  let random_reset_button =
    match (Mouse.getElementById "random-reset-button") with
    | None -> failwith "Cannot find the random-reset-button"
    | Some random_reset_button -> random_reset_button in

  let speed_select =
    match (Select.getElementById "speed-select") with
    | None -> failwith "Cannot find the speed-select"
    | Some speed_select -> speed_select in
  
  let handleClickGrid (e: CanvasEvent.t) = (
    Js.log("handleClickGrid");
    (* let x = CanvasEvent.clientX e in *)
    (* let y = CanvasEvent.clientX e in *)
    let (x, y) = relMouseCoords canvas e in
    Js.log(x);
    Js.log(y);
    let o = calcIndex x y !state.rows !state.columns in
    match o with
    | None -> ()
    | Some (x, y) ->
       let g = !state.grid in
       let c = g.(x).(y) in
       let newC = match c with
         | Alive -> Dead
         | Dead -> Alive in
       g.(x).(y) <- newC;
       state := {!state with grid = g};
       let context = canvas |> Canvas.getContext "2d" in
       drawGrid canvas !state.rows !state.columns;
       context |> draw !state.grid;
    ) in
  canvas |> CanvasEvent.addClickEventListener (fun e -> handleClickGrid e);

  let toggleRun _unit =
    state := { !state with run = not !state.run };
    if !state.run
    then
      begin
        Mouse.setInnerHTML play_button "Pause";
        run canvas
      end
    else Mouse.setInnerHTML play_button "Play" in

  play_button |> MouseEvent.addClickEventListener (fun _unit -> toggleRun ());

  let handleSpeedSelectChange _event =(
    let selectedIndex = Select.selectedIndex speed_select in
    let option = (Select.options speed_select).(selectedIndex) in
    match parse_speed @@ Option.value option with
    | None -> () 
    | Some speed -> state := {!state with speed = speed}) in

  speed_select |> SelectEvent.addChangeEventListener handleSpeedSelectChange;

  let randomGrid _unit =
    resetGrid !state.rows !state.columns;
    state := { !state with run = false };
    run canvas in
  
  random_reset_button |> MouseEvent.addClickEventListener (fun _unit -> randomGrid ());
  
  let columns = ((canvas |> Canvas.width) - strokeWidth) / (strokeWidth + cellSize) in
  let rows = ((canvas |> Canvas.height) - strokeWidth) / (strokeWidth + cellSize) in
  
  Random.self_init ();
  
  let grid =
    (Array.make_matrix rows columns () |> Array.map @@ fun cells ->
      cells |> Array.map @@ fun _ -> if (Random.bool ()) then Alive else Dead) in

  state := { !state with rows = rows ; columns = columns; grid = grid };

  run canvas
