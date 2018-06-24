http://blog.klipse.tech/reason/2017/10/17/externals-js-ffi-reason.html
https://stackoverflow.com/questions/2481350/how-to-get-scrollbar-position-with-javascript
https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
https://en.wikipedia.org/wiki/Gun_(cellular_automaton)
https://codereview.stackexchange.com/questions/114702/drawing-a-grid-on-canvas

```OCaml
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

(* external getElementById : string -> Dom.element option = "document.getElementById" [@@bs.val] [@@bs.return nullable] *)
(*  external getContext : string -> Context.t = "" [@@bs.send] *)
(* https://gist.github.com/woxtu/610b88dd09a3856bcb77899314e16039 *)
(* https://qiita.com/woxtu/items/4e073e8e8dbf8596388e *)

  context |> Context.fillRect ((strokeWidth + 1) * 1 + (cellSize * 0)) (strokeWidth * 1 + (cellSize * 0)) (cellSize * 1) (cellSize * 1);
  context |> Context.fillRect ((strokeWidth * 2 + 1) * 1 + (cellSize * 1)) (strokeWidth * 1 + (cellSize * 0)) (cellSize * 1) (cellSize * 1)




let calculateColumnsAndRows width height stroke cellSize =
  ((width / (stroke + cellSize)), (height / (stroke + cellSize)))

```
