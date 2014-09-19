type t = { x : int; y : int }

let zero () = { x = 0; y = 0 }

let criar x y = { x; y }

let ( + ) p1 p2 = { x = p1.x + p2.x; y = p1.y + p2.y }
