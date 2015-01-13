(* 

  OCaml: Programação Funcional na Prática
  Andrei de A. Formiga - Casa do Código

  Este é um exemplo do Capítulo 08 - Módulos, 
  na seção 8.3 - Módulos e arquivos. 
  O arquivo ponto2D.ml define o módulo Ponto2D.

 *)

type t = { x : int; y : int }

let zero () = { x = 0; y = 0 }

let criar x y = { x; y }

let ( + ) p1 p2 = { x = p1.x + p2.x; y = p1.y + p2.y }
