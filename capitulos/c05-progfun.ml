(* 

  OCaml: Programação Funcional na Prática
  Andrei de A. Formiga - Casa do Código

  Exemplos do Capítulo 05 - Programação Funcional

 *)

(** 5.1 A essência da programação funcional *)

(** 5.2 Mutabilidade e outros efeitos *)

(** 5.3 Programação recursiva *)

(** 5.4 Funções de primeira classe *)

(** 5.5 Padrões de recursividade *)

(** 5.6 Tipos como fonte de informação *)

(* Função identidade *)
let g x = x

(** 5.7 Dois operadores para aplicar funções *)

(* Redefinicao do operador @@ para fins didáticos *)
let ( @@ ) f x = f x

let quadrado x = x * x

let quad_soma_1 l = quadrado (List.fold_left (+) 0 l)

let quad_soma_2 l = quadrado @@ List.fold_left (+) 0 l

(* Redefinição do operador |> para fins didáticos *)
let ( |> ) x f = f x


(** 5.8 Funções de alta ordem em árvores *)

type 'a arvore = Folha | No of 'a arvore * 'a * 'a arvore

let rec map_arvore f a = 
  match a with
  | Folha -> Folha
  | No (ae, x, ad) -> 
     No (map_arvore f ae, f x, map_arvore f ad)

let a2 = No (No (Folha, 17, Folha), 21, No (Folha, 42, Folha))

let a2_x2 = map_arvore (fun x -> x * 2) a2

let rec fold_arvore f arv ini = 
  match arv with
  | Folha -> ini
  | No (ae, x, ad) -> 
     f x (fold_arvore f ae ini) (fold_arvore f ad ini)

let soma_a2 = fold_arvore (fun x r1 r2 -> x + r1 + r2) a2 0

