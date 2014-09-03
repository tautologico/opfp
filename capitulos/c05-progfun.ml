(* 

  OCaml: Programação Funcional na Prática
  Andrei de A. Formiga - Casa do Código

  Exemplos do Capítulo 05 - Programação Funcional

 *)

(** 5.1 A essência da programação funcional *)

(* Sem exemplos nesta seção *)


(** 5.2 Mutabilidade e outros efeitos *)

(* Sem exemplos nesta seção *)


(** 5.3 Programação recursiva *)

let rec soma_lista l = 
  match l with 
    [] -> 0
  | x :: rl -> x + soma_lista rl

(* Entra em loop infinito mas estoura a pilha *)
let rec loop_infinito x = 
  x + loop_infinito x

(* Entra em loop infinito mas tem recursão em cauda *)
let rec loop_infinito2 x = 
  loop_infinito2 x

let rec fatorial n = 
  match n with
    0 -> 1
  | 1 -> 1
  | _ -> n * fatorial (n - 1)

let rec fatorial_ac n ac = 
  match n with
    0 | 1 -> ac
  | _ -> fatorial_ac (n-1) (n * ac)

let fatorial_ac_5 = fatorial_ac 5 1

let fatorial n = 
  let rec fatorial_ac n ac = 
    match n with
      0 | 1 -> ac
    | _ -> fatorial_ac (n-1) (n * ac)
  in 
  fatorial_ac n 1


(** 5.4 Funções de primeira classe *)

let quadrado_anon_5 = (fun x -> x * x) 5

(* As duas definições seguintes para q são equivalentes *)
let q = fun x -> x * x
let q x = x * x

let cria_multiplicador x = fun y -> x * y

let mult5 = cria_multiplicador 5

let mult5_7 = mult5 7

let cria_mult_5_11 = cria_multiplicador 5 11

(* As três definições seguintes para mult são equivalentes *)
let mult x y = x * y
let mult x = fun y -> x * y
let mult = fun x -> fun y -> x * y

let mult_6_7 = mult 6 7

(* Função de multiplicação sem currying *)
let mul (x, y) = x * y

let mult5 = ( * ) 5

let lista_x2 = List.map (( * ) 2) [1; 2; 3; 4; 5]

let mult_7_6 = (fun x y -> x * y) 7 6


(** 5.5 Padrões de recursividade *)

(* Mapeamento *)
let rec quadrado_lista l = 
  match l with
    [] -> []
  | n :: rl -> (n * n) :: quadrado_lista rl

let rec maiusculas_lista ls = 
  match ls with
    [] -> []
  | s :: rls -> String.capitalize s :: maiusculas_lista rls

let rec map f l = 
  match l with
    [] -> []
  | x :: rl -> f x :: map f rl

let quadrado_lista l = map (fun x -> x * x) l

let maiusculas_lista l = map String.capitalize l

let quad_lista = map (fun x -> x * x) [1; 2; 3; 4; 5]

let tam_lista = map String.length ["Alys"; "Myau"; "Tyrone"; "Noah (Lutz)"]

(* Filtragem *)
let par n = (n mod 2) = 0

let rec pares_lista l = 
  match l with
    [] -> []
  | n :: rl when par n -> n :: pares_lista rl
  | n :: rl -> pares_lista rl

let pares_lista_1 = pares_lista [3; 11; 26; 13; 2; 0]

let rec filtrar p l = 
  match l with
    [] -> []
  | x :: rl when p x -> x :: filtrar p rl
  | x :: rl -> filtrar p rl

let filtra_par_1 = filtrar par [3; 11; 26; 13; 2; 0]

(* Redução (fold) *)
let rec soma_lista l = 
  match l with 
    [] -> 0
  | x :: rl -> x + soma_lista rl

let rec reducao f i l = 
  match l with 
    [] -> i
  | x :: rl -> f x (reducao f i rl)

let reducao_soma = reducao (+) 0 [1; 2; 3; 4]

let rec reducao_esq f i l = 
  match l with
    [] -> i
  | x :: rl -> reducao_esq f (f i x) rl


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

let a2_x2 = map_arvore (( * ) 2) a2

let rec fold_arvore f arv ini = 
  match arv with
  | Folha -> ini
  | No (ae, x, ad) -> 
     f x (fold_arvore f ae ini) (fold_arvore f ad ini)

let soma_a2 = fold_arvore (fun x r1 r2 -> x + r1 + r2) a2 0

