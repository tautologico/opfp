(* 

  OCaml: Programação Funcional na Prática
  Andrei de A. Formiga - Casa do Código

  Exemplos do Capítulo 02 - Tipos e valores basicos

 *)

(* 
  Os exemplos deste capítulo foram pensados para uso no REPL, digitando uma 
  expressão de cada vez. Quem estiver usando um editor integrado com o REPL 
  pode selecionar cada expressão neste arquivo e mandar para o REPL. 

  Para deixar claro onde termina cada expressão, e para ficar mais fácil 
  de copiar e colar cada uma no REPL, as expressões no arquivo terminam com ;;,
  apesar de código OCaml em arquivos não precisar deste terminador. 
*)

(** 3.1 Sinônimos de tipo *)

type ponto2d = float * float;;

let dist_origem (p : ponto2d) = 
  sqrt ((fst p) ** 2.0 +. (snd p) ** 2.0);;

let dist_origem p = 
  sqrt ((fst p) ** 2.0 +. (snd p) ** 2.0);;

type retangulo = float * float * float * float;;

(** 3.2 Registros *)

type ponto2d = { x : float; y : float };;

{ x = 2.0; y = 3.0 };;

let criar_ponto2d xp yp = { x = xp; y = yp };;

let criar_ponto2d x y = { x = x; y = y };;

let criar_ponto2d x y = { x; y };;

let p = criar_ponto2d 1.2 3.6;;

p.x;;

p.y;;

(* Nomes de campos *)

let sum_xy p = p.x +. p.y;;

type ponto3d = { x : float; y : float; z : float };;

let mult_xy p = p.x *. p.y;;

let mult_xy (p : ponto2d) = p.x *. p.y;;

(** 3.3 Variantes simples *)

type mao = Pedra | Papel | Tesoura;;

Pedra;;

let m1 = Tesoura;;

(* Pattern matching simples *)

let vence_de m = 
  if m = Pedra then Papel
  else if m = Papel then Tesoura
  else Pedra;; (* m = Tesoura *)

vence_de Tesoura;;

let vence_de m = 
  match m with
    Pedra -> Papel
  | Papel -> Tesoura
  | Tesoura -> Pedra;;

let perde_de_errado1 m = 
  if m = Pedra then Tesoura
  else Pedra (* m = Papel *);;

perde_de_errado1 Tesoura;;

let perde_de_errado2 m = 
  match m with
    Pedra -> Tesoura
  | Papel -> Pedra;;

(* O tipo determina o algoritmo *)


(** 3.4 Variantes com valores associados *)

type figura = Retangulo of float * float | Circulo of float 
              | Triangulo of float;;

Circulo 5.0;;

Retangulo (3.2, 1.2);;

(* Pattern matching com valores *)

let pi = 3.14159265359;;

let perimetro f = 
  match f with
    Retangulo (l, a) -> 2.0 *. l +. 2.0 *. a
  | Circulo r -> 2.0 *. pi *. r
  | Triangulo l -> 3.0 *. l;;

let perimetro f = 
  match f with
    Retangulo (largura, altura) -> 2.0 *. largura +. 2.0 *. altura
  | Circulo raio -> 2.0 *. pi *. raio
  | Triangulo lado -> 3.0 *. lado;;

let redondo f = 
  match f with
    Retangulo (l, a) -> false
  | Circulo r -> true
  | Triangulo l -> false;;

let redondo f = 
  match f with
    Retangulo (_, _) -> false
  | Circulo _ -> true
  | Triangulo _ -> false;;

let redondo f = 
  match f with
    Retangulo _ -> false
  | Circulo _ -> true
  | Triangulo _ -> false;;

let redondo f = 
  match f with
  | Circulo _ -> true
  | _ -> false;;

redondo @@ Triangulo 5.23;;
- : bool = false

(** 3.5 Tipos recursivos *)

type lista_int = Nil | Cons of int * lista_int;;

Nil;;

let l1 = Cons (1, Nil);;

let l2 = Cons (2, l1);;

(* Pattern matching e a estrutura recursiva das listas *)

let rec tamanho l = 
  match l with
    Nil -> 0
  | Cons (x, rl) -> 1 + tamanho rl;;

tamanho Nil;;

tamanho (Cons (1, Nil));;

tamanho l2;;

(** 3.5 Árvores *)

type arvore_int = 
    Folha 
  | No of arvore_int * int * arvore_int;;

let a1 = No (Folha, 7, No (Folha, 9, Folha));; 

let a2 = No (No (Folha, 17, Folha), 21, No (Folha, 42, Folha));;

let a3 = No (a1, 12, a2);;

let rec soma_arvore a = 
  match a with
    Folha -> 0
  | No (a1, n, a2) -> soma_arvore a1 + n + soma_arvore a2;;

soma_arvore a1;;

soma_arvore a2;;

soma_arvore a3;;
