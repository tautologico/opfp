(* 

  OCaml: Programação Funcional na Prática
  Andrei de A. Formiga - Casa do Código

  Exemplos do Capítulo 04 - Polimorfismo

 *)

(* 
  Os exemplos deste capítulo foram pensados para uso no REPL, digitando uma 
  expressão de cada vez. Quem estiver usando um editor integrado com o REPL 
  pode selecionar cada expressão neste arquivo e mandar para o REPL. 

  Para deixar claro onde termina cada expressão, e para ficar mais fácil 
  de copiar e colar cada uma no REPL, as expressões no arquivo terminam com ;;,
  apesar de código OCaml em arquivos não precisar deste terminador. 
*)

(* Listas polimórficas *)

type 'a lista = Nil | Cons of 'a * 'a lista;;

Nil;;

Cons (1, Nil);;

Cons ("Arthur", Cons ("Dent", Nil));;

let rec tamanho l = 
  match l with
  | Nil -> 0
  | Cons (_, rl) -> 1 + tamanho rl;;

(** 4.1 As listas predefinidas *)

[];;

1 :: 2 :: [];;

let rec tamanho l = 
  match l with
    [] -> 0
  | x :: rl -> 1 + tamanho rl;;

List.length ["Howard"; "Phillips"; "Lovecraft"];;

(* Mais exemplos com listas *)

let rec soma_lista l = 
  match l with 
    [] -> 0
  | x :: rl -> x + soma_lista rl;;

soma_lista [];;

soma_lista [1; 3; 9; 21; 7];;

String.capitalize "slartibartfast";;

let rec maiusculas_lista ls = 
  match ls with
    [] -> []
  | s :: rls -> String.capitalize s :: maiusculas_lista rls;;

maiusculas_lista ["joao"; "jose"; "da"; "silva"; "xavier"];;

(* Desconstruindo uma lista sem match *)

List.hd [1; 2; 3];;

List.tl [1; 2; 3];;

List.hd [];;

List.tl [];;

(** 4.2 Mais sobre padrões *)

let p = (1, 2);;

let (p1, p2) = p;;

let x, y, z, w = 1.2, 3.4, 0.75, 0.11;;

let h :: _ = [1; 2; 3; 4];;

let l1 = [];;

let h :: _ = l1;;

let soma_par p = 
  match p with
    (x, y) -> x + y;;

let soma_par (x, y) = x + y;;

(* Padrões complexos *)

type ancora = Centro | SupEsq | SupDir | InfEsq | InfDir;;

type ponto_ref = Ancora of ancora | Coord of int * int;;

let dist x y = sqrt ((float x) ** 2.0 +. (float y) ** 2.0);;

let largura = 640;;

let altura = 480;;

let rec dist_refs ps = 
  match ps with
    [] -> []
  | Ancora InfEsq :: rps -> 0.0 :: (dist_ref rps)
  | Ancora InfDir :: rps -> (dist largura 0) :: (dist_ref rps)
  | Ancora SupEsq :: rps -> (dist 0 altura) :: (dist_ref rps)
  | Ancora SupDir :: rps -> 
     (dist largura altura) :: (dist_ref rps)
  | Ancora Centro :: rps -> 
     (dist (largura/2) (altura/2)) :: (dist_ref rps)
  | Coord (x, y) :: rps -> (dist x y) :: (dist_ref rps);;

let dist_ref p = 
  match p with
    Ancora InfEsq -> 0.0
  | Ancora InfDir -> dist largura 0
  | Ancora SupEsq -> dist 0 altura
  | Ancora SupDir -> dist largura altura
  | Ancora Centro -> dist (largura/2) (altura/2)
  | Coord (x, y) -> dist x y;;

let rec dist_refs ps = 
  match ps with
    [] -> []
  | p :: rps -> dist_ref p :: (dist_refs ps);;

(* Padrões condicionais *)

type dia = Quente | Frio | Agradavel;;

let classifica temps = 
  match temps with
    (_, max) when max > 30.0 -> Quente
  | (min, _) when min < 15.0 -> Frio
  | _ -> Agradavel;;

classifica (0.0, 42.0);;

(* Padrões com alternativas e intervalos *)

type naipe = Copas | Espadas | Ouro | Paus;;

let vermelha n = 
  match n with
  | Copas -> true
  | Ouro -> true
  | Espadas -> false
  | Paus -> false;;

let vermelha n = 
  match n with
  | Copas | Ouro -> true
  | Espadas | Paus -> false

let maiuscula c = 
  match c with
    'A'..'Z' -> true
  | _ -> false

(* Nomeando partes de um padrão *)

let rec ultimo l = 
  match l with
    [] -> 0
  | x :: [] -> x
  | _ :: resto -> ultimo resto;; 

let min_max_lista l = 
  match List.sort compare l with
    [] -> (0, 0)
  | min :: resto -> (min, ultimo resto);;

min_max_lista [5; 1; 9; 11; 42; 12];;

min_max_lista [42];;

let min_max_lista l = 
  match List.sort compare l with
    [] -> (0, 0)
  | min :: resto as l_ord -> (min, ultimo l_ord);;

min_max_lista [42];;

min_max_lista [0];;

let min_max_lista l = 
  match List.sort compare l with
    [] -> (0, 0)
  | [x] -> (x, x)
  | min :: (_ :: rs as resto) -> (min, ultimo resto);;

(* match paralelo *)

type mao = Pedra | Papel | Tesoura;;

type result_jogo = J1Vence | J2Vence | Empate;;

let resultado m1 m2 = 
  match m1, m2 with
    _ when m1 = m2 -> Empate
  | Pedra, Papel -> J2Vence
  | Pedra, Tesoura -> J1Vence
  | Papel, Pedra -> J1Vence
  | Papel, Tesoura -> J2Vence
  | Tesoura, Pedra -> J2Vence
  | Tesoura, Papel -> J1Vence;;

(** 4.3 Árvores polimórficas e valores opcionais *)

type 'a arvore = Folha | No of 'a arvore * 'a * 'a arvore;;

let a1 = No (Folha, 7, No (Folha, 9, Folha));;

let a2 = No (No (Folha, 17, Folha), 21, No (Folha, 42, Folha));;

let a_ch = No (No (Folha, 'c', Folha), 'h', Folha);;

let rec soma_arvore a = 
  match a with
    Folha -> 0
  | No (a1, n, a2) -> soma_arvore a1 + n + soma_arvore a2;;

(* Busca em árvores binárias *)

(* o tipo 'a option já está definido na linguagem *)
(* type 'a option = None | Some of 'a *)

None;;

Some 5;;

Some "shawarma";;

let rec busca a v = 
  match a with
    Folha -> None
  | No (a1, n, a2) when n = v -> Some a
  | No (a1, n, _) when v < n -> busca a1 v
  | No (_, n, a2) -> busca a2 v;;

(=);;

(<);;

let a2 = No (No (Folha, 17, Folha), 21, No (Folha, 42, Folha));;

busca a2 111;;

busca a2 17;;

let contem a v = 
  match busca a v with
    None -> false
  | Some a2 -> true;;

