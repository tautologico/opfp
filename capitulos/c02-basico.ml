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

(** 2.1 Primeiros passos *)

print_string "Hello, world!\n";;

3 + 7 * 2;;

(* computador, por favor some 2 mais 3 *) 2 + 3

(** 2.2 Variáveis e tipos básicos *)

let x = 7;;

let resposta = x * 6;;

let s = "Aussonderungsaxiom";;

let pi = 3.14159265359;;

let b1 = 1 < 2;;

let c = 'A';;

(* Operações com inteiros *)

11 / 3;;

11 mod 3;;

(* Operações com números de ponto flutuante *)

3.0 *. 4.5 +. 2.2;;

(11.0 /. 3.0) -. 2.0;;

(float 11) /. 3.0;;

truncate 3.14159265358979312;;

2.0 ** 8.0;;

(* Operações booleanas *)

true && false;;

false || not true;;

2 = 2;;

2 = 3 || 2 <> 3;;

let x = if 3 > 4 then 3 else 4;;

(* Operações básicas com strings *)

let inigo = "Meu nome eh " ^ "Inigo Montoya";;

inigo.[4];;

inigo.[35];;

(** 2.3 Funções *)

let quadrado x = x * x;;

quadrado 5;;

let mult x y = x * y;;

mult 4 5;;

(* Definindo operadores *)

(+);;

(+.);;

let (++) x y = x + y + y;;

5 ++ 7;;

let ( * ) x y = x *. y;;

(* Declarações locais *)

let x = 2 in x * x * 4;;

let x = 1001;;

let x = 2 in x * x * 4;;

x;;

let raiz_positiva a b c = 
  if (b *. b -. 4.0 *. a *. c) >= 0.0 then 
    (-.b +. (sqrt (b *. b -. 4.0 *. a *. c))) /. (2.0 *. a)
  else infinity;;

let raiz_positiva a b c = 
  let delta = b *. b -. 4.0 *. a *. c in
  if delta >= 0.0 then 
    (-.b +. sqrt delta) /. 2.0 *. a 
  else infinity;;

(* Funções recursivas *)

let rec fatorial n = 
  if n = 0 then 1 else n * fatorial (n - 1);;

fatorial 3;;

fatorial 5;;

let rec par n = 
  if n = 0 then true 
  else if n = 1 then false
  else impar (n-1)
and impar n = 
  if n = 0 then false
  else if n = 1 then true
  else par (n-1);;

(** 2.4 Tipos agregados *)

let p = (1, "ichi");;

fst p;;

snd p;;

let n, str = p;;

(* Tuplas *)

let tr = (3.14, "pi", true);;

let qd = (2, "fast", 2.0, "furious");;

let dist (x1, y1, z1) (x2, y2, z2) = 
  sqrt ((x1 -. x2) ** 2.0 +. (y1 -. y2) ** 2.0 +. 
          (z1 -. z2) ** 2.0);;

dist (0.0, 0.0, 0.0) (1.0, 2.0, 3.0);;

(* Listas *)
[1; 2; 3; 4];;

List.length [1; 2; 3; 4];;

[1; 2; 3; 4] @ [5; 6; 7; 8];;

4, 5;;

[1, 2, 3];;

let l1 = [0; 1; 2; 3; 4];;

List.nth l1 2;;

List.sort compare [5; 9; 1; 7; 13; 21];;

List.filter par [3; 9; 2; 1; 12; 7; 8];;

(* Declaração explícita de tipo *)

let x : int = 3;;

let quadrado (x : int) = x * x;;

let mult x (y : int) = x * y;;

let quadrado (x : int) : int = x * x;;

