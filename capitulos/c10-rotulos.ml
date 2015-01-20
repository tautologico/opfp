(* 

  OCaml: Programação Funcional na Prática
  Andrei de A. Formiga - Casa do Código

  Exemplos do Capítulo 10 - Parâmetros rotulados

 *)

(* 
  Os exemplos deste capítulo foram pensados para uso no REPL, digitando uma 
  expressão de cada vez. Quem estiver usando um editor integrado com o REPL 
  pode selecionar cada expressão neste arquivo e mandar para o REPL. 

  Para deixar claro onde termina cada expressão, e para ficar mais fácil 
  de copiar e colar cada uma no REPL, as expressões no arquivo terminam com ;;,
  apesar de código OCaml em arquivos não precisar deste terminador. 
*)

(** 10.1 Rótulos para nomear parâmetros *)

let foldl ~f ~ini ~lista = 
  List.fold_left f ini lista;;

foldl ~f:(+) ~ini:0 ~lista:[1; 2; 3; 4; 5];;

foldl ~ini:0 ~f:(+) ~lista:[1; 2; 3; 4; 5];;

foldl ~lista:[1; 2; 3; 4; 5] ~f:(+) ~ini:0;;

let mapquad = List.map (fun x -> x * x);;

mapquad [14; 42];;

mapquad [7; 3; 5];;

let map ~f ~lista = 
  List.map f lista;;

let mapquad = map ~f:(fun x -> x * x);;

mapquad [14; 42];;

mapquad [7; 3; 5];;

let map123 = map ~lista:[1; 2; 3];;

map123 (fun x -> x + 2);;

map123 (fun x -> x * 2);;

map (fun x -> x * x) [1; 2; 3; 4];;

foldl;;

foldl ~f:(+) ~ini:0 ~lista:[1; 2; 3; 4; 5];;

foldl (+) 0 [1; 2; 3; 4; 5];;

map;;

ListLabels.fold_left;;

ListLabels.map;;

(* Rótulos e nomes de parâmetros *)

let f ~x:x1 ~y:y1 = x1 * y1;;

f ~x:3 ~y:9;;

let g ~x:x1 ~x:x2 ~y:y1 = x1 * y1 + x2;;

g ~x:3 ~x:5 ~y:2;;

g ~x:3 ~y:2 ~x:5;;

let h ~x y z = x * y + z;;

h ~x:3 2 5;;

h 2 ~x:3 5;;

h 2 5 ~x:3;;

let h2 = h 2;;

let h25 = h 2 5;;

h25 ~x:3;;

h25 3;;

let h3 = h ~x:3;;

h3 2 5;;

(** 10.2 Parâmetros opcionais *)

let raiz ?(n=2) x = 
  x ** (1.0 /. (float n));;

raiz 25.0;;

raiz 27.0;;

raiz ~n:3 27.0;;

let f ?(x=1.0) = x *. x;;

f;;

(* 
f 2.0;;
 *)

f ~x:2.0;;

let opcional_rot ?(x=1.0) ~y = 
  x *. y;;

opcional_rot 3.4;;

opcional_rot ~x:2.0 ~y:2.3;;

opcional_rot ~y:2.3;;

(* Valores default *)

let recip ?num den = 
  match num with
  | Some n -> n /. den
  | None -> 1.0 /. den

let recip ?(num=1.0) den = 
  num /. den

recip 25.0;;

recip ~num:2.0 25.0;;

(* 
(recip 25.0) ~num:2.0;;
 *)

(** 10.3 Inferência de tipos e funções de alta ordem *)

let f ~x ~y = x * y;;

let h g = g ~y:2 ~x:3;;

(*
h f;;
 *)

let h (g: x:int -> y:int -> int) = g ~y:2 ~x:3;;

h f;;

let intervalo ?(inc = 1) min max = 
  let rec loop i = 
    if i > max then [] else i :: loop (i + inc) 
  in
  loop min;;

intervalo 1 10;;

intervalo ~inc:2 2 16;;

let teste_inc r = r ~inc:2 2 20;;

(*
test_inc intervalo;;
 *)

let h g = g ~x:3 ~y:2;;

let f x y = x * y;;

h f;;

(** 10.4 Sugestões para o bom uso de rótulos *)

module List = ListLabels;;

List.fold_right ~f:(+) ~init:0 [1; 2; 3; 4; 5];;


let h1 g = g ~x:3 ~y:2;;

let f1 x y = x * y;;

(*
h1 f1;;
 *)

let f1_com ~x ~y = f x y;;

h1 f1_com;;

