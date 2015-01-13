(* 

  OCaml: Programação Funcional na Prática
  Andrei de A. Formiga - Casa do Código

  Exemplos do Capítulo 07 - Características imperativas

 *)

(* 
  Os exemplos deste capítulo foram pensados para uso no REPL, digitando uma 
  expressão de cada vez. Quem estiver usando um editor integrado com o REPL 
  pode selecionar cada expressão neste arquivo e mandar para o REPL. 

  Para deixar claro onde termina cada expressão, e para ficar mais fácil 
  de copiar e colar cada uma no REPL, as expressões no arquivo terminam com ;;,
  apesar de código OCaml em arquivos não precisar deste terminador. 
*)

(** 7.1 O tipo unit *)

();;

print_string;;

print_string "cellar door ";;

(** 7.2 Entrada e saída *)

Printf.printf "Inteiro: %d, float: %5.3f\n" 42 3.14159265;;

(* Trabalhando com arquivos *)

(** 7.3 Sequenciamento de expressões *)

let raiz_positiva a b c = 
  Printf.printf "Na funcao raiz_positiva\n";
  let delta = b *. b -. 4.0 *. a *. c in
  Printf.printf "Delta = %5.3f\n" delta;
  if delta >= 0.0 then 
    (-.b +. sqrt delta) /. 2.0 *. a 
  else infinity

raiz_positiva 3.0 4.0 5.0;;

O REPL indica o erro no %%else%%, pois aparentemente esse %%else%% 
está ligado à segunda expressão da sequência, na qual não existe nenhum %%if%%. 
A forma correta de fazer isso é protegendo a sequência usando parênteses ou 
%%begin%% e %%end%%. Como a sequência está em uma estrutura de controle, 
recomenda-se a segunda forma: 

let raiz_positiva a b c = 
  let delta = b *. b -. 4.0 *. a *. c in
  if delta >= 0.0 then 
    begin
      Printf.printf "Existe raiz positiva\n";
      (-.b +. sqrt delta) /. 2.0 *. a 
    end
  else infinity;;


(** 7.4 Atualização funcional de registros *)

type ponto2d = { x : int; y : int }

let mover_vert p delta = { x = p.x; y = p.y + delta }

let p1 = {x = 2; y = 6};;

let p2 = mover_vert p1 10;;

p1;;

let mover_vert p delta = { p with y = p.y + delta }

Nesse caso, não houve muita economia pois só existem dois campos no 
registro, mas caso %%p%% fosse um registro com 10 campos, a função 
seria ainda a mesma. Vários campos podem ser atualizados ao mesmo 
tempo, separando as atualizações com ponto e vírgula:

{ p1 with x = 2; y = 7 };;


(** 7.5 Registros com campos mutáveis *)

type ponto2d = { mutable x: int; mutable y: int }

let p1 = {x = 5; y = 7};;

p1.x <- 20;;

let mover_vert_imp p delta = p.y <- p.y + delta

mover_vert;;

mover_vert_imp;;


(** 7.6 Referências *)

let xref = ref 0;;

!xref;;

xref := 5;;

!xref;;

(* Definição para fins didáticos; a tipo 'a ref é predefinido em OCaml *)
(*
type 'a ref = { mutable contents: a }

let ref x = { contents = x }
*)

(*
let ( ! ) r = r.contents

let ( := ) r v = r.contents <- v
*)


(** 7.7 Arrays *)

Array.make 10 0;;

let a1 = [| 4; 8; 15; 16; 23; 42 |];;

a1.(0);;

a1.(5);;

a1.(2) <- 51;;

a1;;

Array.length a1;;

(* Funções de alta ordem com arrays *)

[title Funções de alta ordem com ::arrays::]

Array.map (fun x -> x * x) a1;;

a1;;

Array.mapi (fun i x -> i * x) a1;;

a1;;

Array.iteri (fun i x -> a1.(i) <- x * x) a1;;

a1;;

(** 7.8 Estruturas de controle imperativas *)

let raiz_positiva a b c = 
  let delta = b *. b -. 4.0 *. a *. c in
  if delta = 0.0 then Printf.printf "delta = 0, raiz unica\n";
  if delta >= 0.0 then 
    (-.b +. sqrt delta) /. 2.0 *. a 
  else infinity

(* Loops *)

let soma_array a = 
  let soma = ref 0 in
  let tam = Array.length a in 
  for i = 0 to (tam - 1) do
    soma := !soma + a.(i)
  done;
  !soma

let soma_array a = 
  Array.fold_left (+) 0 a  

for i = 5 downto 1 do 
  Printf.printf "iteracao %d\n" i
done;;

(** 7.9 Exceções *)

Invalid_argument "mensagem";;

raise @@ Invalid_argument "mensagem";;

sqrt @@ -.2.3;;

let sqrt_exn x = 
  if x < 0.0 then 
    raise @@ Invalid_argument "sqrt_exn: numero negativo"
  else 
    sqrt x;;

sqrt_exn 25.0;;

sqrt_exn @@ -.2.3;;

raise;;

let sqrt_exn x = 
  if x < 0.0 then 
    invalid_arg "sqrt_exn: numero negativo"
  else 
    sqrt x;;

(* Tratamento de exceções *)
try 
  sqrt_exn @@ -.3.7
with 
  Invalid_argument m -> 
  Printf.printf "Excecao: argumento negativo\n"; 0.0;;

try 
  sqrt_exn @@ -.3.7
with 
  Invalid_argument m -> Printf.printf "Excecao: %s\n" m; 0.0;;

try
  sqrt_exn @@ -.3.14  
with 
  Failure _ -> Printf.printf "Falha\n"; 0.0;;

try
  sqrt_exn @@ -.3.14
with
  Failure _ -> Printf.printf "Falha\n"; 0.0
| Invalid_argument _ -> Printf.printf "inv_arg\n"; 0.0;;

try
  sqrt_exn @@ -.3.14
with
  e -> Printf.printf "Excecao\n"; 0.0;;

(* Declarando novas exceções *)

exception AlertaVermelho;;

raise AlertaVermelho;;

exception Mensagem of string;;

raise @@ Mensagem "ALERTA!!!!";;

