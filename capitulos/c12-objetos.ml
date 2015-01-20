(* 

  OCaml: Programação Funcional na Prática
  Andrei de A. Formiga - Casa do Código

  Exemplos do Capítulo 12 - Um pouco sobre objetos

 *)

(* 
  Os exemplos deste capítulo foram pensados para uso no REPL, digitando uma 
  expressão de cada vez. Quem estiver usando um editor integrado com o REPL 
  pode selecionar cada expressão neste arquivo e mandar para o REPL. 

  Para deixar claro onde termina cada expressão, e para ficar mais fácil 
  de copiar e colar cada uma no REPL, as expressões no arquivo terminam com ;;,
  apesar de código OCaml em arquivos não precisar deste terminador. 
*)


(** 12.1 Objetos *)

let p = 
  object 
    val mutable x = 0
    method get_x = x
    method inc_x = x <- x + 1
  end;;

p#get_x;;

p#inc_x;;

p#get_x;;

let get_x_dobro o = 
  o#get_x * 2;;

get_x_dobro p;;

(** 12.2 Classes *)

class contador = 
  object
    val mutable c = 0
    method get = c
    method inc = c <- c + 1
    method dec = c <- c - 1
    method inc_n n = c <- c + n
  end

let c1 = new contador;;

c1#inc_n 10;;

let c2 = new contador;;

c2#inc_n 2;;

c1#get;;

c2#get;;

(* classe com construtor *)
class contador = fun init -> 
  object
    val mutable c = init
    method get = c
    method inc_n n = c <- c + n
  end

(* açúcar sintático para construtor *)
class contador init = 
  object
    val mutable c = init
    method get = c
    method inc_n n = c <- c + n
  end

let c1 = new contador 10;;

c1#get;;

let c2 = new contador;;

let c3 = c2 42;;

c3#get;;
