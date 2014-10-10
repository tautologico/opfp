open OUnit2
open Titanic

(* passageiros *)
let p1 = { 
  id = 1;
  sobreviveu = false;
  classe = Terceira;
  nome = "Jack";
  gen = Some Masc;
  idade = 20.0;
  irmpar = 0;
  paisfilhos = 0;
  passagem = "Clandestino";
  preco = 0.0;
  cabine = "C1";
  embarque = Some Southampton
}

let p2 = { 
  id = 2;
  sobreviveu = true;
  classe = Primeira;
  nome = "Rose";
  gen = Some Fem;
  idade = 21.0;
  irmpar = 0;
  paisfilhos = 0;
  passagem = "A2";
  preco = 10.0;
  cabine = "A1";
  embarque = Some Cherbourg
}

(* Patrick e fulano de Downton Abbey *)

let passageiros = [p1; p2]

let sobreviventes = List.filter (fun p -> p.sobreviveu) passageiros

let mortos = List.filter (fun p -> not p.sobreviveu) passageiros

(* testes de unidade *)
let t_entropia ctxt = 
  List.iter (assert_equal ~cmp:(cmp_float ~epsilon:0.001) 0.0)
            [entropia sobreviventes; entropia mortos]

(* a suite de testes *)
let suite = 
  "suite" >:::
    ["calculo de entropia" >:: t_entropia]

(* executor dos testes *)
let () = 
  run_test_tt_main suite

