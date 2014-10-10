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

let t_max_f ctxt = 
  let l1 = [1.1; 0.9; 2.4; 0.2; 3.7; 5.8; 0.3] in
  let m, _ = max_f (fun x -> x *. x) l1 in
  assert_equal ~cmp:(cmp_float ~epislon:0.0001) m 5.8

let t_particao_lista ctxt = 
  let h1 = Hash.create 10 in
  Hash.add h1 1 [1; 2];
  Hash.add h1 4 [3; 7];
  Hash.add h1 9 [2; 2];
  let l1 = particao_lista h1 in
  assert_bool "[1; 2] deve aparecer na lista convertida da tabela hash" (List.mem [1; 2] l1);
  assert_bool "[3; 7] deve aparecer na lista convertida da tabela hash" (List.mem [3; 7] l1);
  assert_bool "[2; 2] deve aparecer na lista convertida da tabela hash" (List.mem [2; 2] l1)

(* a suite de testes *)
let suite = 
  "suite" >:::
    [
      "calculo de entropia" >:: t_entropia;
      "max_f" >:: t_max_f;
      "partical_lista" >:: t_particao_lista
    ]

(* executor dos testes *)
let () = 
  run_test_tt_main suite

