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

let p3 = { 
  id = 3;
  sobreviveu = false;
  classe = Primeira;
  nome = "Crawley, Patrick";
  gen = Some Masc;
  idade = 22.0;
  irmpar = 0;
  paisfilhos = 0;
  passagem = "A1";
  preco = 27.0;
  cabine = "B220";
  embarque = Some Queenstown
}

let p4 = { 
  id = 4;
  sobreviveu = false;
  classe = Primeira;
  nome = "Crawley, James";
  gen = Some Masc;
  idade = 51.0;
  irmpar = 0;
  paisfilhos = 0;
  passagem = "A1";
  preco = 36.0;
  cabine = "B220";
  embarque = Some Queenstown
}


let passageiros = [p1; p2; p3; p4]

let sobreviventes = List.filter (fun p -> p.sobreviveu) passageiros

let mortos = List.filter (fun p -> not p.sobreviveu) passageiros

(* testes de unidade *)
let t_entropia ctxt = 
  List.iter (assert_equal ~cmp:(cmp_float ~epsilon:0.001) 0.0)
            [entropia sobreviventes; entropia mortos]

let t_max_f ctxt = 
  let l1 = [1.1; 0.9; 2.4; 0.2; 3.7; 5.8; 0.3] in
  let mi, max = max_f (fun x -> x *. x) l1 in
  assert_equal mi 5

let t_remove_indice ctxt = 
  let l1 = [1; 2; 3; 4; 5; 6] in
  let l2 = [11] in
  assert_equal (remove_indice 0 l1) [2; 3; 4; 5; 6];
  assert_equal (remove_indice 1 l1) [1; 3; 4; 5; 6];
  assert_equal (remove_indice 4 l1) [1; 2; 3; 4; 6];
  assert_equal (remove_indice 5 l1) [1; 2; 3; 4; 5];
  assert_equal (remove_indice 0 l2) []

let t_particao ctxt = 
  let part_gen = particao teste_genero passageiros in
  let part_prec = particao teste_preco_4faixas passageiros in
  let homens = List.nth part_gen 1 in
  assert_bool "[p2] deve aparecer na particao por genero" (List.mem [p2] part_gen);
  assert_bool "p1 deve aparecer entre os homens" (List.mem p1 homens);
  assert_bool "p3 deve aparecer entre os homens" (List.mem p3 homens);
  assert_equal (List.length part_prec) 4

(* Induz uma arvore apenas com exemplos positivos *)
let id3_pos ctxt = 
  let arvore = id3 sobreviventes [teste_genero] false in
  assert_equal arvore (Result true)

(* Induz uma arvore apenas com exemplos negativos *)
let id3_neg ctxt = 
  let arvore = id3 mortos [teste_genero] false in
  assert_equal arvore (Result false)

(* a suite de testes *)
let suite = 
  "suite" >:::
    [
      "calculo de entropia" >:: t_entropia;
      "max_f" >:: t_max_f;
      "particao_indice" >:: t_remove_indice;
      "particao" >:: t_particao;
      "id3 sobreviventes" >:: id3_pos;
      "id3 mortos" >:: id3_neg
    ]

(* executor dos testes *)
let () = 
  run_test_tt_main suite

