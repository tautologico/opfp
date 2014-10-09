open OUnit2
open Titanic

(* passageiros *)
let p1 = { 
  id = 1;
  sobreviveu = false;
  classe = Terceira;
  nome = "Jack";
  genero = Some Masc;
  idade = 20;
  irmpar = 0;
  paisfilhos = 0;
  passagem = "Clandestino";
  preco = 0.0;
  cabine = "C1";
  embarque = Southampton
}

let p2 = { 
  id = 2;
  sobreviveu = true;
  classe = Primeira;
  nome = "Rose";
  genero = Some Fem;
  idade = 21;
  irmpar = 0;
  paisfilhos = 0;
  passagem = "A2";
  preco = 10.0;
  cabine = "A1";
  embarque = Cherbourg
}

(* Patrick e fulano de Downton Abbey *)
