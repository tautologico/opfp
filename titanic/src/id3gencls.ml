(* 
 *
 * id3gencls.ml
 * Constroi uma arvore de decisao usando ID3 com testes para genero e classe.
 * Classifica os dados de teste usando a arvore construida. 
 *
 * Andrei de A. Formiga, 2014-10-04
 *
 *)

let testes = [Titanic.testa_classe; Titanic.testa_genero]

let () = 
  Printf.printf "Lendo dados de treinamento e construindo arvore\n";
  let arvore = Titanic.arvore_testes "data/train.csv" testes in
  Printf.printf "Lendo dados de teste e escrevendo gencls.csv\n";
  Titanic.classifica_teste_arvore arvore "data/test.csv" "gencls.csv"
