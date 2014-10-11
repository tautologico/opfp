(* 
 *
 * id3gencls.ml
 * Constroi uma arvore de decisao usando ID3 e testando genero e classe.
 *
 * Andrei de A. Formiga, 2014-10-04
 *
 *)

let () = 
  Printf.printf "Lendo dados de treinamento e construindo arvore\n";
  let d_treino = Titanic.ler_dados_treino "data/train.csv" in
  let arvore = Titanic.id3 d_treino [Titanic.testa_classe; Titanic.testa_genero] in
  Printf.printf "Lendo dados de teste e escrevendo gencls.csv\n";
  let d_teste = Titanic.ler_dados_teste "data/test.csv" in
  let resultado = Titanic.aplica_arvore_dados arvore d_teste in
  Titanic.escreve_resultado resultado "gencls.csv"
