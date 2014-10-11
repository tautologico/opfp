(* 
 *
 * genero.ml
 * Classificador baseado em genero
 *
 * Andrei de A. Formiga, 2014-10-02
 *
 *)

let () = 
  Printf.printf "Lendo arquivo de teste e escrevendo genero.csv\n";
  Titanic.classifica_teste_por_genero "data/test.csv" "genero.csv"


