Compilação e Interpretação
====

Este diretório contém os exemplos de código do capítulo 6 do livro,
"Exemplo: compilador e interpretador".

O uso do OASIS para organizar o projeto e da biblioteca ounit para
testes de unidade é mostrado no capítulo 13 do livro,
"Organização de projeto e testes".

## Testes

Para executar os testes é preciso ter a biblioteca ounit instalada, o que
pode ser feito facilmente usando OPAM.

Para compilar os testes é preciso configurar com essa opção, e depois
compilar o projeto normalmente:

````
ocaml setup.ml -configure --enable-tests
ocaml setup.ml -build
````

Depois disso, a bateria de testes pode ser executada executando o arquivo
test.native ou usando o setup.ml:

````
ocaml setup.ml -test
````
