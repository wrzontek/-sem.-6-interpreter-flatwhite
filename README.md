# flatwhite
Interpreter języka FlatWhite, będącego skromną wersją języka Latte.
Przed interpretacją kodu sprawdza go typechecker.
Kompilacja za pomocą ```make```. 
Uruchamianie za pomocą ```./interpreter <ścieżka do pliku z kodem>``` lub ```/intepreter``` i podanie kodu na standardowym wejściu.

Odbiega od Latte brakiem operacji inkremntacji/dekrementacji, dodaniem zmiennych readonly, pętli for z readonly zmienną sterującą oraz koniecznością wywołania ```return``` w każdym wywołaniu funkcji.
