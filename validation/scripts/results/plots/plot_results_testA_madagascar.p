set term post eps
set output 'prova.eps'
set datafile separator "|"
plot './testA_results_madagascar.log' u 2:3 w l
