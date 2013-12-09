set term post eps
set output 'metis_testA.eps'
set datafile separator "|"
set xlabel "Nr. of states"
set ylabel "Time (s)"
set autoscale
plot '../metis_testA_results.log' u 2:6 t 'Test A' w linespoints , \
     '../metis_testA_dupl_results.log' u 2:6 t 'Test A with duplication' w linespoints
