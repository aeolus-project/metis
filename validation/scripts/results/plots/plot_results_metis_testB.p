set term post eps
set output 'metis_testB.eps'
set datafile separator "|"
set xlabel "Nr. of components"
set ylabel "Time (s)"
set autoscale
plot '../metis_testB_results.log' u 2:6 t 'Test B' w linespoints , \
     '../metis_testB_dupl_results.log' u 2:6 t 'Test B with duplication' w linespoints
