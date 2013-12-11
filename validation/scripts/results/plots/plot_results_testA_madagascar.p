set term post eps
set output 'testA.eps'
set datafile separator "|"
set xlabel "Nr. of states"
set ylabel "Time (s)"
set autoscale
plot '../metis_testA_results.log' every ::1::20 u 2:6 t 'Metis' w linespoints , \
     '../testA_results_madagascar.log' u 2:6 t 'Madagascar-p' w linespoints , \
     '../testA_results_metricff.log' u 2:6 t 'Metric-FF' w linespoints , \
