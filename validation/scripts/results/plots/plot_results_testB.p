set term post eps
set output 'testB-plot.eps'
set datafile separator "|"
set xlabel "Nr. of components"
set ylabel "Time (s)"
set autoscale
set style line 1 lt 1 lw 3 pt 3 linecolor rgb "blue"
set style line 2 lt 1 lw 3 pt 3 linecolor rgb "red"
set style line 3 lt 1 lw 3 pt 3 linecolor rgb "green"
plot '../metis_testB_results.log' every ::1::22 u 2:6 t 'Metis' w linespoints ls 1, \
     '../mp_testB_results.log' every ::1::3 u 2:6 t 'Madagascar-p' w linespoints ls 2, \
     '../modified_metricff_testB_results.log' u 2:6 t 'Metric-FF' w linespoints ls 3
