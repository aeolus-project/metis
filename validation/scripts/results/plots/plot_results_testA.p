set term post eps
set output 'testA-plot.eps'
set datafile separator "|"
set xlabel "Nr. of states"
set ylabel "Time (s)"
set autoscale
set style line 1 lt 1 lw 3 pt 3 linecolor rgb "blue"
set style line 2 lt 1 lw 3 pt 3 linecolor rgb "red"
set style line 3 lt 1 lw 3 pt 3 linecolor rgb "green"
plot '../metis_testA_results.log' every ::1::48 u 2:6 t 'Metis' w linespoints ls 1, \
     '../mp_testA_results.log' every ::1::13 u 2:6 t 'Madagascar-p' w linespoints ls 2, \
     '../metricff_testA_results.log' every ::1::17 u 2:6 t 'Metric-FF' w linespoints ls 3
