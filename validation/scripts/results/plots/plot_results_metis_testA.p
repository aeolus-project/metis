set term post eps
set output 'metis-testA-plot.eps'
set datafile separator "|"
set xlabel "Nr. of states"
set ylabel "Time (s)"
set autoscale
set style line 1 lt 1 lw 3 pt 3 linecolor rgb "blue"
set style line 2 lt 1 lw 3 pt 3 linecolor rgb "red"
plot '../metis_testA_results.log' every ::1::48 u 2:6 t 'Test A' w linespoints ls 1 , \
     '../metis_testA_dupl_results.log' every ::1::15 u 2:6 t 'Test A+' w linespoints ls 2
