set term post eps
set output 'metis-testB-plot.eps'
set datafile separator "|"
set xlabel "Nr. of components"
set ylabel "Time (s)"
set autoscale
set style line 1 lt 1 lw 3 pt 3 linecolor rgb "blue"
set style line 2 lt 1 lw 3 pt 3 linecolor rgb "red"
plot '../metis_testB_results.log' every ::1::22 u 2:6 t 'Test B' w linespoints ls 1 , \
     '../metis_testB_dupl_results.log' every ::1::22 u 2:6 t 'Test B+' w linespoints ls 2
