set xlabel 'Feedback Iteration'
set ylabel 'Speedup compared to sequential'

set grid

set yrange [1:1.5]

set xticks 1

plot "4core.tbl" using 1:2 with linespoints title "4 cores", \
     "8core.tbl" using 1:2 with linespoints title "8 cores"
     "16core.tbl" using 1:2 with linespoints title "16 cores"
