set terminal postscript  eps enhanced color font 'Helvetica,12'
set size 1,1
set term post portrait color "Times-Roman" 12
  set output '| ps2pdf - SUM.pdf'
  set timefmt "%Y/%m"
  set xdata time
  set format x "%Y/%m"
  set xrange ["2014/01":"2015/09"]   
  set yrange [0 :30]
  set y2range [0 :500]
  set y2tics 20 nomirror tc lt 2
  set y2label 'no of comments' tc lt 2
  set ylabel 'no of tickets' tc lt 1
  set ytics nomirror
  set title "Monthly statistcs for all Riak tickets created/solved/commented by ESL"
  plot "monthly_stats.dat" using 5:(stringcolumn(2) eq "SUM"? $6:1/0) title "SUM created" lc rgb "blue" axis x1y1 with lines, \
  "" using 5:(stringcolumn(2) eq "SUM"? $7:1/0) title "SUM solved" lc rgb "red"  axis x1y1 with lines, \
  "" using 5:(stringcolumn(2) eq "SUM"? $8:1/0) title "SUM commented" lc rgb "green" axis x1y2 with lines