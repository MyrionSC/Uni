  # Gnuplot script file for plotting data in file "force.dat"
  # This file is called   force.p
  set   autoscale                        # scale axes automatically
  unset log                              # remove any log-scaling
  unset label                            # remove any previous labels
  set xtic auto                          # set xtics automatically
  set ytic auto                          # set ytics automatically
  set term qt title "delta vs step length"
  set title "delta vs step length"
  set clip two
  set xlabel "delta"
  set ylabel "step length"
plot \
 "delta-vs-len.dat" using 1:2 with points title "delta vs len"



# "moving-avg.dat" using ($1):($2) with linespoints title "moving average", \
# "peak.dat" using 1:2 with points ps 2 title "step peaks", \
# "valley.dat" using 1:2 with points ps 2 title "step valleys", \
# "step-detected.dat" using 1:(-10):(0):(30) with vectors nohead title "step detected"


 #"step-calculated.dat" using 1:(-10):(0):(30) with vectors nohead title "step calculated", \
 #"accel-sum.dat" using 1:2 with linespoints title "sum of accel data", \
