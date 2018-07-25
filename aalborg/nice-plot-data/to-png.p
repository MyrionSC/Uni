  # Gnuplot script file for plotting data in file "force.dat"
  # This file is called   force.p
  set   autoscale                        # scale axes automatically
  unset log                              # remove any log-scaling
  unset label                            # remove any previous labels
  set xtic auto                          # set xtics automatically
  set ytic auto                          # set ytics automatically
  set term png large size 1400,1400 enhanced font "Helvetica,20"
  set output 'output.png'
  #set term qt
  set title "accelerometer readings and average moving over time"
  set clip two
  set xlabel "Time from start (ms)"
  set ylabel "Acceleration (m/s^2)"
plot "accel-sum.dat" using 1:2 with lines lw 3 title "accelerometer sum", \
 "moving-avg.dat" using ($1):($2 + 8) with lines lw 3 title "moving average"
# "step.dat" using 1:(-10):(0):(40) with vectors nohead title "step detected"
# "peak.dat" using 1:2 with points ps 2 title "step peaks", \
# "valley.dat" using 1:2 with points ps 2 title "step valleys", \

