  # Gnuplot script file for plotting data in file "force.dat"
  # This file is called   force.p
  set   autoscale                        # scale axes automatically
  unset log                              # remove any log-scaling
  unset label                            # remove any previous labels
  set xtic auto                          # set xtics automatically
  set ytic auto                          # set ytics automatically
  set term qt title "split"
  set title "accelerometer and steps over time"
  set clip two
  set xlabel "Time(ms)"
  set ylabel "accel readings"
plot "accel-split.dat" using 1:2 with lines, \
 "accel-split.dat" using 1:3 with lines, \
 "accel-split.dat" using 1:4 with lines, \
 "step.dat" using 1:(-10):(0):(40) with vectors nohead title "step detected"
