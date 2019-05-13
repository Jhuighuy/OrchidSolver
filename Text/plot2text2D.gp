set terminal eps
set output '2.eps'
set datafile separator ','
set isosamples 10000
set samples 10000
set pm3d map
set dgrid 256,256
splot '../Results-OT-HLLC/fields-10000.csv' u 1:2:4
