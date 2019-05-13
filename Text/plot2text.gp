set terminal eps
set output '1.eps'
set datafile separator ','
plot '../Results/fields-1800.csv' u 1:4 with l ti 'Rho' lw 5, \
     '../Results/fields-1800.csv' u 1:10 w l   ti 'B_y' lw 5
