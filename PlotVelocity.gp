set terminal gif animate size 1280,1280 delay 0

set xrange[-10:+10]
set yrange[-10:+10]

set output 'Velocity.gif'
#set pm3d map

#set dgrid3d 400,200
#set pm3d corners2color c2 map

do for [i=0:100000000] {
	plot 'Results/fields-'.(i*200).'.dat' using 1:2:3:4 with vectors  #filled lc palette lt 3 notitle 
}
