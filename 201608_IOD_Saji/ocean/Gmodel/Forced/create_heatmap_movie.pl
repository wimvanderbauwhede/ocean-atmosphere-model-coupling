#!/usr/bin/perl
use warnings;
use strict;


#perl split_sstano.pl < sstano.dat 
open my $IN, '<', 'sstano.dat';
my $OUT;
while(my $line = <$IN>) {
    $line=~/#\s+(\d+)/ && do {
        open $OUT, '>','sstano_'.$1.'.dat';
   };
       print $OUT $line;
  } 
close $OUT;
close $IN;
my $k=0;
for my $i (66 .. 99) {
for my $j (1 ..12) {
    $k++;
my $jj= $j<10 ? "0$j": "$j";

my $kkk= $k< 10? '00'.$k: $k<100 ? '0'.$k : $k;
    open my $GP, '>', 'gen_sstano_heatmap.gnuplot';

    print $GP <<"ENDGP";
set term png            
set output "sstano_$kkk.png"
set cbrange [-3:3]
plot 'sstano_19$i$jj.dat' matrix with image

ENDGP

    close $GP;
    system("gnuplot gen_sstano_heatmap.gnuplot");
}
}

system('rm sstano_heatmap.mp4');
system('ffmpeg -framerate 6 -i sstano_%03d.png -vf format=yuv420p sstano_heatmap.mp4');
system('rm *.png');
system('rm sstano_*.dat');

