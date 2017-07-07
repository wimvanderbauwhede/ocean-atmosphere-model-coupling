my $OUT;
while(<>) {
    /#\s+(\d+)/ && do {
        open $OUT, '>','sstano_'.$1.'.dat';
   };
       print $OUT $_;
  } 
