#!/usr/bin/perl
use strict;
no strict qw( refs vars );
use warnings;
no warnings 'once';

my @colors=qw( 
        black 
        red 
        green 
        yellow 
        blue 
        magenta 
        cyan 
        lightgrey 
        grey 
        brightred 
        brightgreen 
        brightyellow 
        brightblue 
        brightmagenta 
        brightcyan 
        white 
);

my $i=0;
map {$$_ = calc_code($i++)} @colors;

while (<>) {
    chomp;
    if( /error/i ) {
        print "$brightred$_$white\n";
    } elsif ( /warning/i ) {
        print "$red$_$white\n";
    } elsif ( /\#/ ) {
        print "$green$_$white\n";
    } elsif ( /scons:/ ) {
        print "$grey$_$white\n";
    } elsif ( /^[A-Z]/ ) {
        print "$yellow$_$white\n";
    } else { print "$_\n";
    }
}

# Only valid for codes up to 15! Works on Linux and Mac
sub calc_code {(my $i)=@_;
    my @codes =  (27, 91 ,51+int($i / 8)*6,48+($i % 8), 109 );
    my $code_str = join('', map { chr($_) } @codes );
    return $code_str;
}

sub test_tput {
    for my $i (0..255) {
        my $code = `tput setaf $i`;
        my @chs = split('', $code);
        print $code;
        print "code $i\t";    
        map { print ' '.ord($_) } @chs;
        print "\n";
    }    
}

# uncomment `test_tput()` and run with
# ./colorise.pl ''
# for an overview of the codes
#test_tput();


