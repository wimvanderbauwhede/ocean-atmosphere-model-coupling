#!/usr/bin/perl
#
# see `test_tput.pl` for an overview of the codes
my $white = `tput setaf 15`;
my $darkred = `tput setaf 1`;
my $green = `tput setaf 2`;
my $yellow =  `tput setaf 3`;
my $blue =  `tput setaf 4`;
my $magenta = `tput setaf 5`;
my $cyan= `tput setaf 6`;
my $lightgrey= `tput setaf 7`;
my $grey =  `tput setaf 8`;
my $red = `tput setaf 9`;

while (<>) {
    chomp;
    if( /error/i ) {
        print "$red $_ $white\n";
    } elsif ( /warning/i ) {
        print "$darkred $_ $white\n";
    } elsif ( /\#\#/ ) {
        print "$green $_ $white\n";
    } elsif ( /scons:/ ) {
        print "$grey $_ $white\n";
    } elsif ( /^[A-Z]/ ) {
        print "$yellow $_ $white\n";
    } else { print "$_\n";
    }
}

