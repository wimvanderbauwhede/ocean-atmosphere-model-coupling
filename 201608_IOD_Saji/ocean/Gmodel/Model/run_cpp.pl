#!/usr/bin/env perl
use v5.22;
use warnings;
use strict;

use Cwd;
my $wd=cwd();

my $VV=0;

# SYS_Linux SYS_UNIX CODE_ASCII CODE_IEEE CODE_ENDIAN
my @macros=qw( );

my @srcdirs=qw(.);

my @includes = map {"-I$wd/".$_}  @srcdirs;
my $includestr = join(' ',@includes);

my @includes_l1 = map {"-I$wd/../".$_}  @srcdirs;
my $includestr_l1 = join(' ',@includes);

my @defines = map {"-D".$_}  @macros;
my $definestr = join(' ',@defines);

if (not -d "$wd/../PostCPP") {
    mkdir "$wd/../PostCPP";
}

my @srcs = glob("*.F");
for my $src (@srcs) {
    my $srcf =$src;
    $srcf=~s/F$/f/;
    say("cpp -P  $includestr $definestr -Wno-extra-tokens $src > $wd/../PostCPP/$srcf") if $VV;
    system("cpp -P  $includestr $definestr -Wno-extra-tokens $src > $wd/../PostCPP/$srcf");
}

for my $srcdir (@srcdirs) {
    if (not -d "$wd/../PostCPP/$srcdir") {
        system("mkdir -p $wd/../PostCPP/$srcdir");
    }
    chdir "$wd/$srcdir";
    my @srcs = glob("*.F");
    for my $src (@srcs) {
        my $srcf =$src;
        $srcf=~s/F$/f/;
        say("cpp -P  $includestr_l1 $definestr -Wno-extra-tokens $src > $wd/../PostCPP/$srcdir/$srcf") if $VV;
        system("cpp -P  $includestr_l1 $definestr -Wno-extra-tokens $src > $wd/../PostCPP/$srcdir/$srcf");
    }
    chdir $wd;
}
