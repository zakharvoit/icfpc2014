#!/usr/bin/env perl

use warnings;
use strict;

my %labels;
my $text;
my $cnt = 0;

while (<>) {
    s/;.*//g;
    chomp;
    next if $_ eq "";
    if (/([a-zA-Z]+):/) {
        $labels{$1} = $cnt;
        s/[a-zA-Z]+:\s*//g;
    }
    $text .= $_ . "\n";
    ++$cnt;
}

for (keys %labels) {
    $text =~ s/$_/$labels{$_}/g;
}

print $text;
