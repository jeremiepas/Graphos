#!/usr/bin/perl
use strict;
use warnings;
use Data::Dumper;

sub greet {
    my ($name) = @_;
    return "hello, $name";
}

sub main {
    print greet("world"), "\n";
}

main();
