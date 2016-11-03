#!/usr/bin/env perl

use strict;
use warnings;

use v5.14;

use File::Slurp::Tiny qw(read_lines);

my $glob = shift || "op-measures-*.csv";
my @files =  glob $glob;

my %ranks;
for my $file_name ( @files ) {
    my @data = read_lines( $file_name, chomp => 1 );
    
    if ( !@data ) {
	die "Can't read $file_name"
    }

    my %by_length;
    
    my @columns = split( /\s*,\s*/, $data[0] );
    for my $d ( @data[1..$#data] ) {
	next if !$d;
	my @this_data = split(/\s*,\s*/,$d );
	if ( !$this_data[1] ) {
	  warn "Something is wrong with ", @this_data;
	}
	push @{$by_length{$this_data[1]}}, [$this_data[0], $this_data[2]];
    }
	
    for my $l ( keys %by_length ) {
	my @sorted = sort { $a->[1] <=> $b->[1] } @{$by_length{$l}};
	for ( my $i = 0; $i <= $#sorted; $i++ ) {
	    push @{$ranks{$sorted[$i]->[0]}}, $i+1;
	}
    }
    
}


my %avg_ranks;
for my $l ( keys %ranks ) {
  my $sum = 0;
  map( $sum += $_, @{$ranks{$l}});
  $avg_ranks{$l} = $sum/(scalar @{$ranks{$l}});
}

say "LanguageData,AvgRank";
for my $l (sort { $avg_ranks{$a} <=> $avg_ranks{$b} } keys %avg_ranks ) {
  say "$l,$avg_ranks{$l}";
}
