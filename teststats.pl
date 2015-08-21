#!/usr/bin/perl

use strict;
use warnings;

my @ports = (0,
             21512, 21512, 21512, 21512, 21512,
             22522, 22522, 22522, 22522, 22522 );
my @counts= (0,
             0, 0, 0, 0, 0,
             0, 0, 0, 0, 0);
my @gauges= (0,
             0, 0, 0, 0, 0,
             0, 0, 0, 0, 0);
my @consts= (0,
             1, 1, 1, 1, 1,
             2, 2, 2, 2, 2);


while(1) {
  my $i = 0;
  my $date = scalar (localtime(time()));
  foreach my $n (1, 2, 3, 4, 5, 6, 7, 8 ,9, 10) {
    $i++;
    my $port = $ports[$n];
    my $const = $consts[$n];
    my $host = "host_$n"."_".$port;
    $counts[$n] += $n * 10 * 60;
    $gauges[$n] = $n * 20;
    print "$date\t$port\t$host\tfoo\t$counts[$n]\tbar\t$gauges[$n]\n";
    `mondemand-tool -o lwes::172.16.101.128:$port -c host:$host -s counter:foo:$counts[$n] -s gauge:bar:$gauges[$n]`;
  }
  foreach my $l ([1,2,3,4,5], [6,7,8,9,10]) {
    my %s = ('foo' => \@counts, 'bar' => \@gauges);
    foreach my $m (keys %s) {
      my @data = @{$s{$m}};
      my $min = 5000000000;
      my $max = -5000000000;
      my $count = 0;
      my $sum = 0;
      my $port;
      foreach my $n (@{$l}) {
        $min = $data[$n] < $min ? $data[$n] : $min;
        $max = $data[$n] > $max ? $data[$n] : $max;
        $count++;
        $sum += $data[$n];
        $port = $ports[$n];
      }
      print "$date\t$port\t$m\tmin\t$min\n";
      print "$date\t$port\t$m\tmax\t$max\n";
      print "$date\t$port\t$m\tcount\t$count\n";
      print "$date\t$port\t$m\tsum\t$sum\n";
    }
  }

  foreach my $n (1,2,3,4,5,6,7,8,9,10) {
    $gauges[$n] = 0;
  }

  sleep 60
}
