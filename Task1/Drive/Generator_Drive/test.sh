#!/bin/bash

mkdir -p tests

for size in "small 100" "medium 1000" "large 10000" 
do
  set -- $size
  for i in {1..10}
  do
    for limit in 30 40 50 
    do
      echo "Testing: " $limit $limit $1 $i
      ./drive $limit $limit tests/$1$i.in >tests/$1$i$limit.out
      if (diff tests/$1$i$limit.out lemonidas/$1$i$limit.out)
      then
        echo "Same!"
      else 
        echo "Different!"
      fi
    done
  done
done
