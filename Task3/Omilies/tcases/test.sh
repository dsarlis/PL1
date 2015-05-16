#!/bin/bash

for i in {1..10}
do
  echo "Running omilies "$i
  ./omilies tests/tcase$i.txt >in$i.txt
  if (diff in$i.txt me/out$i.txt) ; then
    echo "SAME"
  else
    echo "DIFFERENT"
  fi
done
