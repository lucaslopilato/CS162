#!/bin/bash

for file in basic_tests/*; do
  answer=`scala -cp scala-parser-combinators.jar:. Interpreter $file | tail -n 1`
  expected=`head -n 1 $file | sed 's/-- //'`
  if [ "$answer" != "$expected" ]; then
    echo "$file:"
    echo "  Expected: $expected"
    echo "  Given: $answer"
  fi
done
