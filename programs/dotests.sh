#!/bin/sh

pad=$(printf '%0.1s' " "{1..40})
GOC=../src/main.native

echo "== Testing $1 ================================"

for f in $(ls $1/*.valid $1/*.invalid); do
  printf ' %s' $f
  printf '%*.*s' 0 $((40 - ${#f})) "$pad"
  fname=$(echo $f | cut -f 1 -d '.')
  ext="${f##*.}" 

  expected=0
  if [ -f "$fname.expected" ]
  then
    ../src/main.native lex < $f | diff $fname.expected - > /dev/null 2>&1
  else
    if [ $ext = "invalid" ]
    then
      expected=1
    fi
    ../src/main.native lex < $f > /dev/null 2>&1
  fi

  result=$?
  if [ $result -eq $expected ]
  then
    echo -e "[\e[32mpassed\e[39m]"
  elif [ $? -eq $((1 - $expected)) ]
  then
    echo -e "[\e[31mfailed\e[39m]"
  else
    echo "There was something wrong with the diff command"
  fi
done


