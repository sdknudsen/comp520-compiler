#!/bin/sh

pad=$(printf '%0.1s' "."{1..80})
GOC=../src/main.native

echo "== Testing $1 ================================"

case $1 in
"lex"|"parse")
files=$(ls $1/*.valid $1/*.invalid)

;;
"pretty")
files=$(ls $1/*.go)

;;
esac

for f in $files; do
  printf '%s ' $f
  printf '%*.*s' 0 $((80 - ${#f} - 10)) "$pad"
  fname=$(echo $f | cut -f 1 -d '.')
  ext="${f##*.}" 

  expected=0

  if [ $1 = "pretty" ]
  then
      $GOC pretty < $f > "$fname.pretty"
      $GOC pretty "$fname.pretty" |  diff $fname.pretty - > /dev/null 2>&1
  else
    if [ -f "$fname.expected" ]
    then
      $GOC $1 < $f | diff $fname.expected - > /dev/null 2>&1
    else
      if [ $ext = "invalid" ]
      then
        expected=1
      fi
      $GOC $1 < $f > /dev/null 2>&1
    fi
  fi

  result=$?
  if [ $result -eq $expected ] # 0 or 1
  then
    echo -e " [\e[32mpassed\e[39m]"
  elif [ $result -eq $((1 - $expected)) ] # 0 or 1
  then
    echo -e " [\e[31mfailed\e[39m]"
  else
    echo "There was something wrong with the diff command"
  fi
done


