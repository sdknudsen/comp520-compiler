#!/bin/sh

#compile?
if [[ $2 = "v" ]] || [[ $2 = "valid" ]]; then
    folder="valid"
elif [[ $2 = "i" ]] || [[ $2 = "invalid" ]]; then
    folder="invalid"
else 
    echo "usage: bash runtests [lex|parse|pretty|typecheck|compile] [v|i|valid|invalid]";
    exit
fi
ls $folder > _test_progs.txt

while [[ -s "_test_progs.txt" ]]
do
curr=$(head -n 1 _test_progs.txt)

../src/main.native $1 $folder/$curr &> _out.txt
~cs520/golitec $1 $folder/$curr &> _gold.txt

_=$(diff _out.txt _gold.txt) && echo "*	"$curr || echo "!	"$curr
sed -i -e "1d" _test_progs.txt
done

rm _out.txt
rm _gold.txt
rm _test_progs.txt

