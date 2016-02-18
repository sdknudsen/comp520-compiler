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
echo $curr
../src/main.native $1 $folder/$curr
echo ""
sed -i -e "1d" _test_progs.txt
done
rm _test_progs.txt

