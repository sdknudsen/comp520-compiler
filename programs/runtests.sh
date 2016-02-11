#!/bin/sh

#should we compile here?
if [[ $1 = "v" ]] || [[ $1 = "valid" ]]; then
    folder="valid"
elif [[ $1 = "i" ]] || [[ $1 = "invalid" ]]; then
    folder="invalid"
else 
    echo "usage: bash runtests [v|i|valid|invalid]";
    exit
fi
ls $folder > testProgs.txt

while [[ -s "testProgs.txt" ]]
do
curr=$(head -n 1 testProgs.txt)
echo $curr
../src/main.native $folder/$curr
echo ""
sed -i -e "1d" testProgs.txt
done
rm testProgs.txt*
