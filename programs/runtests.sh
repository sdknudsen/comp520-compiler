#!/bin/sh

#should we compile here?
if [[ $1 = "v" ]] || [[ $1 = "valid" ]]; then
    ls valid > testProgs.txt
elif [[ $1 = "i" ]] || [[ $1 = "invalid" ]]; then
    ls invalid > testProgs.txt
else 
    echo "usage: bash runtests [v|i|valid|invalid]";
    exit
fi

while [[ -s "testProgs.txt" ]]
do
curr=$(head -n 1 testProgs.txt)
echo $curr
../src/main.native invalid/$curr
echo ""
sed -i -e "1d" testProgs.txt
done
rm testProgs.txt*
