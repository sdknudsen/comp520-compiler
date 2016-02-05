# This can be run with $bash run foo.min
# Alternatively, ./run.native foo.min works (as indicated in the README)
# Sorry you can't just $run foo.min, the only way I know how to do that is by creating an alias
# after running this once, I believe $run foo.min will work

name=$1
./run.native $name

alias run='./run.native '
