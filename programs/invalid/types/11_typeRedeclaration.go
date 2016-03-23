package main

type num int

func f(x num) num {
  type num float64
  var y num;
  return x % y
}

func main() {
  var x num
  print(f(x))
}
