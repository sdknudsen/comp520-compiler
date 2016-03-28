package main

func fib(n int) int {
  switch n {
  case 0,1:
    return 1
  default:
    return fib(n-1)+fib(n-2)
  }
}

func main() {
  println(fib(48))
}
