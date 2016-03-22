package main

func f1() int {
  x := 1
  return x
}

func f2() string {
  y := "Hello world"
  return y
}

func main() {
  switch x, y := f1(), f2(); {
    case x < 0: print(-y)
    default: print(y)
  }
}
