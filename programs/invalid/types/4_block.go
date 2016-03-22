package main

func main() {
  var a int = 1
  {
    var b int = 2
    a = 3
  }
  b = 4
}
