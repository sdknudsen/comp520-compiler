// This is a line comment

package main

func f1() int {
  var (
    x1, x2 int
    y1, y2 = 42, 43
    z1, z2 int = 1, 2
  )
  return z1
}

func f2() {
  type (
    num int
    real float64
    point struct {
      x, y, z int
    }
  )
  return
}

func f3() {
  var x []int
  var y [3]int
  return
}

func f4(a, b, c int) int {
  return a
}
