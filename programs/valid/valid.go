package main

func f1() int {
	type t int
	var a1 num = 3
	var x1 int
	var x2 int
	var y1 = 42
	var y2 = 43
	var z1 int = 1
	var z2 int = 2
	if (y1 != y2) {
		x1 = 2
	} else if false {
		x1 = 3
	} else {
		y1 = 5
	}
	if y1 != y2 {
		y1 = 10
	}


	return z1
}
func f3()  {
	var x []int
	var y [3]int
	var z int
	var b bool
	var c int
	//var a float32

	z = 3 - 2 * 5 / 2 % 4
	b = 4 < 5 && (5 < 2) || ! false == true != true && 3 >= 3 || 1 <= 2
	c = 1 &^ 4 << 3 & 5 | ^ 7 >> 2
	c += 2
	c -= 4
	c *= 4
	c /= 4
	c %= 4
	c &= 4
	c |= 4
	c ^= 4
	c <<= 4
	c >>= 4;
	c ++
	c --
	c %= 3

  	//%=
  	//&=
  	//|=
  	//^=
  	//&^=
  	//<-
  	//!=
  	//:=

	return
}
func f4(a int, b int, c int) int {
	return a
}
func f5(a int, b int, c int)  {
	var x []int
	x = append(x, 1)
	var y [3]int
	y[0] = 1
	type point struct {
		x int
		y int
		z int
	}
	var p point
	p.x = 1
}
func f2()  {
	type num int
	type real float64
	type point struct {
		x int
		y int
		z int
	}
	return
}