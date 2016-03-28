package main

var m16 = 0x0000ffff
var m8  = 0x00ff00ff
var m4  = 0x0f0f0f0f
var m2  = 0x33333333
var m1  = 0x55555555

func bitcount( x int ) int{
  x = (x & m1)  + ((x >> 1)  & m1)
  x = (x & m2)  + ((x >> 2)  & m2)
  x = (x & m4)  + ((x >> 4)  & m4)
  x = (x & m8)  + ((x >> 8)  & m8)
  x = (x & m16) + ((x >> 16) & m16)
  return x
}

func main () {
  println(bitcount(0x23456456))
  println(bitcount(0333333333))
  println(bitcount(0555555555))
}
