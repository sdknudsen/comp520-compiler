/*
Author: Cheuk Chuen Siow
Description: Example program that takes 2 sequences and check
             whether they overlap based on coordinates.
*/

package main

type (
  sequence struct {
    start, end int
  }
)

func checkOverlap(seqA, seqB sequence) bool {
  var isOverlap bool = false

  if seqA.end >= seqB.start && seqA.start <= seqB.end {
    isOverlap = true
  } else {
    isOverlap = false
  }

  return isOverlap
}

func main() {
  var (
    seq1 sequence
    seq2 sequence
  )
  
  seq1.start = 12052
  seq1.end   = 23179
  seq2.start = 15830
  seq2.end   = 31694
  
  if isOverlap := checkOverlap(seq1, seq2); isOverlap {
    println("Overlapping sequences.")
  } else {
    println("Non-overlapping sequences.")
  }
}
