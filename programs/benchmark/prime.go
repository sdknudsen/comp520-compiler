package main

func prime(n int) bool {
  if n <= 1 {
    return false
  } else if n <= 3 {
    return true
  } else {
    for i := 2; i < n; i++ {
      if (n%i) == 0 {
        return false
      }
    }
    return true
  }
}

func main() {
  n, count := 300000, 0
  for i := 1; i <= n; i++ {
    if prime(i) {
      count++
    }
  }
  println(count)
}
