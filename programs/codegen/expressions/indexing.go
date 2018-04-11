package main

func main() {
  var a [][]int
  var b []int
  a = append(a, b)
  a[0] = append(a[0], 0)
  b = a[0]
  b = a[a[0][0]]
}
