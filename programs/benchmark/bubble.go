//~19990
//~19991
//~19992
//~19993
//~19994
//~19995
//~19996
//~19997
//~19998
//~19999

package main

// We love bubble sort

var arrayCount = 20000;

var array [20000]int

func bubble(array [20000]int) [20000]int {
	var hasChanged=true
  for ; hasChanged == true; {
		hasChanged = false
		var i=0
    for ; i < arrayCount - 1; i++ {
      if array[i] > array[i+1] {
        array[i], array[i+1] = array[i+1], array[i]
        hasChanged = true
      }
    }
  }
  return array
}

func main() {
  for i := 0; i < arrayCount; i++ {
    array[arrayCount - 1 - i] = i
  }
	var t = bubble(array)
  for i := arrayCount-10; i < arrayCount; i++ {
    println(t[i])
  }
}
