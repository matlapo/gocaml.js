//~0
//~1
//~2
//~
//~Not stuck in the infinite loop !

package main

func main() {
	for i := 0; i < 3; i++ {
		println(i)
	}
	println()

	for {
		break
	}
	println("Not stuck in the infinite loop !")
}
