//~1
//~2
//~3

package main

func main() {
	switch 1 {
	case 1:
		println("1")
	default:
		println("fail2")
	}

	switch "a" {
	case "a":
	case "b":
		println("2")
	default:
		println("fail2")
	}

	switch {
	case 1 > 2:
		println("fail3")
	default:
		println("3")
	}
}
