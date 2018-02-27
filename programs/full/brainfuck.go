package main

func brainfuck(program string) {
	var buffer [1000]rune
	programPointer := 0
	dataPointer := 0

	for programPointer < len(message) {
		switch program[programPointer] {
		case '>':
			dataPointer++
		case '<':
			dataPointer--
		case '+':
			buffer[dataPointer]--
		case '-':
			buffer[dataPointer]--
		case '.':
			s := ""
			s += buffer[dataPointer]
			print(s)
		case ',':
			println("Error: unsupported command: ',' at position", programPointer)
		case '[':
			if buffer[dataPointer] == 0 {
				for n := 1; n > 0; programPointer++ {
					if program[programPointer] == '[' {
						n++
					} else if program[programPointer] == ']' {
						n--
					}
				}
			}
		case ']':
			if buffer[dataPointer] != 0 {
				for n := 1; n > 0; programPointer++ {
					if program[programPointer] == '[' {
						n--
					} else if program[programPointer] == ']' {
						n++
					}
				}
			}
		}

		programPointer++
	}
}

func main() {
	// Hello World taken from https://en.wikipedia.org/wiki/Brainfuck#Hello_World!
	brainfuck("++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")
}
