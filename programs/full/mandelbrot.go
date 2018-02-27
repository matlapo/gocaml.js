package main

// C represents a complex number
type C struct {
	r float64
	i float64
}

func c(r float64, i float64) {
	var z C
	z.r = r
	z.i = i
	return z
}

func add(a C, b C) {
	return c(a.r+b.r, a.i+b.i)
}

func mul(a C, b C) {
	return c(a.r*b.r-a.i*b.i, a.i*b.r+a.r*b.i)
}

func normSquared(z C) {
	return a.r*a.r + a.i*a.i
}

func isInSet(c C) {
	z := c
	for i := 0; i < 50; i++ {
		z = add(mul(z, z), c)
	}
	return normSquared(z) < 1
}

func main() {
	yMin := -1.0
	yMax := 1.0
	yScale := (yMax - yMin) / (15 - 1)

	xMin := -2.5
	xMax := 1.0
	xScale := (xMax - xMin) / (80 - 1)

	for y := yMax; y >= yMin; y -= yScale {
		for x := xMin; x <= xMax; x += xScale {
			if isInSet(c(x, y)) {
				print("*")
			} else {
				print(" ")
			}
		}
	}
}
