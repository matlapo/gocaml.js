package main

// C represents a complex number
type C struct {
	r float64
	i float64
}

func c(r float64, i float64) C {
	var z C
	z.r = r
	z.i = i
	return z
}

func add(a C, b C) C {
	return c(a.r+b.r, a.i+b.i)
}

func mul(a C, b C) C {
	return c(a.r*b.r-a.i*b.i, a.i*b.r+a.r*b.i)
}

func normSquared(z C) float64 {
	return z.r*z.r + z.i*z.i
}

func isInSet(c C) bool {
	z := c
	for i := 0; i < 50; i++ {
		z = add(mul(z, z), c)
	}
	return normSquared(z) < 1.0
}

func main() {
	yMin := -1.0
	yMax := 1.0
	yScale := (yMax - yMin) / (15.0 - 1.0)

	xMin := -2.5
	xMax := 1.0
	xScale := (xMax - xMin) / (80.0 - 1.0)

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
