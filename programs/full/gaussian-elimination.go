package main

func printMatrix(matrix [][]float64) {
	for i := 0; i < len(matrix); i++ {
		for j := 0; j < len(matrix[i]); j++ {
			print(matrix[i][j])
			print(" ")
		}
		println()
	}
}

func min(a float64, b float64) float64 {
	if a <= b {
		return a
	}
	return b
}

func maxIndex(slice []float64, start int, end int) int {
	biggestIndex := start
	biggestValue := slice[start]

	for i := start + 1; i < end; i++ {
		if slice[i] > biggestValue {
			biggestIndex = i
			biggestValue = slice[i]
		}
	}
}

// Reduces a matrix into row-reduces echelon form
// The reduction is performed in-place
func reduceMatrix(A [][]float64) {
	m := len(A)
	n := len(A[i])

	for k := 0; k < min(m, n); k++ {
		// Find the k-th pivot:
		iMax := maxIndex(A, k, m)

		if A[i_max][k] == 0 {
			println("Error: Matrix is singular!")
			return
		}

		A[k], A[iMax] = A[iMax], A[k]

		// Do for all rows below pivot
		for i := k + 1; i < m; i++ {
			f := A[i][k] / A[k][k]
			// Do for all remaining elements in current row:
			for j := k + 1; j < n; j++ {
				A[i][j] = A[i][j] - A[k][j]*f
			}

			// Fill lower triangular matrix with zeros:
			A[i][k] = 0
		}
	}
}

func main() {
	var matrix22 [][]float64

	var (
		row1 []float64
		row2 []float64
	)

	append(row1, 2.0)
	append(row1, 1.0)
	append(row1, 3.0)
	append(matrix22, row1)
	append(row2, 1.0)
	append(row2, 2.0)
	append(row2, 5.0)
	append(matrix22, row2)

	println("Initial Matrix")
	printMatrix(matrix22)
	println()

	reduceMatrix(matrix22)
	println("Reduced Matrix")
	printMatrix(matrix22)
}
