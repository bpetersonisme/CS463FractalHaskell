CS 463 Project 3 

This is a command line program to generate "ASCII" fractal patterns. It has two possible outputs: a Mandelbrot set, or a Julia set. 

Mandelbrot "maxiters" "rows" "cols" "lowR" "lowI" "highR" "highI" "filename"

Julia "maxiters" "rows" "cols" "lowR" "lowI" "highR" "highI" "realC" "imaginaryC" "filename"

maxIters is the number of iterations of the fractal formula before the point is said to have "escaped" 

rows is the number of vertical rows that the set will have 

cols is the number of horizontal columns that the set will have 

lowR and lowI are the real and imaginary components of the complex number represented by the set's lower left corner 

highR and highI are the real and imaginary components of the complex number represented by the set's upper right corner 

realC and imaginary C are the real and imaginary components of the complex constant used in each iteration of the fractal function 

"filename" is the name of the file to which the fractal will be saved. I recommend using .txt