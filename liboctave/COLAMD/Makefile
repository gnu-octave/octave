colamd_example:  colamd_example.c colamd.c colamd.h
	cc -O -o colamd_example colamd_example.c colamd.c
	- ./colamd_example

clean:
	- rm *.o colamd_example
	- rm colamdmex.mex* symamdmex.mex*
	- rm colamdtestmex.mex* symamdtestmex.mex*

# Compiles the MATLAB-callable routines
matlab: colamdmex.c colamd.c colamd.h
	mex -O colamdmex.c colamd.c
	mex -O symamdmex.c colamd.c

# Compiles the extensive test code
test: matlab colamdmex.c colamd.c colamd.h
	mex -O colamdtestmex.c colamd.c
	mex -O symamdtestmex.c colamd.c

