function colamd_make
% COLAMD_MAKE:  compiles COLAMD Version 2.4 for MATLAB

mex -O colamdmex.c colamd.c colamd_global.c
mex -O symamdmex.c colamd.c colamd_global.c
fprintf ('COLAMD successfully compiled.\n') ;
