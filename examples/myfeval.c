#include "mex.h"

void
mexFunction (int nlhs, mxArray* plhs[], int nrhs, 
             const mxArray* prhs[])
{
  char *str;

  mexPrintf ("Hello, World!\n");

  mexPrintf ("I have %d inputs and %d outputs\n", nrhs,
             nlhs);

  if (nrhs < 1 || ! mxIsString (prhs[0])) 
    mexErrMsgTxt ("function name expected");

  str = mxArrayToString (prhs[0]);

  mexPrintf ("I'm going to call the function %s\n", str);

  mexCallMATLAB (nlhs, plhs, nrhs-1, prhs+1, str);

  mxFree (str);
}
