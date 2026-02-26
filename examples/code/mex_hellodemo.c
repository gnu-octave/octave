#include "mex.h"

void
mexFunction (int nlhs, mxArray *plhs[],
             int nrhs, const mxArray *prhs[])
{
  mexPrintf ("Hello, World!\n");

  mexPrintf ("I have %d inputs and %d outputs\n", nrhs, nlhs);

  /* Return empty matrices for any outputs */
  int i;
  for (i = 0; i < nlhs; i++)
    plhs[i] = mxCreateDoubleMatrix (0, 0, mxREAL);
}
