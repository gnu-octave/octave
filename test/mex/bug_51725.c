#include "mex.h"

void mexFunction (int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  if (nlhs > 0)
    plhs[0] = mxCreateDoubleMatrix (0, 0, mxREAL);

  if (nlhs > 2)
    plhs[2] = mxCreateDoubleMatrix (0, 0, mxREAL);
}
