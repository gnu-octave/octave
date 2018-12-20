#include "mex.h"

static const char* field_names[] = {"field"};

void mexFunction (int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  mxArray *tmp_val;

  plhs[0] = mxCreateStructMatrix (1, 1, 1, field_names);

  mxSetFieldByNumber (plhs[0], 0, 0, NULL);

  tmp_val = mxGetFieldByNumber (plhs[0], 0, 0);

  if (tmp_val)
    mexErrMsgTxt ("struct elements set to NULL should be NULL internally");

  /* But in the interpreter, they should appear as [].  */
}
