#include "mex.h"

// To be called with
//
//   single array
//   complex single array
//   double array
//   complex double array
//
// Returns copies of the input arrays to test the mxArray ->
// octave_value conversion.

#define MAKE_INTERLEAVED_COPY(TYPE, ID, CPLXTY, RHS, GET, LHS, SET)     \
  do                                                                    \
    {                                                                   \
      size_t i;                                                         \
      TYPE *data = GET (RHS);                                           \
      const mwSize *data_dims = mxGetDimensions (RHS);                  \
      mwSize data_ndims = mxGetNumberOfDimensions (RHS);                \
      size_t numel = mxGetNumberOfElements (RHS);                       \
      TYPE *data_copy = mxMalloc (numel * sizeof (TYPE));               \
      for (i = 0; i < numel; i++)                                       \
        data_copy[i] = data[i];                                         \
      LHS = mxCreateNumericMatrix (0, 0, ID, CPLXTY);                   \
      SET (LHS, data_copy);                                             \
      mxSetDimensions (LHS, data_dims, data_ndims);                     \
    }                                                                   \
  while (0)

#define MAKE_REAL_COPY(TYPE, ID, RHS, LHS)                      \
  do                                                            \
    {                                                           \
      size_t i;                                                 \
      TYPE *data = mxGetData (RHS);                             \
      const mwSize *data_dims = mxGetDimensions (RHS);          \
      mwSize data_ndims = mxGetNumberOfDimensions (RHS);        \
      size_t numel = mxGetNumberOfElements (RHS);               \
      TYPE *data_copy = mxMalloc (numel * sizeof (TYPE));       \
      for (i = 0; i < numel; i++)                               \
        data_copy[i] = data[i];                                 \
      LHS = mxCreateNumericMatrix (0, 0, ID, mxREAL);           \
      mxSetData (LHS, data_copy);                               \
      mxSetDimensions (LHS, data_dims, data_ndims);             \
    }                                                           \
  while (0)

#define MAKE_CPLX_COPY(TYPE, ID, RHS, LHS)                      \
  do                                                            \
    {                                                           \
      size_t i;                                                 \
      TYPE *real_data = mxGetData (RHS);                        \
      TYPE *imag_data = mxGetImagData (RHS);                    \
      const mwSize *data_dims = mxGetDimensions (RHS);          \
      mwSize data_ndims = mxGetNumberOfDimensions (RHS);        \
      size_t numel = mxGetNumberOfElements (RHS);               \
      TYPE *real_data_copy = mxMalloc (numel * sizeof (TYPE));  \
      TYPE *imag_data_copy = mxMalloc (numel * sizeof (TYPE));  \
      for (i = 0; i < numel; i++)                               \
        {                                                       \
          real_data_copy[i] = real_data[i];                     \
          imag_data_copy[i] = imag_data[i];                     \
        }                                                       \
      LHS = mxCreateNumericMatrix (0, 0, ID, mxCOMPLEX);        \
      mxSetData (LHS, real_data_copy);                          \
      mxSetImagData (LHS, imag_data_copy);                      \
      mxSetDimensions (LHS, data_dims, data_ndims);             \
    }                                                           \
  while (0)

void
mexFunction (int nlhs, mxArray *plhs[],
             int nrhs, const mxArray *prhs[])
{
  if (nrhs != 4 || nlhs != 4)
    mexErrMsgTxt ("invalid arguments");

#if MX_HAS_INTERLEAVED_COMPLEX

  MAKE_INTERLEAVED_COPY (mxSingle, mxSINGLE_CLASS, mxREAL, prhs[0],
                         mxGetSingles, plhs[0], mxSetSingles);

  MAKE_INTERLEAVED_COPY (mxComplexSingle, mxSINGLE_CLASS, mxCOMPLEX, prhs[1],
                         mxGetComplexSingles, plhs[1], mxSetComplexSingles);

  MAKE_INTERLEAVED_COPY (mxDouble, mxDOUBLE_CLASS, mxREAL, prhs[2],
                         mxGetDoubles, plhs[2], mxSetDoubles);

  MAKE_INTERLEAVED_COPY (mxComplexDouble, mxDOUBLE_CLASS, mxCOMPLEX, prhs[3],
                         mxGetComplexDoubles, plhs[3], mxSetComplexDoubles);

#else

  MAKE_REAL_COPY (mxSingle, mxSINGLE_CLASS, prhs[0], plhs[0]);

  MAKE_CPLX_COPY (mxSingle, mxSINGLE_CLASS, prhs[1], plhs[1]);

  MAKE_REAL_COPY (mxDouble, mxDOUBLE_CLASS, prhs[2], plhs[2]);

  MAKE_CPLX_COPY (mxDouble, mxDOUBLE_CLASS, prhs[3], plhs[3]);

#endif

}
