/* ========================================================================== */
/* === colamd mexFunction =================================================== */
/* ========================================================================== */

/*
    Usage:

	P = colamd (A) ;

	P = colamd (A, knobs) ;

	[ P, stats ] = colamd (A) ;

	[ P, stats ] = colamd (A, knobs) ;

    Returns a permutation vector P such that the LU factorization of A (:,P)
    tends to be sparser than that of A.  The Cholesky factorization of
    (A (:,P))'*(A (:,P)) will also tend to be sparser than that of A'*A.
    This routine provides the same functionality as COLMMD, but is much faster
    and returns a better permutation vector.  Note that the COLMMD m-file in
    MATLAB 5.2 also performs a column elimination tree post-ordering.  This
    mexFunction does not do this post-ordering.  This mexFunction is a
    replacement for the p = sparsfun ('colmmd', A) operation.

    The knobs and stats vectors are optional:

	knobs (1)	rows with more than (knobs (1))*n_col entries
			are removed prior to ordering.  If knobs is not present,
			then the default is used (0.5).

	knobs (2)	columns with more than (knobs (2))*n_row entries
			are removed prior to ordering, and placed last in the
			column permutation.  If knobs is not present,
			then the default is used (0.5).

	knobs (3)	print level, similar to spparms ('spumoni')

	stats (1)	the number of dense (or empty) rows ignored

	stats (2)	the number of dense (or empty) columms.  These
			are ordered last, in their natural order.

	stats (3)	the number of garbage collections performed.

	stats (4)	return status:

			0:  matrix is a valid MATLAB matrix.

			1:  matrix has duplicate entries or unsorted columns.
			    This should not occur in a valid MATLAB matrix,
			    but the ordering proceeded anyway by sorting the
			    row indices in each column and by ignoring the
			    duplicate row indices in each column.  See
			    stats (5:7) for more information.

	stats (5)	highest numbered column that is unsorted or has
			duplicate entries (zero if none)

	stats (6)	last seen duplicate or unsorted row index
			(zero if none)

	stats (7)	number of duplicate or unsorted row indices

    Authors:

	The authors of the code itself are Stefan I. Larimore and Timothy A.
	Davis (davis@cise.ufl.edu), University of Florida.  The algorithm was
	developed in collaboration with John Gilbert, Xerox PARC, and Esmond
	Ng, Oak Ridge National Laboratory.

    Date:

	September 8, 2003.  Version 2.3.

    Acknowledgements:

	This work was supported by the National Science Foundation, under
	grants DMS-9504974 and DMS-9803599.

    Notice:

	Copyright (c) 1998-2003 by the University of Florida.
	All Rights Reserved.

	See http://www.cise.ufl.edu/research/sparse/colamd (the colamd.c
	file) for the License.

    Availability:

	The colamd/symamd library is available at

	    http://www.cise.ufl.edu/research/sparse/colamd/

	This is the http://www.cise.ufl.edu/research/sparse/colamd/colamdmex.c
	file.  It requires the colamd.c and colamd.h files.

*/

/* ========================================================================== */
/* === Include files ======================================================== */
/* ========================================================================== */

#include "colamd.h"
#include "mex.h"
#include "matrix.h"
#include <stdlib.h>
#include <string.h>

/* ========================================================================== */
/* === colamd mexFunction =================================================== */
/* ========================================================================== */

void mexFunction
(
    /* === Parameters ======================================================= */

    int nlhs,			/* number of left-hand sides */
    mxArray *plhs [],		/* left-hand side matrices */
    int nrhs,			/* number of right--hand sides */
    const mxArray *prhs []	/* right-hand side matrices */
)
{
    /* === Local variables ================================================== */

    int *A ;			/* colamd's copy of the matrix, and workspace */
    int *p ;			/* colamd's copy of the column pointers */
    int Alen ;			/* size of A */
    int n_col ;			/* number of columns of A */
    int n_row ;			/* number of rows of A */
    int nnz ;			/* number of entries in A */
    int full ;			/* TRUE if input matrix full, FALSE if sparse */
    double knobs [COLAMD_KNOBS] ; /* colamd user-controllable parameters */
    double *out_perm ;		/* output permutation vector */
    double *out_stats ;		/* output stats vector */
    double *in_knobs ;		/* input knobs vector */
    int i ;			/* loop counter */
    mxArray *Ainput ;		/* input matrix handle */
    int spumoni ;		/* verbosity variable */
    int stats [COLAMD_STATS] ;	/* stats for colamd */

    /* === Check inputs ===================================================== */

    if (nrhs < 1 || nrhs > 2 || nlhs < 0 || nlhs > 2)
    {
	mexErrMsgTxt (
	"colamd: incorrect number of input and/or output arguments") ;
    }

    /* === Get knobs ======================================================== */

    colamd_set_defaults (knobs) ;
    spumoni = 0 ;

    /* check for user-passed knobs */
    if (nrhs == 2)
    {
	in_knobs = mxGetPr (prhs [1]) ;
	i = mxGetNumberOfElements (prhs [1]) ;
	if (i > 0) knobs [COLAMD_DENSE_ROW] = in_knobs [COLAMD_DENSE_ROW] ;
	if (i > 1) knobs [COLAMD_DENSE_COL] = in_knobs [COLAMD_DENSE_COL] ;
	if (i > 2) spumoni = (int) in_knobs [2] ;
    }

    /* print knob settings if spumoni is set */
    if (spumoni > 0)
    {
	mexPrintf ("colamd: dense row fraction: %f\n",
	    knobs [COLAMD_DENSE_ROW]) ;
	mexPrintf ("colamd: dense col fraction: %f\n",
	    knobs [COLAMD_DENSE_COL]) ;
    }

    /* === If A is full, convert to a sparse matrix ========================= */

    Ainput = (mxArray *) prhs [0] ;
    if (mxGetNumberOfDimensions (Ainput) != 2)
    {
	mexErrMsgTxt ("colamd: input matrix must be 2-dimensional") ;
    }
    full = !mxIsSparse (Ainput) ;
    if (full)
    {
	mexCallMATLAB (1, &Ainput, 1, (mxArray **) prhs, "sparse") ;
    }

    /* === Allocate workspace for colamd ==================================== */

    /* get size of matrix */
    n_row = mxGetM (Ainput) ;
    n_col = mxGetN (Ainput) ;

    /* get column pointer vector so we can find nnz */
    p = (int *) mxCalloc (n_col+1, sizeof (int)) ;
    (void) memcpy (p, mxGetJc (Ainput), (n_col+1)*sizeof (int)) ;
    nnz = p [n_col] ;
    Alen = colamd_recommended (nnz, n_row, n_col) ;

    /* === Copy input matrix into workspace ================================= */

    A = (int *) mxCalloc (Alen, sizeof (int)) ;
    (void) memcpy (A, mxGetIr (Ainput), nnz*sizeof (int)) ;

    if (full)
    {
	mxDestroyArray (Ainput) ;
    }

    /* === Order the columns (destroys A) =================================== */

    if (!colamd (n_row, n_col, Alen, A, p, knobs, stats))
    {
	colamd_report (stats) ;
	mexErrMsgTxt ("colamd error!") ;
    }
    mxFree (A) ;

    /* === Return the permutation vector ==================================== */

    plhs [0] = mxCreateDoubleMatrix (1, n_col, mxREAL) ;
    out_perm = mxGetPr (plhs [0]) ;
    for (i = 0 ; i < n_col ; i++)
    {
	/* colamd is 0-based, but MATLAB expects this to be 1-based */
	out_perm [i] = p [i] + 1 ;
    }
    mxFree (p) ;

    /* === Return the stats vector ========================================== */

    /* print stats if spumoni > 0 */
    if (spumoni > 0)
    {
	colamd_report (stats) ;
    }

    if (nlhs == 2)
    {
	plhs [1] = mxCreateDoubleMatrix (1, COLAMD_STATS, mxREAL) ;
	out_stats = mxGetPr (plhs [1]) ;
	for (i = 0 ; i < COLAMD_STATS ; i++)
	{
	    out_stats [i] = stats [i] ;
	}

	/* fix stats (5) and (6), for 1-based information on jumbled matrix. */
	/* note that this correction doesn't occur if symamd returns FALSE */
	out_stats [COLAMD_INFO1] ++ ; 
	out_stats [COLAMD_INFO2] ++ ; 
    }
}
