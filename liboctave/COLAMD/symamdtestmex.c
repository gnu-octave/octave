/* ========================================================================== */
/* === symamdtest mexFunction =============================================== */
/* ========================================================================== */

/*
    This MATLAB mexFunction is for testing only.  It is not meant for
    production use.  See symamdmex.c instead.

    Usage:

	[ P, stats ] = symamdtest (A, knobs) ;

    Returns a permutation vector P such that the Cholesky factorization of
    A (P,P) tends to be sparser than that of A.  This routine provides the same
    functionality as SYMMMD, but tends to be much faster and tends to return a
    better permutation vector.  Note that the SYMMMD m-file in
    MATLAB 5.2 also performs a symmetric elimination tree post-ordering.  This
    mexFunction does not do this post-ordering.  This mexFunction is a
    replacement for the p = sparsfun ('symmmd', A) operation.

    A must be square, and is assummed to have a symmetric nonzero pattern.
    Only the nonzero pattern of the lower triangular portion of A is accessed.
    This routine constructs a matrix M such that the nonzero pattern of M'M is
    equal to A (assuming A has symmetric pattern), and then performs a column
    ordering of M using colamd.

    The knobs and stats vectors are optional:

	knobs (1)	rows and columns with more than (knobs(1))*n entries
			are removed prior to ordering, and placed last in
			the output ordering.  If knobs is not present, then the
			default of 0.5 is used.

	knobs (2)	print level, similar to spparms ('spumoni')

	knobs (3)	for testing only.  Controls how the input matrix is
			jumbled prior to calling symamd, to test its error
			handling capability.

	stats (1)	the number of dense (or empty) rows and columms.  These
			are ordered last, in their natural order.

	stats (2)	(same as stats (1))

	stats (3)	the number of garbage collections performed.

	stats (4)	return status:

			0:  matrix is a valid MATLAB matrix.

			1:  matrix has duplicate entries or unsorted columns.
			    This should not occur in a valid MATLAB matrix,
			    but the ordering proceeded anyway by ignoring the
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

	This is the
	http://www.cise.ufl.edu/research/sparse/colamd/symamdtestmex.c
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

static void dump_matrix
(
    int A [ ],
    int p [ ],
    int n_row,
    int n_col,
    int Alen,
    int limit
) ;

/* ========================================================================== */
/* === symamd mexFunction =================================================== */
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

    int *perm ;			/* column ordering of M and ordering of A */
    int *A ;			/* row indices of input matrix A */
    int *p ;			/* column pointers of input matrix A */
    int n_col ;			/* number of columns of A */
    int n_row ;			/* number of rows of A */
    int full ;			/* TRUE if input matrix full, FALSE if sparse */
    double knobs [COLAMD_KNOBS] ; /* colamd user-controllable parameters */
    double *out_perm ;		/* output permutation vector */
    double *out_stats ;		/* output stats vector */
    double *in_knobs ;		/* input knobs vector */
    int i ;			/* loop counter */
    mxArray *Ainput ;		/* input matrix handle */
    int spumoni ;		/* verbosity variable */
    int stats2 [COLAMD_STATS] ;	/* stats for symamd */

    int *cp, *cp_end, result, nnz, col, length ;
    int *stats ;
    stats = stats2 ;

    /* === Check inputs ===================================================== */

    if (nrhs < 1 || nrhs > 2 || nlhs < 0 || nlhs > 2)
    {
	mexErrMsgTxt (
	"symamd: incorrect number of input and/or output arguments.") ;
    }

    if (nrhs != 2)
    {
	mexErrMsgTxt ("symamdtest: knobs are required") ;
    }
    /* for testing we require all 3 knobs */
    if (mxGetNumberOfElements (prhs [1]) < 3)
    {
	mexErrMsgTxt ("symamdtest: must have all 3 knobs for testing") ;
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
	if (i > 1) spumoni = (int) in_knobs [1] ;
    }

    /* print knob settings if spumoni > 0 */
    if (spumoni > 0)
    {
	mexPrintf ("symamd: dense row/col fraction: %f\n",
	    knobs [COLAMD_DENSE_ROW]) ;
    }

    /* === If A is full, convert to a sparse matrix ========================= */

    Ainput = (mxArray *) prhs [0] ;
    if (mxGetNumberOfDimensions (Ainput) != 2)
    {
	mexErrMsgTxt ("symamd: input matrix must be 2-dimensional.") ;
    }
    full = !mxIsSparse (Ainput) ;
    if (full)
    {
	mexCallMATLAB (1, &Ainput, 1, (mxArray **) prhs, "sparse") ;
    }

    /* === Allocate workspace for symamd ==================================== */

    /* get size of matrix */
    n_row = mxGetM (Ainput) ;
    n_col = mxGetN (Ainput) ;
    if (n_col != n_row)
    {
	mexErrMsgTxt ("symamd: matrix must be square.") ;
    }

    /* p = mxGetJc (Ainput) ; */
    p = (int *) mxCalloc (n_col+1, sizeof (int)) ;
    (void) memcpy (p, mxGetJc (Ainput), (n_col+1)*sizeof (int)) ;

    nnz = p [n_col] ;
    if (spumoni > 0)
    {
	mexPrintf ("symamdtest: nnz %d\n", nnz) ;
    }

    /* A = mxGetIr (Ainput) ; */
    A = (int *) mxCalloc (nnz+1, sizeof (int)) ;
    (void) memcpy (A, mxGetIr (Ainput), nnz*sizeof (int)) ;

    perm = (int *) mxCalloc (n_col+1, sizeof (int)) ;

/* === Jumble matrix ======================================================== */


/*
	knobs [2]	FOR TESTING ONLY: Specifies how to jumble matrix
			0 : No jumbling
			1 : (no errors)
			2 : Make first pointer non-zero
			3 : Make column pointers not non-decreasing
			4 : (no errors)
			5 : Make row indices not strictly increasing
			6 : Make a row index greater or equal to n_row
			7 : Set A = NULL
			8 : Set p = NULL
			9 : Repeat row index
			10: make row indices not sorted
			11: jumble columns massively (note this changes
				the pattern of the matrix A.)
			12: Set stats = NULL
			13: Make n_col less than zero
*/

    /* jumble appropriately */
    switch ((int) in_knobs [2])
    {

	case 0 :
	    if (spumoni > 0)
	    {
		mexPrintf ("symamdtest: no errors expected\n") ;
	    }
	    result = 1 ;		/* no errors */
	    break ;

	case 1 :
	    if (spumoni > 0)
	    {
		mexPrintf ("symamdtest: no errors expected (1)\n") ;
	    }
	    result = 1 ;
	    break ;

	case 2 :
	    if (spumoni > 0)
	    {
		mexPrintf ("symamdtest: p [0] nonzero\n") ;
	    }
	    result = 0 ;		/* p [0] must be zero */
	    p [0] = 1 ;
	    break ;

	case 3 :
	    if (spumoni > 0)
	    {
		mexPrintf ("symamdtest: negative length last column\n") ;
	    }
	    result = (n_col == 0) ;	/* p must be monotonically inc. */
	    p [n_col] = p [0] ;
	    break ;

	case 4 :
	    if (spumoni > 0)
	    {
		mexPrintf ("symamdtest: no errors expected (4)\n") ;
	    }
	    result = 1 ;
	    break ;

	case 5 :
	    if (spumoni > 0)
	    {
		mexPrintf ("symamdtest: row index out of range (-1)\n") ;
	    }
	    if (nnz > 0)		/* row index out of range */
	    {
		result = 0 ;
		A [nnz-1] = -1 ;
	    }
	    else
	    {
	        if (spumoni > 0)
		{
		    mexPrintf ("Note: no row indices to put out of range\n") ;
		}
		result = 1 ;
	    }
	    break ;

	case 6 :
	    if (spumoni > 0)
	    {
		mexPrintf ("symamdtest: row index out of range (ncol)\n") ;
	    }
	    if (nnz > 0)		/* row index out of range */
	    {
		result = 0 ;
		A [nnz-1] = n_col ;
	    }
	    else
	    {
	        if (spumoni > 0)
		{
		    mexPrintf ("Note: no row indices to put out of range\n") ;
		}
		result = 1 ;
	    }
	    break ;

	case 7 :
	    if (spumoni > 0)
	    {
		mexPrintf ("symamdtest: A not present\n") ;
	    }
	    result = 0 ;		/* A not present */
	    A = (int *) NULL ;
	    break ;

	case 8 :
	    if (spumoni > 0)
	    {
		mexPrintf ("symamdtest: p not present\n") ;
	    }
	    result = 0 ;		/* p not present */
	    p = (int *) NULL ;
	    break ;

	case 9 :
	    if (spumoni > 0)
	    {
		mexPrintf ("symamdtest: duplicate row index\n") ;
	    }
	    result = 1 ;		/* duplicate row index */

	    for (col = 0 ; col < n_col ; col++)
	    {
		length = p [col+1] - p [col] ;
	    	if (length > 1)
		{
		    A [p [col+1]-2] = A [p [col+1] - 1] ;
		    if (spumoni > 0)
		    {
			mexPrintf ("Made duplicate row %d in col %d\n",
		    	 A [p [col+1] - 1], col) ;
		    }
		    break ;
		}
	    }

	    if (spumoni > 1)
	    {
		dump_matrix (A, p, n_row, n_col, nnz, col+2) ;
	    }
	    break ;

	case 10 :
	    if (spumoni > 0)
	    {
		mexPrintf ("symamdtest: unsorted column\n") ;
	    }
	    result = 1 ;		/* jumbled columns */

	    for (col = 0 ; col < n_col ; col++)
	    {
		length = p [col+1] - p [col] ;
	    	if (length > 1)
		{
		    i = A[p [col]] ;
		    A [p [col]] = A[p [col] + 1] ;
		    A [p [col] + 1] = i ;
		    if (spumoni > 0)
		    {
			mexPrintf ("Unsorted column %d \n", col) ;
		    }
		    break ;
		}
	    }

	    if (spumoni > 1)
	    {
		dump_matrix (A, p, n_row, n_col, nnz, col+2) ;
	    }
	    break ;

	case 11 :
	    if (spumoni > 0)
	    {
		mexPrintf ("symamdtest: massive jumbling\n") ;
	    }
	    result = 1 ;		/* massive jumbling, but no errors */
	    srand (1) ;
	    for (i = 0 ; i < n_col ; i++)
	    {
		cp = &A [p [i]] ;
		cp_end = &A [p [i+1]] ;
		while (cp < cp_end)
		{
		    *cp++ = rand() % n_row ;
		}
	    }
	    if (spumoni > 1)
	    {
		dump_matrix (A, p, n_row, n_col, nnz, n_col) ;
	    }
	    break ;

	case 12 :
	    if (spumoni > 0)
	    {
		mexPrintf ("symamdtest: stats not present\n") ;
	    }
	    result = 0 ;		/* stats not present */
	    stats = (int *) NULL ;
	    break ;

	case 13 :
	    if (spumoni > 0)
	    {
		mexPrintf ("symamdtest: ncol out of range\n") ;
	    }
	    result = 0 ;		/* ncol out of range */
	    n_col = -1 ;
	    break ;

    }

    /* === Order the rows and columns of A (does not destroy A) ============= */

    if (!symamd (n_col, A, p, perm, knobs, stats, &mxCalloc, &mxFree))
    {

	/* return p = -1 if colamd failed */
	plhs [0] = mxCreateDoubleMatrix (1, 1, mxREAL) ;
	out_perm = mxGetPr (plhs [0]) ;
	out_perm [0] = -1 ;
	mxFree (p) ;
	mxFree (A) ;

	if (spumoni > 0 || result)
	{
	    symamd_report (stats) ;
	}

	if (result)
	{
	    mexErrMsgTxt ("symamd should have returned TRUE\n") ;
	}

	return ;
	/* mexErrMsgTxt ("symamd error!") ; */
    }

    if (!result)
    {
	symamd_report (stats) ;
	mexErrMsgTxt ("symamd should have returned FALSE\n") ;
    }

    if (full)
    {
	mxDestroyArray (Ainput) ;
    }

    /* === Return the permutation vector ==================================== */

    plhs [0] = mxCreateDoubleMatrix (1, n_col, mxREAL) ;
    out_perm = mxGetPr (plhs [0]) ;
    for (i = 0 ; i < n_col ; i++)
    {
	/* symamd is 0-based, but MATLAB expects this to be 1-based */
	out_perm [i] = perm [i] + 1 ;
    }
    mxFree (perm) ;

    /* === Return the stats vector ========================================== */

    /* print stats if spumoni > 0 */
    if (spumoni > 0)
    {
	symamd_report (stats) ;
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


#ifdef MIN
#undef MIN
#endif
#define MIN(a,b) (((a) < (b)) ? (a) : (b))


static void dump_matrix
(
    int A [ ],
    int p [ ],
    int n_row,
    int n_col,
    int Alen,
    int limit
)
{
    int col, k, row ;

    mexPrintf ("dump matrix: nrow %d ncol %d Alen %d\n", n_row, n_col, Alen) ;

    if (!A)
    {
    	mexPrintf ("A not present\n") ;
	return ;
    }

    if (!p)
    {
    	mexPrintf ("p not present\n") ;
	return ;
    }

    for (col = 0 ; col < MIN (n_col, limit) ; col++)
    {
	mexPrintf ("column %d, p[col] %d, p [col+1] %d, length %d\n",
		col, p [col], p [col+1], p [col+1] - p [col]) ;
    	for (k = p [col] ; k < p [col+1] ; k++)
	{
	    row = A [k] ;
	    mexPrintf (" %d", row) ;
	}
	mexPrintf ("\n") ;
    }
}
