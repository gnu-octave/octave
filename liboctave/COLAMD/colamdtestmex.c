/* ========================================================================== */
/* === colamdtest mexFunction =============================================== */
/* ========================================================================== */

/*
    This MATLAB mexFunction is for testing only.  It is not meant for
    production use.  See colamdmex.c instead.

    Usage:

	[ P, stats ] = colamdtest (A, knobs) ;

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

	knobs (4)	for testing only.  Controls the workspace used by
			colamd.

	knobs (5)	for testing only.  Controls how the input matrix is
			jumbled prior to calling colamd, to test its error
			handling capability.

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

	This is the
	http://www.cise.ufl.edu/research/sparse/colamd/colamdtestmex.c
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
    int stats2 [COLAMD_STATS] ;	/* stats for colamd */

    int *cp, *cp_end, result, col, length ;
    int *stats ;
    stats = stats2 ;

    /* === Check inputs ===================================================== */

    if (nrhs < 1 || nrhs > 2 || nlhs < 0 || nlhs > 2)
    {
	mexErrMsgTxt (
	"colamd: incorrect number of input and/or output arguments") ;
    }

    if (nrhs != 2)
    {
	mexErrMsgTxt ("colamdtest: knobs are required") ;
    }
    /* for testing we require all 5 knobs */
    if (mxGetNumberOfElements (prhs [1]) < 5)
    {
	mexErrMsgTxt ("colamd: must have all 5 knobs for testing") ;
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


/* === Modify size of Alen if testing ======================================= */

/*
	knobs [3]	amount of workspace given to colamd.
			<  0 : TIGHT memory
			>  0 : MIN + knob [3] - 1
			== 0 : RECOMMENDED memory
*/

/* Here only for testing */
#ifdef MIN
#undef MIN
#endif
#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define COLAMD_MIN_MEMORY(nnz,n_row,n_col) \
    (2 * (nnz) + COLAMD_C (n_col) + COLAMD_R (n_row))

    /* get knob [3], if negative */
    if (in_knobs [3] < 0)
    {
	Alen = COLAMD_MIN_MEMORY (nnz, n_row, n_col) + n_col ;
    }
    else if (in_knobs [3] > 0)
    {
	Alen = COLAMD_MIN_MEMORY (nnz, n_row, n_col) + in_knobs [3] - 1 ;
    }

    /* otherwise, we use the recommended amount set above */

    /* === Copy input matrix into workspace ================================= */

    A = (int *) mxCalloc (Alen, sizeof (int)) ;
    (void) memcpy (A, mxGetIr (Ainput), nnz*sizeof (int)) ;

    if (full)
    {
	mxDestroyArray (Ainput) ;
    }


/* === Jumble matrix ======================================================== */

/*
	knobs [4]	FOR TESTING ONLY: Specifies how to jumble matrix
			0 : No jumbling
			1 : Make n_row less than zero
			2 : Make first pointer non-zero
			3 : Make column pointers not non-decreasing
			4 : Make a column pointer greater or equal to Alen
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
    switch ((int) in_knobs [4])
    {

	case 0 :
	    if (spumoni > 0)
	    {
		mexPrintf ("colamdtest: no errors expected\n") ;
	    }
	    result = 1 ;		/* no errors */
	    break ;

	case 1 :
	    if (spumoni > 0)
	    {
		mexPrintf ("colamdtest: nrow out of range\n") ;
	    }
	    result = 0 ;		/* nrow out of range */
	    n_row = -1 ;
	    break ;

	case 2 :
	    if (spumoni > 0)
	    {
		mexPrintf ("colamdtest: p [0] nonzero\n") ;
	    }
	    result = 0 ;		/* p [0] must be zero */
	    p [0] = 1 ;
	    break ;

	case 3 :
	    if (spumoni > 0)
	    {
		mexPrintf ("colamdtest: negative length last column\n") ;
	    }
	    result = (n_col == 0) ;	/* p must be monotonically inc. */
	    p [n_col] = p [0] ;
	    break ;

	case 4 :
	    if (spumoni > 0)
	    {
		mexPrintf ("colamdtest: Alen too small\n") ;
	    }
	    result = 0 ;		/* out of memory */
	    p [n_col] = Alen ;
	    break ;

	case 5 :
	    if (spumoni > 0)
	    {
		mexPrintf ("colamdtest: row index out of range (-1)\n") ;
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
		mexPrintf ("colamdtest: row index out of range (n_row)\n") ;
	    }
	    if (nnz > 0)		/* row index out of range */
	    {
		if (spumoni > 0)
		{
		    mexPrintf ("Changing A[nnz-1] from %d to %d\n",
			    A [nnz-1], n_row) ; 
		}
		result = 0 ;
		A [nnz-1] = n_row ;
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
		mexPrintf ("colamdtest: A not present\n") ;
	    }
	    result = 0 ;		/* A not present */
	    A = (int *) NULL ;
	    break ;

	case 8 :
	    if (spumoni > 0)
	    {
		mexPrintf ("colamdtest: p not present\n") ;
	    }
	    result = 0 ;		/* p not present */
	    p = (int *) NULL ;
	    break ;

	case 9 :
	    if (spumoni > 0)
	    {
		mexPrintf ("colamdtest: duplicate row index\n") ;
	    }
	    result = 1 ;		/* duplicate row index */

	    for (col = 0 ; col < n_col ; col++)
	    {
		length = p [col+1] - p [col] ;
	    	if (length > 1)
		{
		    A [p [col]] = A [p [col] + 1] ;
		    if (spumoni > 0)
		    {
			mexPrintf ("Made duplicate row %d in col %d\n",
		    	 A [p [col] + 1], col) ;
		    }
		    break ;
		}
	    }

	    if (spumoni > 1)
	    {
		dump_matrix (A, p, n_row, n_col, Alen, col+2) ;
	    }
	    break ;

	case 10 :
	    if (spumoni > 0)
	    {
		mexPrintf ("colamdtest: unsorted column\n") ;
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
		dump_matrix (A, p, n_row, n_col, Alen, col+2) ;
	    }
	    break ;

	case 11 :
	    if (spumoni > 0)
	    {
		mexPrintf ("colamdtest: massive jumbling\n") ;
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
		dump_matrix (A, p, n_row, n_col, Alen, n_col) ;
	    }
	    break ;

	case 12 :
	    if (spumoni > 0)
	    {
		mexPrintf ("colamdtest: stats not present\n") ;
	    }
	    result = 0 ;		/* stats not present */
	    stats = (int *) NULL ;
	    break ;

	case 13 :
	    if (spumoni > 0)
	    {
		mexPrintf ("colamdtest: ncol out of range\n") ;
	    }
	    result = 0 ;		/* ncol out of range */
	    n_col = -1 ;
	    break ;

    }


    /* === Order the columns (destroys A) =================================== */

    if (!colamd (n_row, n_col, Alen, A, p, knobs, stats))
    {

	/* return p = -1 if colamd failed */
	plhs [0] = mxCreateDoubleMatrix (1, 1, mxREAL) ;
	out_perm = mxGetPr (plhs [0]) ;
	out_perm [0] = -1 ;
	mxFree (p) ;
	mxFree (A) ;

	if (spumoni > 0 || result)
	{
	    colamd_report (stats) ;
	}

	if (result)
	{
	    mexErrMsgTxt ("colamd should have returned TRUE\n") ;
	}

	return ;
	/* mexErrMsgTxt ("colamd error!") ; */
    }

    if (!result)
    {
	colamd_report (stats) ;
	mexErrMsgTxt ("colamd should have returned FALSE\n") ;
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
