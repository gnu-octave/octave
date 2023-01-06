########################################################################
##
## Copyright (C) 2005-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {@var{d} =} eigs (@var{A})
## @deftypefnx {} {@var{d} =} eigs (@var{A}, @var{k})
## @deftypefnx {} {@var{d} =} eigs (@var{A}, @var{k}, @var{sigma})
## @deftypefnx {} {@var{d} =} eigs (@var{A}, @var{k}, @var{sigma}, @var{opts})
## @deftypefnx {} {@var{d} =} eigs (@var{A}, @var{B})
## @deftypefnx {} {@var{d} =} eigs (@var{A}, @var{B}, @var{k})
## @deftypefnx {} {@var{d} =} eigs (@var{A}, @var{B}, @var{k}, @var{sigma})
## @deftypefnx {} {@var{d} =} eigs (@var{A}, @var{B}, @var{k}, @var{sigma}, @var{opts})
## @deftypefnx {} {@var{d} =} eigs (@var{Af}, @var{n})
## @deftypefnx {} {@var{d} =} eigs (@var{Af}, @var{n}, @var{k})
## @deftypefnx {} {@var{d} =} eigs (@var{Af}, @var{n}, @var{k}, @var{sigma})
## @deftypefnx {} {@var{d} =} eigs (@var{Af}, @var{n}, @var{k}, @var{sigma}, @var{opts})
## @deftypefnx {} {@var{d} =} eigs (@var{Af}, @var{n}, @var{B})
## @deftypefnx {} {@var{d} =} eigs (@var{Af}, @var{n}, @var{B}, @var{k})
## @deftypefnx {} {@var{d} =} eigs (@var{Af}, @var{n}, @var{B}, @var{k}, @var{sigma})
## @deftypefnx {} {@var{d} =} eigs (@var{Af}, @var{n}, @var{B}, @var{k}, @var{sigma}, @var{opts})
## @deftypefnx {} {[@var{V}, @var{D}] =} eigs (@dots{})
## @deftypefnx {} {[@var{V}, @var{D}, @var{flag}] =} eigs (@dots{})
## Calculate a limited number of eigenvalues and eigenvectors based on a
## selection criteria.
##
## By default, @code{eigs} solve the equation
## @tex
## $A \nu = \lambda \nu$,
## @end tex
## @ifinfo
## @code{A * v = lambda * v},
## @end ifinfo
## where
## @tex
## $\lambda$ is a scalar representing one of the eigenvalues, and $\nu$
## @end tex
## @ifinfo
## @code{lambda} is a scalar representing one of the eigenvalues, and @code{v}
## @end ifinfo
## is the corresponding eigenvector.  If given the positive definite matrix
## @var{B} then @code{eigs} solves the general eigenvalue equation
## @tex
## $A \nu = \lambda B \nu$.
## @end tex
## @ifinfo
## @code{A * v = lambda * B * v}.
## @end ifinfo
##
## The input @var{A} is a square matrix of dimension @var{n}-by-@var{n}.
## Typically, @var{A} is also large and sparse.
##
## The input @var{B} for the generalized eigenvalue problem is a square matrix
## with the same size as @var{A} (@var{n}-by-@var{n}).  Typically, @var{B} is
## also large and sparse.
##
## The number of eigenvalues and eigenvectors to calculate is given by @var{k}
## and defaults to 6.
##
## The argument @var{sigma} determines which eigenvalues are returned.
## @var{sigma} can be either a scalar or a string.  When @var{sigma} is a
## scalar, the @var{k} eigenvalues closest to @var{sigma} are returned.  If
## @var{sigma} is a string, it must be one of the following values.
##
## @table @asis
## @item @nospell{@qcode{"lm"}}
## Largest Magnitude (default).
##
## @item @nospell{@qcode{"sm"}}
## Smallest Magnitude.
##
## @item @nospell{@qcode{"la"}}
## Largest Algebraic (valid only for real symmetric problems).
##
## @item @nospell{@qcode{"sa"}}
## Smallest Algebraic (valid only for real symmetric problems).
##
## @item @nospell{@qcode{"be"}}
## Both Ends, with one more from the high-end if @var{k} is odd (valid only for
## real symmetric problems).
##
## @item @nospell{@qcode{"lr"}}
## Largest Real part (valid only for complex or unsymmetric problems).
##
## @item @nospell{@qcode{"sr"}}
## Smallest Real part (valid only for complex or unsymmetric problems).
##
## @item @nospell{@qcode{"li"}}
## Largest Imaginary part (valid only for complex or unsymmetric problems).
##
## @item @nospell{@qcode{"si"}}
## Smallest Imaginary part (valid only for complex or unsymmetric problems).
## @end table
##
## If @var{opts} is given, it is a structure defining possible options that
## @code{eigs} should use.  The fields of the @var{opts} structure are:
##
## @table @code
## @item issym
## If @var{Af} is given then this flag (true/false) determines whether the
## function @var{Af} defines a symmetric problem.  It is ignored if a matrix
## @var{A} is given.  The default is false.
##
## @item isreal
## If @var{Af} is given then this flag (true/false) determines whether the
## function @var{Af} defines a real problem.  It is ignored if a matrix @var{A}
## is given.  The default is true.
##
## @item tol
## Defines the required convergence tolerance, calculated as
## @code{tol * norm (A)}.  The default is @code{eps}.
##
## @item maxit
## The maximum number of iterations.  The default is 300.
##
## @item p
## The number of @nospell{Lanczos} basis vectors to use.  More vectors will
## result in faster convergence, but a greater use of memory.  The optimal
## value of @code{p} is problem dependent and should be in the range
## @code{@var{k} + 1} to @var{n}.  The default value is @code{2 * @var{k}}.
##
## @item v0
## The starting vector for the algorithm.  An initial vector close to the final
## vector will speed up convergence.  The default is for @sc{arpack} to
## randomly generate a starting vector.  If specified, @code{v0} must be
## an @var{n}-by-1 vector where @code{@var{n} = rows (@var{A})}.
##
## @item disp
## The level of diagnostic printout (0|1|2).  If @code{disp} is 0 then
## diagnostics are disabled.  The default value is 0.
##
## @item cholB
## If the generalized eigenvalue problem is being calculated, this flag
## (true/false) specifies whether the @var{B} input represents
## @code{chol (@var{B})} or simply the matrix @var{B}.  The default is false.
##
## @item permB
## The permutation vector of the Cholesky@tie{}factorization for @var{B} if
## @code{cholB} is true.  It is obtained by
## @code{[R, ~, permB] = chol (@var{B}, @qcode{"vector"})}.  The default is
## @code{1:@var{n}}.
##
## @end table
##
## It is also possible to represent @var{A} by a function denoted @var{Af}.
## @var{Af} must be followed by a scalar argument @var{n} defining the length
## of the vector argument accepted by @var{Af}.  @var{Af} can be a function
## handle, an inline function, or a string.  When @var{Af} is a string it
## holds the name of the function to use.
##
## @var{Af} is a function of the form @code{y = Af (x)} where the required
## return value of @var{Af} is determined by the value of @var{sigma}.
## The four possible forms are
##
## @table @code
## @item A * x
## if @var{sigma} is not given or is a string other than "sm".
##
## @item A \ x
## if @var{sigma} is 0 or "sm".
##
## @item (A - sigma * I) \ x
## if @var{sigma} is a scalar not equal to 0; @code{I} is the identity matrix
## of the same size as @var{A}.
##
## @item (A - sigma * B) \ x
## for the general eigenvalue problem.
## @end table
##
## The return arguments and their form depend on the number of return arguments
## requested.  For a single return argument, a column vector @var{d} of length
## @var{k} is returned containing the @var{k} eigenvalues that have been
## found.  For two return arguments, @var{V} is an @var{n}-by-@var{k} matrix
## whose columns are the @var{k} eigenvectors corresponding to the returned
## eigenvalues.  The eigenvalues themselves are returned in @var{D} in the
## form of a @var{k}-by-@var{k} matrix, where the elements on the diagonal
## are the eigenvalues.
##
## The third return argument @var{flag} returns the status of the convergence.
## If @var{flag} is 0 then all eigenvalues have converged.  Any other value
## indicates a failure to converge.
##
## Programming Notes: For small problems, @var{n} < 500, consider using
## @code{eig (full (@var{A}))}.
##
## If @sc{arpack} fails to converge consider increasing the number of
## @nospell{Lanczos} vectors (@var{opt}.p), increasing the number of iterations
## (@var{opt}.maxiter), or decreasing the tolerance (@var{opt}.tol).
##
## Reference:
## This function is based on the @sc{arpack} package, written by
## @nospell{R. Lehoucq, K. Maschhoff, D. Sorensen, and C. Yang}.  For more
## information see @url{http://www.caam.rice.edu/software/ARPACK/}.
##
## @seealso{eig, svds}
## @end deftypefn

## Programming Note: For compatibility with Matlab, handle small matrix cases
## and all-zero matrices in this m-file which ARPACK can not.

function varargout = eigs (varargin)

  if (nargin == 0)
    print_usage ();
  endif

  call_eig = false;  # flag whether to take shortcut path with eig()
  have_A = false;
  have_B = false;
  offset = 0;
  k = 6;
  sigma = "lm";

  ## FIXME: Input validation is split between eigs.m and __eigs__.cc.
  ##        It would be simpler if all input validation was done in the m-file
  ##        and then the internal function __eigs__ could be simplified to
  ##        rely on having "good" inputs.
  if (isnumeric (varargin{1}) && issquare (varargin{1}))
    A = varargin{1};
    have_A = true;
    k = min (k, rows (A));  # reduce default k if necessary
    if (nargin > 1)
      if (! isnumeric (varargin{2}))
        error ("eigs: second argument must be numeric");
      endif
      if (size_equal (A, varargin{2}))
        B = varargin{2};
        have_B = true;
        offset = 1;
      elseif (isempty (varargin{2}))
        ## Special syntax to do regular eigenvalue decomposition rather
        ## than generalized eigenvalue decomposition (B = []).
        offset = 1;
      endif
    endif

    if (rows (A) <= 12)  # p = 2*k criteria
      call_eig = true;
    endif

    if (nargin > 1 + offset)
      ## FIXME: Input validation should recognize improper inputs.
      ##        Code below only checks for what it expects to find.
      ##        Sample bad input: eigs (magic (5), [], {1})
      arg = varargin{2+offset};
      if (isnumeric (arg) && isscalar (arg) && isreal (arg)
          && fix (arg) == arg)
        k = arg;
        p = 2 * k;
      elseif (isfield (arg, "p"))
        p = arg.p;
      endif

      if (nargin > 2 + offset)
        arg = varargin{3+offset};
        if (ischar (arg))
          sigma = tolower (arg);
        elseif (isnumeric (arg) && isscalar (arg))
          sigma = arg;
        elseif (isfield (arg, "p"))
          p = arg.p;
        endif

        if (nargin > 3 + offset)
          arg = varargin{4+offset};
          if (isfield (arg, "p"))
            p = arg.p;
          else
            p = 2 * k;
          endif
        endif
      endif

      if (p >= rows (A))
        call_eig = true;
      else
        call_eig = false;
      endif
    endif
  endif

  if (call_eig)
    ## Special code path for small matrices which ARPACK does not handle.
    varargout = cell (1, min (2, max (1, nargout)));
    if (have_B)
      real_valued = isreal (A) && isreal (B);
      symmetric = issymmetric (A) && issymmetric (B);
      [varargout{:}] = eig (A, B);
    else
      real_valued = isreal (A);
      symmetric = issymmetric (A);
      [varargout{:}] = eig (A);
    endif
    varargout = select_eig (varargout, k, sigma, real_valued, symmetric);
    if (nargout == 3)
      varargout{3} = 0;  # Flag value is always 0 (success) for eig() code path
    endif

  else
    varargout = cell (1, max (1, nargout));
    if (have_A && nnz (A) == 0)
      ## Special case of zeros matrix which ARPACK handles badly.
      switch (nargout)
        case 3
          V = diag (ones ([k,1]), rows (A), k);
          varargout = { V, diag(zeros (k,1)), 0.0 };

        case 2
          V = diag (ones ([k,1]), rows (A), k);
          varargout = { V, diag(zeros (k,1)) };

        case {0, 1}
          varargout = { zeros(k,1) };
      endswitch
    else
      ## Call ARPACK
      [varargout{:}] = __eigs__ (varargin{:});
    endif
  endif

endfunction

## For cases which do not go through ARPACK, but rather through eig() shortcut
## code path, select which values to return based on input parameters and
## number of outputs.
function out = select_eig (args, k, sigma, real_valued, symmetric)

  if (numel (args) == 1)
    d = args{1};
  else
    d = diag (args{2});
  endif

  n = numel (d);
  if (k > n)
    error ("eigs: requested number of eigenvalues K (%d) exceeds available eigenvalues (%d)", k, n);
  endif

  if (ischar (sigma))
    switch (sigma)
      case "lm"
        [~, idx] = sort (abs (d), "descend");

      case "sm"
        [~, idx] = sort (abs (d), "ascend");

      case "la"
        if (real_valued && symmetric)
          [~, idx] = sort (real (d), "descend");
        else
          error ('eigs: SIGMA = "la" requires real symmetric problem');
        endif

      case "sa"
        if (real_valued && symmetric)
          [~, idx] = sort (real (d), "ascend");
        else
          error ('eigs: SIGMA = "sa" requires real symmetric problem');
        endif

      case "be"
        if (real_valued && symmetric)
          [~, idx] = sort (real (d), "ascend");
        else
          error ('eigs: SIGMA = "be" requires real symmetric problem');
        endif

      case "lr"
        if (! (real_valued && symmetric))
          [~, idx] = sort (real (d), "descend");
        else
          error ('eigs: SIGMA = "lr" requires complex or unsymmetric problem');
        endif

      case "sr"
        if (! (real_valued && symmetric))
          [~, idx] = sort (real (d), "ascend");
        else
          error ('eigs: SIGMA = "sr" requires complex or unsymmetric problem');
        endif

      case "li"
        if (! (real_valued && symmetric))
          [~, idx] = sort (imag (d), "descend");
        else
          error ('eigs: SIGMA = "li" requires complex or unsymmetric problem');
        endif

      case "si"
        if (! (real_valued && symmetric))
          [~, idx] = sort (imag (d), "ascend");
        else
          error ('eigs: SIGMA = "si" requires complex or unsymmetric problem');
        endif

      otherwise
        error ("eigs: unrecognized value for SIGMA: %s", sigma);
    endswitch
  else
    ## numeric sigma, find k closest values
    [~, idx] = sort (abs (d - sigma));
  endif

  d = d(idx);

  if (strcmp (sigma, "be"))
    n1 = floor (k/2);
    n2 = n - (k - n1) + 1;
    selection = [1:n1, n2:n];
  else
    selection = 1:k;
  endif

  d = d(selection);

  if (numel (args) == 1)
    out{1} = d;
  else
    out{2} = diag (d);

    V = args{1};
    V = V(:,idx);
    out{1} = V(:,selection);
  endif

endfunction


### TRIVIAL TESTS ###

%!test
%! for i = 1:20
%!   assert (eigs (i, 1), i, 1e-11);
%!   assert (eigs (zeros (i), 1), 0, 1e-11);
%!   assert (eigs (sparse (i), 1), i, 1e-11);
%!   assert (eigs (sparse (i, i), 1), 0, 1e-11);
%! endfor

%!testif HAVE_ARPACK
%! for i = 1:20
%!   assert (eigs (ones (i), 1), i, 1e-11);
%!   assert (eigs (sparse (ones (i)), 1), i, 1e-11);
%! endfor

### SPARSE MATRIX TESTS ###

## Real positive definite tests, n must be even
%!shared n, k, A, d0, d2, old_state, restore_state
%! n = 20;
%! k = 4;
%! A = sparse ([3:n,1:n,1:(n-2)],[1:(n-2),1:n,3:n],[ones(1,n-2),4*ones(1,n),ones(1,n-2)]);
%! d0 = eig (A);
%! d2 = sort (d0);
%! [~, idx] = sort (abs (d0));
%! d0 = d0(idx);
%! old_state = rand ("state");
%! restore_state = onCleanup (@() rand ("state", old_state));
%! rand ("state", 42); # initialize generator to make eigs behavior reproducible
%!testif HAVE_ARPACK
%! d1 = eigs (A, k);
%! assert (d1, d0(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k+1);
%! assert (d1, d0(end:-1:(end-k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "lm");
%! assert (d1, d0(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! d1 = eigs (A, k, "sm");
%! assert (d1, d0(k:-1:1), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "la");
%! assert (d1, d2(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "sa");
%! assert (d1, d2(1:k), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "be");
%! assert (d1, d2([1:floor(k/2), (end - ceil(k/2) + 1):end]), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k+1, "be");
%! assert (d1, d2([1:floor((k+1)/2), (end - ceil((k+1)/2) + 1):end]), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! d1 = eigs (A, k, 4.1);
%! [~, idx0] = sort (abs (d0 - 4.1));
%! [~, idx1] = sort (abs (d1 - 4.1));
%! assert (d1(idx1), d0(idx0(1:k)), 1e-11);
%!testif HAVE_ARPACK, HAVE_CHOLMOD
%! d1 = eigs (A, speye (n), k, "lm");
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! assert (eigs (A, k, 4.1), eigs (A, speye (n), k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! d1 = eigs (A, speye (n), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, speye (n)(q,q), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! opts.cholB = true;
%! d1 = eigs (A, speye (n), k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, speye (n)(q,q), k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! assert (eigs (A, k, 4.1), eigs (A, speye (n), k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! fcn = @(x) A * x;
%! opts.issym = 1;  opts.isreal = 1;
%! d1 = eigs (fcn, n, k, "lm", opts);
%! assert (d1, d0(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK
%! fcn = @(x) A \ x;
%! opts.issym = 1;  opts.isreal = 1;
%! d1 = eigs (fcn, n, k, "sm", opts);
%! assert (d1, d0(k:-1:1), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! fcn = @(x) (A - 4.1 * eye (n)) \ x;
%! opts.issym = 1;  opts.isreal = 1;
%! d1 = eigs (fcn, n, k, 4.1, opts);
%! assert (d1, eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK, HAVE_CHOLMOD
%! AA = speye (10);
%! fcn = @(x) AA * x;
%! opts.issym = 1;  opts.isreal = 1;
%! assert (eigs (fcn, 10, AA, 3, "lm", opts), [1; 1; 1], 10*eps);
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "lm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! [v1,d1] = eigs (A, k, "sm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "la");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "sa");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "be");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor

## Real unsymmetric tests
%!shared n, k, A, d0, old_state, restore_state
%! n = 20;
%! k = 4;
%! A = sparse ([3:n,1:n,1:(n-2)],[1:(n-2),1:n,3:n],[ones(1,n-2),1:n,-ones(1,n-2)]);
%! d0 = eig (A);
%! [~, idx] = sort (abs (d0));
%! d0 = d0(idx);
%! old_state = rand ("state");
%! restore_state = onCleanup (@() rand ("state", old_state));
%! rand ("state", 42); # initialize generator to make eigs behavior reproducible
%!testif HAVE_ARPACK
%! d1 = eigs (A, k);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k+1);
%! assert (abs (d1), abs (d0(end:-1:(end-k))),1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "lm");
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! d1 = eigs (A, k, "sm");
%! assert (abs (d1), abs (d0(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "lr");
%! [~, idx] = sort (real (d0));
%! d2 = d0(idx);
%! assert (real (d1), real (d2(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "sr");
%! [~, idx] = sort (real (abs (d0)));
%! d2 = d0(idx);
%! assert (real (d1), real (d2(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "li");
%! [~, idx] = sort (imag (abs (d0)));
%! d2 = d0(idx);
%! assert (sort (imag (d1)), sort (imag (d2(end:-1:(end-k+1)))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "si");
%! [~, idx] = sort (imag (abs (d0)));
%! d2 = d0(idx);
%! assert (sort (imag (d1)), sort (imag (d2(1:k))), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! d1 = eigs (A, k, 4.1);
%! [~, idx0] = sort (abs (d0 - 4.1));
%! [~, idx1] = sort (abs (d1 - 4.1));
%! assert (abs (d1(idx1)), abs (d0(idx0(1:k))), 1e-11);
%! assert (sort (imag (d1(idx1))), sort (imag (d0(idx0(1:k)))), 1e-11);
%!testif HAVE_ARPACK, HAVE_CHOLMOD
%! d1 = eigs (A, speye (n), k, "lm");
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! d1 = eigs (A, speye (n), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, speye (n)(q,q), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! opts.cholB = true;
%! d1 = eigs (A, speye (n), k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, speye (n)(q,q), k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! assert (abs (eigs (A, k, 4.1)), abs (eigs (A, speye (n), k, 4.1)), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! assert (sort (imag (eigs (A, k, 4.1))),
%!         sort (imag (eigs (A, speye (n), k, 4.1))), 1e-11);
%!testif HAVE_ARPACK
%! fcn = @(x) A * x;
%! opts.issym = 0;  opts.isreal = 1;
%! d1 = eigs (fcn, n, k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! fcn = @(x) A \ x;
%! opts.issym = 0;  opts.isreal = 1;
%! d1 = eigs (fcn, n, k, "sm", opts);
%! assert (abs (d1), d0(1:k), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! fcn = @(x) (A - 4.1 * eye (n)) \ x;
%! opts.issym = 0;  opts.isreal = 1;
%! d1 = eigs (fcn, n, k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "lm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! [v1,d1] = eigs (A, k, "sm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "lr");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "sr");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "li");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "si");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor


## Complex hermitian tests
%!shared n, k, A, d0, old_state, restore_state
%! n = 20;
%! k = 4;
%! A = sparse ([3:n,1:n,1:(n-2)],[1:(n-2),1:n,3:n],[1i*ones(1,n-2),4*ones(1,n),-1i*ones(1,n-2)]);
%! d0 = eig (A);
%! [~, idx] = sort (abs (d0));
%! d0 = d0(idx);
%! old_state = rand ("state");
%! restore_state = onCleanup (@() rand ("state", old_state));
%! rand ("state", 42); # initialize generator to make eigs behavior reproducible
%!testif HAVE_ARPACK
%! d1 = eigs (A, k);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k+1);
%! assert (abs (d1), abs (d0(end:-1:(end-k))),1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "lm");
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! d1 = eigs (A, k, "sm");
%! assert (abs (d1), abs (d0(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "lr");
%! [~, idx] = sort (real (abs (d0)));
%! d2 = d0(idx);
%! assert (real (d1), real (d2(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "sr");
%! [~, idx] = sort (real (abs (d0)));
%! d2 = d0(idx);
%! assert (real (d1), real (d2(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "li");
%! [~, idx] = sort (imag (abs (d0)));
%! d2 = d0(idx);
%! assert (sort (imag (d1)), sort (imag (d2(end:-1:(end-k+1)))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "si");
%! [~, idx] = sort (imag (abs (d0)));
%! d2 = d0(idx);
%! assert (sort (imag (d1)), sort (imag (d2(1:k))), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! d1 = eigs (A, k, 4.1);
%! [~, idx0] = sort (abs (d0 - 4.1));
%! [~, idx1] = sort (abs (d1 - 4.1));
%! assert (abs (d1(idx1)), abs (d0(idx0(1:k))), 1e-11);
%! assert (sort (imag (d1(idx1))), sort (imag (d0(idx0(1:k)))), 1e-11);
%!testif HAVE_ARPACK, HAVE_CHOLMOD
%! d1 = eigs (A, speye (n), k, "lm");
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! d1 = eigs (A, speye (n), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, speye (n)(q,q), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! opts.cholB = true;
%! d1 = eigs (A, speye (n), k, 4.1, opts);
%! assert (abs (abs (d1)), abs (eigs (A, k, 4.1)), 1e-11);
%! assert (sort (imag (abs (d1))), sort (imag (eigs (A, k, 4.1))), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, speye (n)(q,q), k, 4.1, opts);
%! assert (abs (abs (d1)), abs (eigs (A, k, 4.1)), 1e-11);
%! assert (sort (imag (abs (d1))), sort (imag (eigs (A, k, 4.1))), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! assert (abs (eigs (A, k, 4.1)), abs (eigs (A, speye (n), k, 4.1)), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! assert (sort (imag (eigs (A, k, 4.1))),
%!         sort (imag (eigs (A, speye (n), k, 4.1))), 1e-11);
%!testif HAVE_ARPACK
%! fcn = @(x) A * x;
%! opts.issym = 0;  opts.isreal = 0;
%! d1 = eigs (fcn, n, k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! fcn = @(x) A \ x;
%! opts.issym = 0;  opts.isreal = 0;
%! d1 = eigs (fcn, n, k, "sm", opts);
%! assert (abs (d1), d0(1:k), 1e-11);
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! fcn = @(x) (A - 4.1 * eye (n)) \ x;
%! opts.issym = 0;  opts.isreal = 0;
%! d1 = eigs (fcn, n, k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "lm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK, HAVE_UMFPACK
%! [v1,d1] = eigs (A, k, "sm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "lr");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "sr");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "li");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "si");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*speye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK, HAVE_UMFPACK, HAVE_CHOLMOD
%! A = toeplitz (sparse (1:10));
%! B = toeplitz (sparse ([1, 1], [1, 2], [2, 1], 1, 10));
%! [v, d] = eigs (A, B, 4, "lm");
%! for i = 1:4
%!   assert (A * v(:,i), d(i, i) * B * v(:,i), 1e-12);
%! endfor
%! ddiag = diag (d);
%! [ddiag, idx] = sort (ddiag);
%! v = v(:, idx);
%! R = chol (B);
%! [v1, d1] = eigs (R' \ A / R, 4, "lm");
%! d1diag = diag (d1);
%! [d1diag, idx] = sort (d1diag);
%! v1 = v1(:, idx);
%! assert (abs (v), abs (R \ v1), 1e-12);
%!testif HAVE_ARPACK, HAVE_UMFPACK, HAVE_CHOLMOD
%! A = spdiags ([[1./(2:11)]',[-5:-2:-23]',[1:10]'],-1:1,10,10);
%! B = toeplitz (sparse ([1, 1], [1, 2], [2, 1], 1, 10));
%! [v, d] = eigs (A, B, 4, "lm");
%! for i = 1:4
%!   assert (A * v(:,i), d(i, i) * B * v(:,i), 1e-12);
%! endfor
%! ddiag = diag (d);
%! [ddiag, idx] = sort (ddiag);
%! v = v(:, idx);
%! R = chol (B);
%! [v1, d1] = eigs (R' \ A / R, 4, "lm");
%! d1diag = diag (d1);
%! [d1diag, idx] = sort (d1diag);
%! v1 = v1(:, idx);
%! assert (abs (v), abs (R \ v1), 1e-12);
%!testif HAVE_ARPACK, HAVE_UMFPACK, HAVE_CHOLMOD
%! A = spdiags ([[1./(2:11)]',[-5:-2:-23]',[1:10]'],-1:1,10,10) -...
%! 1i * spdiags ([[1./(2:11)]',[-5:-2:-23]',[1:10]'],-1:1,10,10);
%! B = toeplitz (sparse ([1, 1], [1, 2], [2, 1], 1, 10));
%! [v, d] = eigs (A, B, 4, "lm");
%! for i = 1:4
%!   assert (A * v(:,i), d(i, i) * B * v(:,i), 1e-12);
%! endfor
%! ddiag = diag (d);
%! [ddiag, idx] = sort (ddiag);
%! v = v(:, idx);
%! R = chol (B);
%! [v1, d1] = eigs (R' \ A / R, 4, "lm");
%! d1diag = diag (d1);
%! [d1diag, idx] = sort (d1diag);
%! v1 = v1(:, idx);
%! assert (abs (v), abs (R \ v1), 1e-12);
%!testif HAVE_ARPACK, HAVE_UMFPACK, HAVE_CHOLMOD
%! A = toeplitz (sparse (1:10));
%! B = toeplitz (sparse ([1, 1], [1, 2], [2, 1], 1, 10));
%! [v, d] = eigs (A, B, 4, 1);
%! for i = 1:4
%!   assert (A * v(:,i), d(i, i) * B * v(:,i), 1e-12);
%! endfor
%! ddiag = diag (d);
%! [ddiag, idx] = sort (ddiag);
%! v = v(:, idx);
%! R = chol (B);
%! [v1, d1] = eigs (R' \ A / R, 4, 1);
%! d1diag = diag (d1);
%! [d1diag, idx] = sort (d1diag);
%! v1 = v1(:, idx);
%! assert (abs (v), abs (R \ v1), 1e-12);
%!testif HAVE_ARPACK, HAVE_UMFPACK, HAVE_CHOLMOD
%! A = spdiags ([[1./(2:11)]',[-5:-2:-23]',[1:10]'],-1:1,10,10);
%! B = toeplitz (sparse ([1, 1], [1, 2], [2, 1], 1, 10));
%! [v, d] = eigs (A, B, 4, 1);
%! for i = 1:4
%!   assert (A * v(:,i), d(i, i) * B * v(:,i), 1e-12);
%! endfor
%! ddiag = diag (d);
%! [ddiag, idx] = sort (ddiag);
%! v = v(:, idx);
%! R = chol (B);
%! [v1, d1] = eigs (R' \ A / R, 4, 1);
%! d1diag = diag (d1);
%! [d1diag, idx] = sort (d1diag);
%! v1 = v1(:, idx);
%! assert (abs (v), abs (R \ v1), 1e-12);
%!testif HAVE_ARPACK, HAVE_UMFPACK, HAVE_CHOLMOD
%! A = spdiags ([[1./(2:11)]',[-5:-2:-23]',[1:10]'],-1:1,10,10) -...
%! 1i * spdiags ([[1./(2:11)]',[-5:-2:-23]',[1:10]'],-1:1,10,10);
%! B = toeplitz (sparse ([1, 1], [1, 2], [2, 1], 1, 10));
%! [v, d] = eigs (A, B, 4, 1);
%! for i = 1:4
%!   assert (A * v(:,i), d(i, i) * B * v(:,i), 1e-12);
%! endfor
%! ddiag = diag (d);
%! [ddiag, idx] = sort (ddiag);
%! v = v(:, idx);
%! R = chol (B);
%! [v1, d1] = eigs (R' \ A / R, 4, 1);
%! d1diag = diag (d1);
%! [d1diag, idx] = sort (d1diag);
%! v1 = v1(:, idx);
%! assert (abs (v), abs (R \ v1), 1e-12);
%!testif HAVE_ARPACK, HAVE_UMFPACK, HAVE_CHOLMOD
%! A = toeplitz (sparse (1:10));
%! B = toeplitz (sparse ([1, 1], [1, 2], [2, 1], 1, 10));
%! R = chol (B);
%! opts.cholB = true;
%! [v, d] = eigs (A, R, 4, "lm", opts);
%! for i = 1:4
%!   assert (A * v(:,i), d(i, i) * B * v(:,i), 1e-12);
%! endfor
%!testif HAVE_ARPACK, HAVE_UMFPACK, HAVE_CHOLMOD
%! A = toeplitz (sparse (1:10));
%! B = toeplitz (sparse ([1, 1], [1, 2], [2, 1], 1, 10));
%! [R, ~, permB] = chol (B, "vector");
%! opts.cholB = true;
%! opts.permB = permB;
%! [v, d] = eigs (A, R, 4, "lm", opts);
%! for i = 1:4
%!   assert (A * v(:,i), d(i, i) * B * v(:,i), 1e-12);
%! endfor

### FULL MATRIX TESTS ###

## Real positive definite tests, n must be even
%!shared n, k, A, d0, d2, old_state, restore_state
%! n = 20;
%! k = 4;
%! A = full (sparse ([3:n,1:n,1:(n-2)],[1:(n-2),1:n,3:n],[ones(1,n-2),4*ones(1,n),ones(1,n-2)]));
%! d0 = eig (A);
%! d2 = sort (d0);
%! [~, idx] = sort (abs (d0));
%! d0 = d0(idx);
%! old_state = rand ("state");
%! restore_state = onCleanup (@() rand ("state", old_state));
%! rand ("state", 42); # initialize generator to make eigs behavior reproducible
%!testif HAVE_ARPACK
%! d1 = eigs (A, k);
%! assert (d1, d0(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k+1);
%! assert (d1, d0(end:-1:(end-k)),1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "lm");
%! assert (d1, d0(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "sm");
%! assert (d1, d0(k:-1:1), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "la");
%! assert (d1, d2(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "sa");
%! assert (d1, d2(1:k), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "be");
%! assert (d1, d2([1:floor(k/2), (end - ceil(k/2) + 1):end]), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k+1, "be");
%! assert (d1, d2([1:floor((k+1)/2), (end - ceil((k+1)/2) + 1):end]), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 4.1);
%! [~, idx0] = sort (abs (d0 - 4.1));
%! [~, idx1] = sort (abs (d1 - 4.1));
%! assert (d1(idx1), d0(idx0(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, eye (n), k, "lm");
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! assert (eigs (A, k, 4.1), eigs (A, eye (n), k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! d1 = eigs (A, eye (n), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, eye (n)(q,q), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! d1 = eigs (A, eye (n), k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, eye (n)(q,q), k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! assert (eigs (A, k, 4.1), eigs (A, eye (n), k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! fcn = @(x) A * x;
%! opts.issym = 1;  opts.isreal = 1;
%! d1 = eigs (fcn, n, k, "lm", opts);
%! assert (d1, d0(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK
%! fcn = @(x) A \ x;
%! opts.issym = 1;  opts.isreal = 1;
%! d1 = eigs (fcn, n, k, "sm", opts);
%! assert (d1, d0(k:-1:1), 1e-11);
%!testif HAVE_ARPACK
%! fcn = @(x) (A - 4.1 * eye (n)) \ x;
%! opts.issym = 1;  opts.isreal = 1;
%! d1 = eigs (fcn, n, k, 4.1, opts);
%! assert (d1, eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "lm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "sm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "la");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "sa");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "be");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor

## Real unsymmetric tests
%!shared n, k, A, d0, old_state, restore_state
%! n = 20;
%! k = 4;
%! A = full (sparse ([3:n,1:n,1:(n-2)],[1:(n-2),1:n,3:n],[ones(1,n-2),1:n,-ones(1,n-2)]));
%! d0 = eig (A);
%! [~, idx] = sort (abs (d0));
%! d0 = d0(idx);
%! old_state = rand ("state");
%! restore_state = onCleanup (@() rand ("state", old_state));
%! rand ("state", 42); # initialize generator to make eigs behavior reproducible
%!testif HAVE_ARPACK
%! d1 = eigs (A, k);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k+1);
%! assert (abs (d1), abs (d0(end:-1:(end-k))),1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "lm");
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "sm");
%! assert (abs (d1), abs (d0(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "lr");
%! [~, idx] = sort (real (d0));
%! d2 = d0(idx);
%! assert (real (d1), real (d2(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "sr");
%! [~, idx] = sort (real (abs (d0)));
%! d2 = d0(idx);
%! assert (real (d1), real (d2(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "li");
%! [~, idx] = sort (imag (abs (d0)));
%! d2 = d0(idx);
%! assert (sort (imag (d1)), sort (imag (d2(end:-1:(end-k+1)))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "si");
%! [~, idx] = sort (imag (abs (d0)));
%! d2 = d0(idx);
%! assert (sort (imag (d1)), sort (imag (d2(1:k))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 4.1);
%! [~, idx0] = sort (abs (d0 - 4.1));
%! [~, idx1] = sort (abs (d1 - 4.1));
%! assert (abs (d1(idx1)), abs (d0(idx0(1:k))), 1e-11);
%! assert (sort (imag (d1(idx1))), sort (imag (d0(idx0(1:k)))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, eye (n), k, "lm");
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! d1 = eigs (A, eye (n), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, eye (n)(q,q), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! d1 = eigs (A, eye (n), k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, eye (n)(q,q), k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! assert (abs (eigs (A, k, 4.1)), abs (eigs (A, eye (n), k, 4.1)), 1e-11);
%!testif HAVE_ARPACK
%! assert (sort (imag (eigs (A, k, 4.1))),
%!         sort (imag (eigs (A, eye (n), k, 4.1))), 1e-11);
%!testif HAVE_ARPACK
%! fcn = @(x) A * x;
%! opts.issym = 0;  opts.isreal = 1;
%! d1 = eigs (fcn, n, k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! fcn = @(x) A \ x;
%! opts.issym = 0;  opts.isreal = 1;
%! d1 = eigs (fcn, n, k, "sm", opts);
%! assert (abs (d1), d0(1:k), 1e-11);
%!testif HAVE_ARPACK
%! fcn = @(x) (A - 4.1 * eye (n)) \ x;
%! opts.issym = 0;  opts.isreal = 1;
%! d1 = eigs (fcn, n, k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "lm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "sm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "lr");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "sr");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "li");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "si");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor

## Complex hermitian tests
%!shared n, k, A, d0, old_state, restore_state
%! n = 20;
%! k = 4;
%! A = full (sparse ([3:n,1:n,1:(n-2)],[1:(n-2),1:n,3:n],[1i*ones(1,n-2),4*ones(1,n),-1i*ones(1,n-2)]));
%! d0 = eig (A);
%! [~, idx] = sort (abs (d0));
%! d0 = d0(idx);
%! old_state = rand ("state");
%! restore_state = onCleanup (@() rand ("state", old_state));
%! rand ("state", 42); # initialize generator to make eigs behavior reproducible
%!testif HAVE_ARPACK
%! d1 = eigs (A, k);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k+1);
%! assert (abs (d1), abs (d0(end:-1:(end-k))),1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "lm");
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "sm");
%! assert (abs (d1), abs (d0(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "lr");
%! [~, idx] = sort (real (abs (d0)));
%! d2 = d0(idx);
%! assert (real (d1), real (d2(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "sr");
%! [~, idx] = sort (real (abs (d0)));
%! d2 = d0(idx);
%! assert (real (d1), real (d2(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "li");
%! [~, idx] = sort (imag (abs (d0)));
%! d2 = d0(idx);
%! assert (sort (imag (d1)), sort (imag (d2(end:-1:(end-k+1)))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, "si");
%! [~, idx] = sort (imag (abs (d0)));
%! d2 = d0(idx);
%! assert (sort (imag (d1)), sort (imag (d2(1:k))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 4.1);
%! [~, idx0] = sort (abs (d0 - 4.1));
%! [~, idx1] = sort (abs (d1 - 4.1));
%! assert (abs (d1(idx1)), abs (d0(idx0(1:k))), 1e-11);
%! assert (sort (imag (d1(idx1))), sort (imag (d0(idx0(1:k)))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, eye (n), k, "lm");
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! d1 = eigs (A, eye (n), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, eye (n)(q,q), k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! d1 = eigs (A, eye (n), k, 4.1, opts);
%! assert (abs (abs (d1)), abs (eigs (A, k, 4.1)), 1e-11);
%! assert (sort (imag (abs (d1))), sort (imag (eigs (A, k, 4.1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB = true;
%! q = [2:n,1];
%! opts.permB = q;
%! d1 = eigs (A, eye (n)(q,q), k, 4.1, opts);
%! assert (abs (abs (d1)), abs (eigs (A, k, 4.1)), 1e-11);
%! assert (sort (imag (abs (d1))), sort (imag (eigs (A, k, 4.1))), 1e-11);
%!testif HAVE_ARPACK
%! assert (abs (eigs (A, k, 4.1)), abs (eigs (A, eye (n), k, 4.1)), 1e-11);
%!testif HAVE_ARPACK
%! assert (sort (imag (eigs (A, k, 4.1))),
%!         sort (imag (eigs (A, eye (n), k, 4.1))), 1e-11);
%!testif HAVE_ARPACK
%! fcn = @(x) A * x;
%! opts.issym = 0;  opts.isreal = 0;
%! d1 = eigs (fcn, n, k, "lm", opts);
%! assert (abs (d1), abs (d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! fcn = @(x) A \ x;
%! opts.issym = 0;  opts.isreal = 0;
%! d1 = eigs (fcn, n, k, "sm", opts);
%! assert (abs (d1), d0(1:k), 1e-11);
%!testif HAVE_ARPACK
%! fcn = @(x) (A - 4.1 * eye (n)) \ x;
%! opts.issym = 0;  opts.isreal = 0;
%! d1 = eigs (fcn, n, k, 4.1, opts);
%! assert (abs (d1), eigs (A, k, 4.1), 1e-11);
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "lm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "sm");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "lr");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "sr");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "li");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "si");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs (A, k, "li");
%! d1 = diag (d1);
%! for i=1:k
%!   assert (max (abs ((A - d1(i)*eye (n))*v1(:,i))), 0, 1e-11);
%! endfor

%!testif HAVE_ARPACK
%! A = 2 * diag (ones (10, 1)) - diag (ones (9, 1), 1) - diag (ones (9, 1), -1);
%! B = eye (10);
%! reseig = eig (A, B);
%! [~, idx] = sort (abs (reseig), "ascend");
%! assert (eigs (A, B, 4, 0), reseig (idx(4:-1:1)), 8 * eps);
%!testif HAVE_ARPACK
%! A = eye (9);
%! A(1, 1) = 0;
%! A(1, 9) = 1;
%! [V, L] = eigs (A, 4, -1);
%! assert (! any (isnan (diag (L))));
%! assert (any (abs (diag (L)) <= 2 * eps));
%!testif HAVE_ARPACK
%! A = diag (ones (9, 1), 1);
%! A(10,:) = [-1, zeros(1, 8), -1];
%! opts.v0 = (1:10)';
%! typ = "lr";
%! [v, m] = eigs (A, 4, typ, opts);
%! assert (sort (real (diag (m))), ...
%!         [0.514038; 0.514038; 0.880290; 0.880290], 1e-4);
%! m = eigs (A, 4, typ, opts);
%! assert (sort (real (m)), ...
%!         [0.514038; 0.514038; 0.880290; 0.880290], 1e-4);
%! typ = "li";
%! [v, m] = eigs (A, 4, typ, opts);
%! assert (sort (abs (imag (diag (m)))), ...
%!         [0.78972; 0.78972; 0.96518; 0.96518], 1e-4);
%! m = eigs (A, 4, typ, opts);
%! assert (sort (abs (imag (m))), ...
%!         [0.78972; 0.78972; 0.96518; 0.96518], 1e-4);
%! typ = "sr";
%! [v, m] = eigs (A, 4, typ, opts);
%! assert (sort (real (diag (m))), ...
%!         [-1.12180; -1.12180; -0.69077; -0.69077], 1e-4);
%! m = eigs (A, 4, typ, opts);
%! assert (sort (real (m)), ...
%!         [-1.12180; -1.12180; -0.69077; -0.69077], 1e-4);
%! typ = "si";
%! [v, m] = eigs (A, 4, typ, opts);
%! assert (sort (abs (imag (diag (m)))), ...
%!         [0.25552; 0.25552; 0.30282; 0.30282], 1e-4);
%! m = eigs (A, 4, typ, opts);
%! assert (sort (abs (imag (m))), ...
%!         [0.25552; 0.25552; 0.30282; 0.30282], 1e-4);
%! typ = "lm";
%! [v, m] = eigs (A, 4, typ, opts);
%! assert (sort (abs (diag (m))), ...
%!         [1.02294;  1.02294; 1.15054; 1.15054], 1e-4);
%! m = eigs (A, 4, typ, opts);
%! assert (sort (abs (m)), ...
%!         [1.02294; 1.02294; 1.15054; 1.15054], 1e-4);
%! typ = "sm";
%! [v, m] = eigs (A, 4, typ, opts);
%! assert (sort (abs (diag (m))), ...
%!         [0.93092; 0.93092; 0.94228; 0.94228], 1e-4);
%! m = eigs (A, 4, typ, opts);
%! assert (sort (abs (m)), ...
%!         [0.93092; 0.93092; 0.94228; 0.94228], 1e-4);
%!testif HAVE_ARPACK
%! A = toeplitz (sparse ([2, 1, zeros(1,8)]));
%! opts.v0 = (1:10)';
%! [v, m] = eigs (A, 3, "sa", opts);
%! assert (diag (m), [0.081014; 0.317493; 0.690279], 1e-4);
%! m = eigs (A, 3, "sa", opts);
%! assert (m, [0.081014; 0.317493; 0.690279], 1e-4);

%!test
%! X = [70 47 42 39 50 73 79 23;
%!      19 52 61 80 36 76 63 68;
%!      14 34 66 65 29  4 72  9;
%!      24  8 78 49 58 54 43 33;
%!      62 69 32 31 40 46 22 28;
%!      48 12 45 59 10 17 15 25;
%!      64 67 77 56 13 55 41 74;
%!      37 38 18 21 11  3 71  7;
%!       5 35 16  1 51 27 26 44;
%!      30 57 60 75  2 53 20  6];
%! Z = X * X';
%! r = rank (Z);
%! assert (r, 8);
%! [V, D] = eigs (Z, r, "lm"); # call_eig is true
%! ZZ = V * D * V';
%! tmp = abs (Z - ZZ);
%! assert (max (tmp(:)) < 5e-11);

%!assert (eigs (diag (1:5), 5, "sa"), [1;2;3;4;5]) # call_eig is true
%!assert (eigs (diag (1:5), 5, "la"), [5;4;3;2;1]) # call_eig is true
%!assert (eigs (diag (1:5), 3, "be"), [1;4;5]) # call_eig is true
%!testif HAVE_ARPACK
%! A = toeplitz ([-2, 1, zeros(1, 8)]);
%! A = kron (A, eye (10)) + kron (eye (10), A);
%! opts.v0 = (1:100)';
%! opts.maxit = 3;
%! warning ("off", "Octave:eigs:UnconvergedEigenvalues", "local");
%! d = eigs (A, 4, "lm", opts);
%! assert (d(3:4), [NaN; NaN]);
%!testif HAVE_ARPACK
%! A = toeplitz ([-2, 1, zeros(1, 8)]);
%! A = kron (A, eye (10)) + kron (eye (10), A);
%! opts.v0 = (1:100)';
%! opts.maxit = 1;
%! warning ("off", "Octave:eigs:UnconvergedEigenvalues", "local");
%! d = eigs (A, 4, "sm", opts);
%! assert (d(4), NaN);
%!testif HAVE_ARPACK
%! A = toeplitz ([-2, 1, zeros(1, 8)]);
%! A = kron (A, eye (10)) + kron (eye (10), A);
%! Afcn = @(x) A * x;
%! opts.v0 = (1:100)';
%! opts.maxit = 3;
%! opts.issym = true;
%! warning ("off", "Octave:eigs:UnconvergedEigenvalues", "local");
%! d = eigs (Afcn, 100, 4, "sm", opts);
%! assert (d(3:4), [NaN; NaN]);
%!testif HAVE_ARPACK
%! A = toeplitz ([-2, 1, zeros(1, 8)]);
%! A = kron (A, eye (10)) + kron (eye (10), A);
%! A(1, 2) = 10;
%! opts.v0 = (1:100)';
%! opts.maxit = 5;
%! warning ("off", "Octave:eigs:UnconvergedEigenvalues", "local");
%! d = eigs (A, 4, "lm", opts);
%! assert (d(3:4), [NaN; NaN]);
%!testif HAVE_ARPACK
%! A = toeplitz ([0, 1, zeros(1, 8)], [0, -1, zeros(1, 8)]);
%! A = kron (A, eye (10)) + kron (eye (10), A);
%! opts.v0 = (1:100)';
%! opts.maxit = 4;
%! warning ("off", "Octave:eigs:UnconvergedEigenvalues", "local");
%! d = eigs (A, 4, "lm", opts);
%! assert (d(3:4), [NaN+1i*NaN; NaN+1i*NaN]);
%!testif HAVE_ARPACK
%! A = magic (100);
%! opts.v0 = (1:100)';
%! opts.maxit = 1;
%! warning ("off", "Octave:eigs:UnconvergedEigenvalues", "local");
%! d = eigs (A, 10, "lm", opts);
%! assert (d(9:10), [NaN; NaN]);
%!testif HAVE_ARPACK
%! A = toeplitz ([0, 1, zeros(1, 8)], [0, -1, zeros(1, 8)]);
%! A(1, 1) = 1;
%! A = kron (A, eye (10)) + kron (eye (10), A);
%! opts.v0 = (1:100)';
%! opts.maxit = 1;
%! warning ("off", "Octave:eigs:UnconvergedEigenvalues", "local");
%! d = eigs (A, 4, "sm", opts);
%! if (isreal (d))
%!   assert (d(4), NaN);
%! else
%!   assert (d(4), NaN +1i*NaN);
%! endif
%!testif HAVE_ARPACK
%! A = magic (100) / 10 + eye (100);
%! opts.v0 = (1:100)';
%! opts.maxit = 10;
%! warning ("off", "Octave:eigs:UnconvergedEigenvalues", "local");
%! d = eigs (A, 10, "sm", opts);
%! if (isreal (d))
%!   assert (d(10), NaN);
%! else
%!   assert (d(10), NaN +1i*NaN);
%! endif
%!testif HAVE_ARPACK
%! A = toeplitz ([0, 1, zeros(1, 8)], [0, -1, zeros(1, 8)]);
%! A = kron (A, eye (10)) + kron (eye (10), A);
%! Afcn = @(x) A * x;
%! opts.v0 = (1:100)';
%! opts.maxit = 4;
%! warning ("off", "Octave:eigs:UnconvergedEigenvalues", "local");
%! d = eigs (Afcn, 100, 4, "lm", opts);
%! assert (d(3:4), [NaN+1i*NaN; NaN+1i*NaN]);
%!testif HAVE_ARPACK
%! A = 1i * magic (100);
%! opts.v0 = (1:100)';
%! opts.maxit = 1;
%! warning ("off", "Octave:eigs:UnconvergedEigenvalues", "local");
%! d = eigs (A, 5, "lm", opts);
%! assert (d(5), NaN+1i*NaN);
%!testif HAVE_ARPACK
%! A = 1i * magic (100) + eye (100);
%! opts.v0 = (1:100)';
%! opts.maxit = 7;
%! warning ("off", "Octave:eigs:UnconvergedEigenvalues", "local");
%! d = eigs (A, 10, "sm", opts);
%! assert (d(9:10), [NaN+1i*NaN; NaN+1i*NaN]);
%!testif HAVE_ARPACK
%! A = 1i * magic (100);
%! Afcn = @(x) A * x;
%! opts.v0 = (1:100)';
%! opts.maxit = 1;
%! opts.isreal = false;
%! warning ("off", "Octave:eigs:UnconvergedEigenvalues", "local");
%! d = eigs (Afcn, 100, 6, "lm", opts);
%! assert (d(6), NaN+1i*NaN);
%!testif HAVE_ARPACK, HAVE_CHOLMOD
%! A = sparse (magic (10));
%! B = sparse (magic (10)); # not HPD
%! fail ("eigs (A, B, 4)", "eigs: The matrix B is not positive definite")
%!testif HAVE_ARPACK
%! i_A = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
%! j_A = [1, 2, 3, 4, 5, 6, 7,  8, 9, 10];
%! v_A = [1, 2i, 3, 4i, 5, 6i, 7, 8, 9, 10i];
%! A = sparse (i_A, j_A, v_A);
%! i_B = [1,2, 3, 4, 5, 6, 7, 8, 9, 10];
%! j_B = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
%! v_B = [3, 10i, 1, 8i, 7, 6i, 5, 4i, 9, 7i];
%! B = sparse (i_B, j_B, v_B); # not SPD
%! [Evectors, Evalues] = eigs(A, B, 5, "SM"); # call_eig is true
%! ResidualVectors = A * Evectors - B * Evectors * Evalues;
%! RelativeErrors = norm (ResidualVectors, "columns") ./ ...
%! norm (A * Evectors, "columns");
%! assert (RelativeErrors, zeros (1, 5));
%!testif HAVE_ARPACK
%! A = rand (8);
%! eigs (A, 6, "lr"); # this failed on 4.2.x
%!testif HAVE_ARPACK
%! M = magic (10);
%! A = sin (M);
%! B = cos (M);
%! B = B * B';
%! opts.v0 = (1:10)';
%! [Evector, Evalues] = eigs (A, B, 4, "LM", opts);
%! Afcn = @(x) A * x;
%! [Evector_f Evalues_f] = eigs (Afcn, 10, B, 4, "LM", opts);
%! assert (Evector, Evector_f);
%! assert (Evalues, Evalues_f);
%!testif HAVE_ARPACK
%! M = magic (10);
%! A = sin (M);
%! B = cos (M);
%! B = B * B';
%! opts.v0 = (1:10)';
%! [Evector, Evalues] = eigs (A, B, 4, "SM", opts);
%! [L, U, P] = lu (A);
%! Afcn = @(x) U \ (L \ (P * x));
%! [Evector_f Evalues_f] = eigs (Afcn, 10, B, 4, "SM", opts);
%! assert (Evector, Evector_f);
%! assert (Evalues, Evalues_f);
%!testif HAVE_ARPACK
%! M = magic (10);
%! A = sin (M);
%! A = A * A';
%! B = cos (M);
%! B = B * B';
%! opts.v0 = (1:10)';
%! [Evector, Evalues] = eigs (A, B, 4, "LM", opts);
%! Afcn = @(x) A * x;
%! opts.issym = true;
%! [Evector_f Evalues_f] = eigs (Afcn, 10, B, 4, "LM", opts);
%! assert (Evector, Evector_f);
%! assert (Evalues, Evalues_f);
%!testif HAVE_ARPACK
%! M = magic (10);
%! A = sin (M);
%! A = A * A';
%! B = cos (M);
%! B = B * B';
%! opts.v0 = (1:10)';
%! [Evector, Evalues] = eigs (A, B, 4, "SM", opts);
%! [L, U, P] = lu (A);
%! Afcn = @(x) U \ (L \ (P * x));
%! opts.issym = true;
%! [Evector_f Evalues_f] = eigs (Afcn, 10, B, 4, "SM", opts);
%! assert (Evector, Evector_f);
%! assert (Evalues, Evalues_f);
%!testif HAVE_ARPACK
%! M = magic (10);
%! A = sin (M) + 1i * cos (M);
%! B = cos (M) + 1i * sin (M);
%! B = B * B';
%! opts.v0 = (1:10)';
%! [Evector, Evalues] = eigs (A, B, 4, "LM", opts);
%! Afcn = @(x) A * x;
%! opts.isreal = false;
%! [Evector_f Evalues_f] = eigs (Afcn, 10, B, 4, "LM", opts);
%! assert (Evector, Evector_f);
%! assert (Evalues, Evalues_f);
%!testif HAVE_ARPACK
%! M = magic (10);
%! A = sin (M) + 1i * cos (M);
%! B = cos (M) + 1i * sin (M);
%! B = B * B';
%! opts.v0 = (1:10)';
%! [Evector, Evalues] = eigs (A, B, 4, "SM", opts);
%! [L, U, P] = lu (A);
%! Afcn = @(x) U \ (L \ (P *x));
%! opts.isreal = false;
%! [Evector_f, Evalues_f] = eigs (Afcn, 10, B, 4, "SM", opts);
%! assert (Evector, Evector_f);
%! assert (Evalues, Evalues_f);

%!testif HAVE_ARPACK <*57196>
%! x = ones (10, 10);
%! z = complex (x, x);
%! A = [sparse(10,10), z; z', sparse(10,10)];
%! d = eigs (A);
%! assert (isreal (d));
%! [~, d] = eigs (A);
%! assert (isreal (d));

%!testif HAVE_ARPACK <*59486>
%! A = magic (5);
%! d = eigs (A, [], 1);
%! assert (d, 65, 5 * eps (65));

%!testif HAVE_ARPACK <*59488>
%! A = zeros (20);
%! d = eigs (A, 4);
%! assert (d, zeros (4, 1));
%! [V, d, flag] = eigs (A, 4);
%! Vexp = zeros (20, 4);
%! Vexp(sub2ind (size (Vexp), 1:4, 1:4)) = 1;
%! assert (V, Vexp);
%! assert (d, diag (zeros (4,1)));
%! assert (flag, 0.0);

## Test input validation
%!error <Invalid call> eigs ()
%!error <second argument must be numeric> eigs (1, "foobar")
%!error <requested number of eigenvalues K \(2\) exceeds available eigenvalues \(1\)>
%! eigs (1, [], 2);
%!error <"la" requires real symmetric problem> eigs ([i,0;0,1], 1, "la")
%!error <"la" requires real symmetric problem> eigs ([1,1;0,1], 1, "la")
%!error <"sa" requires real symmetric problem> eigs ([i,0;0,1], 1, "sa")
%!error <"sa" requires real symmetric problem> eigs ([1,1;0,1], 1, "sa")
%!error <"be" requires real symmetric problem> eigs ([i,0;0,1], 1, "be")
%!error <"be" requires real symmetric problem> eigs ([1,1;0,1], 1, "be")
%!error <"lr" requires complex or unsymmetric> eigs ([1,0;0,1], 1, "lr")
%!error <"sr" requires complex or unsymmetric> eigs ([1,0;0,1], 1, "sr")
%!error <"li" requires complex or unsymmetric> eigs ([1,0;0,1], 1, "li")
%!error <"si" requires complex or unsymmetric> eigs ([1,0;0,1], 1, "si")
%!error <unrecognized value for SIGMA: foobar> eigs (eye (2), 1, "foobar")
%!testif HAVE_ARPACK
%! A = rand (10);
%! opts.v0 = ones (8, 1);
%! fail ("eigs (A, 4, 'sm', opts)", "opts.v0 must be n-by-1");
