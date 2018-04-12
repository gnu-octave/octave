## Copyright (C) 1996-2018 John W. Eaton
## Copyright (C) 2009 VZLU Prague
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

## -*- texinfo -*-
## @deftypefn  {} {} issymmetric (@var{A})
## @deftypefnx {} {} issymmetric (@var{A}, @var{tol})
## @deftypefnx {} {} issymmetric (@var{A}, @qcode{"skew"})
## @deftypefnx {} {} issymmetric (@var{A}, @qcode{"skew"}, @var{tol})
## Return true if @var{A} is a symmetric or skew-symmetric matrix within the
## tolerance specified by @var{tol}.
##
## The default tolerance is zero (uses faster code).
##
## The type of symmetry to check may be specified with the additional input
## @qcode{"nonskew"} (default) for regular symmetry or @qcode{"skew"} for
## skew-symmetry.
##
## Background: A matrix is symmetric if the transpose of the matrix is equal
## to the original matrix: @w{@code{@var{A} == @var{A}.'}}.  If a tolerance
## is given then symmetry is determined by
## @code{norm (@var{A} - @var{A}.', Inf) / norm (@var{A}, Inf) < @var{tol}}.
##
## A matrix is skew-symmetric if the transpose of the matrix is equal to the
## negative of the original matrix: @w{@code{@var{A} == -@var{A}.'}}.  If a
## tolerance is given then skew-symmetry is determined by
## @code{norm (@var{A} + @var{A}.', Inf) / norm (@var{A}, Inf) < @var{tol}}.
## @seealso{ishermitian, isdefinite}
## @end deftypefn

## Author: A. S. Hodel <scotte@eng.auburn.edu>
## Created: August 1993
## Adapted-By: jwe

function retval = issymmetric (A, skewopt = "nonskew", tol = 0)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (nargin == 2)
    ## Decode whether second argument is skewopt or tol
    if (isnumeric (skewopt))
      tol = skewopt;
      skewopt = "nonskew";
    elseif (! ischar (skewopt))
      error ("issymmetric: second argument must be a non-negative scalar TOL, or one of the strings: 'skew' / 'nonskew'");
    endif
  endif

  ## Validate inputs
  retval = (isnumeric (A) || islogical (A)) && issquare (A);
  if (! retval)
    return;
  endif

  if (! (strcmp (skewopt, "skew") || strcmp (skewopt, "nonskew")))
    error ("issymmetric: SKEWOPT must be 'skew' or 'nonskew'");
  endif

  if (! (isnumeric (tol) && isscalar (tol) && tol >= 0))
    error ("issymmetric: TOL must be a scalar >= 0");
  endif

  ## Calculate symmetry
  if (strcmp (skewopt, "nonskew"))
    if (tol == 0)
      ## check for exact symmetry
      retval = ! any ((A != A.')(:));
    else
      norm_x = norm (A, Inf);
      retval = norm_x == 0 || norm (A - A.', Inf) / norm_x <= tol;
    endif
  else
    ## skew symmetry
    if (tol == 0)
      retval = ! any ((A != -A.')(:));
    else
      norm_x = norm (A, Inf);
      retval = norm_x == 0 || norm (A + A.', Inf) / norm_x <= tol;
    endif
  endif

endfunction


%!assert (issymmetric (1))
%!assert (! issymmetric ([1, 2]))
%!assert (issymmetric ([]))
%!assert (issymmetric ([1, 2; 2, 1]))
%!assert (issymmetric ([1, 2.1; 2, 1.1], 0.2))
%!assert (issymmetric ([1, 2i; 2i, 1]))
%!assert (issymmetric (speye (100)))
%!assert (issymmetric (logical (eye (2))))
%!assert (issymmetric ([0, 2; -2, 0], "skew"))
%!assert (! issymmetric ([0, 2; -2, eps], "skew"))
%!assert (issymmetric ([0, 2; -2, eps], "skew", eps))

%!assert (! (issymmetric ("test")))
%!assert (! (issymmetric ("t")))
%!assert (! (issymmetric (["te"; "et"])))
%!assert (! issymmetric ({1}))
%!test
%! s.a = 1;
%! assert (! issymmetric (s));

## Test input validation
%!error issymmetric ()
%!error issymmetric (1,2,3,4)
%!error <second argument must be> issymmetric (1, {"skew"})
%!error <SKEWOPT must be 'skew' or 'nonskew'> issymmetric (1, "foobar")
%!error <SKEWOPT must be 'skew' or 'nonskew'> issymmetric (1, "foobar")
%!error <TOL must be a scalar .= 0> issymmetric (1, "skew", {1})
%!error <TOL must be a scalar .= 0> issymmetric (1, "skew", [1 1])
%!error <TOL must be a scalar .= 0> issymmetric (1, -1)
