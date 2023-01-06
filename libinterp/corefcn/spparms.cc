////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1998-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "defun.h"
#include "ov.h"
#include "pager.h"
#include "error.h"
#include "errwarn.h"

#include "oct-spparms.h"

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (spparms, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} { } spparms ()
@deftypefnx {} {@var{vals} =} spparms ()
@deftypefnx {} {[@var{keys}, @var{vals}] =} spparms ()
@deftypefnx {} {@var{val} =} spparms (@var{key})
@deftypefnx {} { } spparms (@var{vals})
@deftypefnx {} { } spparms ("default")
@deftypefnx {} { } spparms ("tight")
@deftypefnx {} { } spparms (@var{key}, @var{val})
Query or set the parameters used by the sparse solvers and factorization
functions.

The first four calls above get information about the current settings, while
the others change the current settings.  The parameters are stored as pairs
of keys and values, where the values are all floats and the keys are one of
the following strings:

@table @samp
@item spumoni
Printing level of debugging information of the solvers (default 0)

@item ths_rel
Included for compatibility.  Not used.  (default 1)

@item ths_abs
Included for compatibility.  Not used.  (default 1)

@item exact_d
Included for compatibility.  Not used.  (default 0)

@item supernd
Included for compatibility.  Not used.  (default 3)

@item rreduce
Included for compatibility.  Not used.  (default 3)

@item wh_frac
Included for compatibility.  Not used.  (default 0.5)

@item autommd
Flag whether the LU/QR and the '\' and '/' operators will automatically
use the sparsity preserving mmd functions (default 1)

@item autoamd
Flag whether the LU and the '\' and '/' operators will automatically
use the sparsity preserving amd functions (default 1)

@item piv_tol
The pivot tolerance of the @sc{umfpack} solvers (default 0.1)

@item sym_tol
The pivot tolerance of the @sc{umfpack} symmetric solvers (default 0.001)

@item bandden
The density of nonzero elements in a banded matrix before it is treated
by the @sc{lapack} banded solvers (default 0.5)

@item umfpack
Flag whether the @sc{umfpack} or mmd solvers are used for the LU, '\' and
'/' operations (default 1)
@end table

The value of individual keys can be set with
@code{spparms (@var{key}, @var{val})}.
The default values can be restored with the special keyword
@qcode{"default"}.  The special keyword @qcode{"tight"} can be used to
set the mmd solvers to attempt a sparser solution at the potential cost of
longer running time.
@seealso{chol, colamd, lu, qr, symamd}
@end deftypefn */)
{
  octave_value_list retval;
  int nargin = args.length ();

  if (nargin == 0)
    {
      if (nargout == 0)
        sparse_params::print_info (octave_stdout, "");
      else if (nargout == 1)
        retval = ovl (sparse_params::get_vals ());
      else if (nargout == 2)
        retval = ovl (sparse_params::get_keys (),
                      sparse_params::get_vals ());
      else
        error ("spparms: too many output arguments");
    }
  else if (nargin == 1)
    {
      if (args(0).is_string ())
        {
          std::string str = args(0).string_value ();
          int len = str.length ();
          for (int i = 0; i < len; i++)
            str[i] = tolower (str[i]);

          if (str == "default")
            sparse_params::defaults ();
          else if (str == "tight")
            sparse_params::tight ();
          else
            {
              double val = sparse_params::get_key (str);
              if (math::isnan (val))
                error ("spparms: KEY not recognized");

              retval = ovl (val);
            }
        }
      else
        {
          NDArray vals = args(0).xarray_value ("spparms: input must be a string or a vector");
          if (vals.numel () > OCTAVE_SPARSE_CONTROLS_SIZE)
            error ("spparms: too many elements in vector VALS");

          sparse_params::set_vals (vals);
        }
    }
  else if (nargin == 2)
    {
      std::string str = args(0).xstring_value ("spparms: first argument must be a string");

      double val = args(1).xdouble_value ("spparms: second argument must be a real scalar");

      if (str == "umfpack")
        warning ("spparms: request to disable umfpack solvers ignored");
      else if (! sparse_params::set_key (str, val))
        error ("spparms: KEY not found");
    }
  else
    error ("spparms: too many input arguments");

  return retval;
}

/*
%!test
%! old_vals = spparms ();  # save state
%! spparms ("default");
%! vals = spparms ();
%! assert (vals, [0 1 1 0 3 3 0.5 1.0 1.0 0.1 0.5 1.0 0.001]');
%! [keys, vals] = spparms ();
%! assert (rows (keys), 13);
%! assert (keys(2,:), "ths_rel");
%! assert (vals, [0 1 1 0 3 3 0.5 1.0 1.0 0.1 0.5 1.0 0.001]');
%! spparms ([3 2 1]);
%! assert (spparms ()(1:3), [3, 2, 1]');
%! assert (spparms ("ths_rel"), 2);
%! spparms ("exact_d", 5);
%! assert (spparms ("exact_d"), 5);
%! spparms (old_vals);     # restore state

## Test input validation
%!error <too many input arguments> spparms (1, 2, 3)
%!error <too many output arguments> [x, y, z] = spparms ()
%!error <KEY not recognized> spparms ("UNKNOWN_KEY")
%!#error <input must be a string> spparms ({1, 2, 3})
%!error spparms ({1, 2, 3})
%!error <too many elements in vector VALS> spparms (ones (14, 1))
%!error <first argument must be a string> spparms (1, 1)
%!#error <second argument must be a real scalar> spparms ("ths_rel", "hello")
%!error spparms ("ths_rel", "hello")
%!error <KEY not found> spparms ("UNKNOWN_KEY", 1)
*/

OCTAVE_END_NAMESPACE(octave)
