/*

Copyright (C) 2004 David Bateman
Copyright (C) 1998-2004 Andy Adler

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "defun-dld.h"
#include "ov.h"
#include "pager.h"
#include "error.h"
#include "gripes.h"

#include "oct-spparms.h"

DEFUN_DLD (spparms, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} { } spparms ()\n\
@deftypefnx {Loadable Function} {@var{vals} =} spparms ()\n\
@deftypefnx {Loadable Function} {[@var{keys}, @var{vals}] =} spparms ()\n\
@deftypefnx {Loadable Function} {@var{val} =} spparms (@var{key})\n\
@deftypefnx {Loadable Function} { } spparms (@var{vals})\n\
@deftypefnx {Loadable Function} { } spparms ('defaults')\n\
@deftypefnx {Loadable Function} { } spparms ('tight')\n\
@deftypefnx {Loadable Function} { } spparms (@var{key}, @var{val})\n\
Sets or displays the parameters used by the sparse solvers and factorization\n\
functions. The first four calls above get information about the current\n\
settings, while the others change the current settings. The parameters are\n\
stored as pairs of keys and values, where the values are all floats and the\n\
keys are one of the strings\n\
\n\
@itemize\n\
@item spumoni\n\
Printing level of debugging information of the solvers (default 0)\n\
@item ths_rel\n\
Included for compatiability. Bot used. (default 1)\n\
@item ths_abs\n\
Included for compatiability. Bot used. (default 1)\n\
@item exact_d\n\
Included for compatiability. Bot used. (default 0)\n\
@item supernd\n\
Included for compatiability. Not used. (default 3)\n\
@item rreduce\n\
Included for compatiability. Not used. (default 3)\n\
@item wh_frac\n\
Inluded for compatiability. Not used. (default 0.5)\n\
@item autommd\n\
Flag whether the LU/QR and the '\\' and '/' operators will automatically\n\
use the sparsity preserving mmd functions (default 1)\n\
@item autoamd\n\
Flag whether the LU and the '\\' and '/' operators will automatically\n\
use the sparsity preserving amd functions (default 1)\n\
@item piv_tol\n\
The pivot tolerance of the UMFPACK solvers (default 0.1)\n\
@item bandden\n\
?? (default 0.5)\n\
@item umfpack\n\
Flag whether the UMFPACK or mmd solvers are used for the LU, '\\' and\n\
'/' operations (default 1)\n\
@end itemize\n\
\n\
The value of individual keys can be set with @code{spparms (@var{key},\n\
@var{val})}. The default values can be restored with the special keyword\n\
'defaults'. The special keyword 'tight' can be used to set the mmd solvers\n\
to attempt for a sparser solution at the potetial cost of longer running\n\
time.\n\
@end deftypefn")
{
  octave_value_list retval;
  int nargin = args.length ();

  if (nargin == 0)
    {
      if (nargout == 0)
	octave_sparse_params::print_info (octave_stdout, "");
      else if (nargout == 1)
	retval(0) =  octave_sparse_params::get_vals ();
      else if (nargout == 2)
	{
	  retval (0) = octave_sparse_params::get_keys ();
	  retval (1) = octave_sparse_params::get_vals ();
	}
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
	    str [i] = tolower (str [i]);

	  if (str == "defaults")
	    octave_sparse_params::defaults ();
	  else if (str == "tight")
	    octave_sparse_params::tight ();
	  else
	    {
	      double val = octave_sparse_params::get_key (str);
	      if (xisnan (val))
		error ("spparams: unrecognized key");
	      else
		retval (0) = val;
	    }
	}
      else
	{
	  NDArray vals = args(0).array_value ();

	  if (error_state)
	    error ("spparms: input must be a string or a vector");
	  else if (vals.numel () > OCTAVE_SPARSE_CONTROLS_SIZE)
	    error ("spparams: too many elements in values vector");
	  else
	    octave_sparse_params::set_vals (vals);
	}
    }
  else if (nargin == 2)
    {
      if (args(0).is_string ())
	{
	  std::string str = args(0).string_value ();
	  
	  double val = args(1).double_value ();

	  if (error_state)
	    error ("spparms: second argument must be a real scalar");
          else if (str == "umfpack")
	    warning ("spparms: request to disable umfpack solvers ignored");
	  else if (!octave_sparse_params::set_key (str, val))
	    error ("spparms: key not found");
	}
      else
	error ("spparms: first argument must be a string");
    }
  else
    error ("spparms: too many input arguments");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
