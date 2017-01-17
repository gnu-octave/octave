/*

Copyright (C) 2006-2016 David Bateman

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <algorithm>
#include <string>

#if defined (HAVE_FFTW3_H)
#  include <fftw3.h>
#endif

#include "oct-fftw.h"

#include "defun-dld.h"
#include "error.h"
#include "errwarn.h"
#include "ov.h"

DEFUN_DLD (fftw, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{method} =} fftw ("planner")
@deftypefnx {} {} fftw ("planner", @var{method})
@deftypefnx {} {@var{wisdom} =} fftw ("dwisdom")
@deftypefnx {} {} fftw ("dwisdom", @var{wisdom})
@deftypefnx {} {} fftw ("threads", @var{nthreads})
@deftypefnx {} {@var{nthreads} =} fftw ("threads")

Manage @sc{fftw} wisdom data.

Wisdom data can be used to significantly accelerate the calculation of the
FFTs, but implies an initial cost in its calculation.  When the @sc{fftw}
libraries are initialized, they read a system wide wisdom file (typically in
@file{/etc/fftw/wisdom}), allowing wisdom to be shared between applications
other than Octave.  Alternatively, the @code{fftw} function can be used to
import wisdom.  For example,

@example
@var{wisdom} = fftw ("dwisdom")
@end example

@noindent
will save the existing wisdom used by Octave to the string @var{wisdom}.
This string can then be saved to a file and restored using the @code{save}
and @code{load} commands respectively.  This existing wisdom can be
re-imported as follows

@example
fftw ("dwisdom", @var{wisdom})
@end example

If @var{wisdom} is an empty string, then the wisdom used is cleared.

During the calculation of Fourier transforms further wisdom is generated.
The fashion in which this wisdom is generated is also controlled by
the @code{fftw} function.  There are five different manners in which the
wisdom can be treated:

@table @asis
@item @qcode{"estimate"}
Specifies that no run-time measurement of the optimal means of
calculating a particular is performed, and a simple heuristic is used
to pick a (probably sub-optimal) plan.  The advantage of this method is
that there is little or no overhead in the generation of the plan, which
is appropriate for a Fourier transform that will be calculated once.

@item @qcode{"measure"}
In this case a range of algorithms to perform the transform is considered
and the best is selected based on their execution time.

@item @qcode{"patient"}
Similar to @qcode{"measure"}, but a wider range of algorithms is
considered.

@item @qcode{"exhaustive"}
Like @qcode{"measure"}, but all possible algorithms that may be used to
treat the transform are considered.

@item @qcode{"hybrid"}
As run-time measurement of the algorithm can be expensive, this is a
compromise where @qcode{"measure"} is used for transforms up to the size
of 8192 and beyond that the @qcode{"estimate"} method is used.
@end table

The default method is @qcode{"estimate"}.  The current method can
be queried with

@example
@var{method} = fftw ("planner")
@end example

@noindent
or set by using

@example
fftw ("planner", @var{method})
@end example

Note that calculated wisdom will be lost when restarting Octave.  However,
the wisdom data can be reloaded if it is saved to a file as described
above.  Saved wisdom files should not be used on different platforms since
they will not be efficient and the point of calculating the wisdom is lost.

The number of threads used for computing the plans and executing the
transforms can be set with

@example
fftw ("threads", @var{NTHREADS})
@end example

Note that octave must be compiled with multi-threaded @sc{fftw} support for
this feature.  The number of processors available to the current process is
used per default.

@seealso{fft, ifft, fft2, ifft2, fftn, ifftn}
@end deftypefn */)
{
#if defined (HAVE_FFTW)

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  octave_value retval;

  std::string arg0 = args(0).xstring_value ("fftw: first argument must be a string");

  if (arg0 == "planner")
    {
      if (nargin == 2)  // planner setter
        {
          // Use STL function to convert to lower case
          std::transform (arg0.begin (), arg0.end (), arg0.begin (), tolower);

          std::string arg1 = args(1).xstring_value ("fftw: METHOD must be a string");

          std::transform (arg1.begin (), arg1.end (), arg1.begin (), tolower);
          octave::fftw_planner::FftwMethod meth
            = octave::fftw_planner::UNKNOWN;
          octave::float_fftw_planner::FftwMethod methf
            = octave::float_fftw_planner::UNKNOWN;

          if (arg1 == "estimate")
            {
              meth = octave::fftw_planner::ESTIMATE;
              methf = octave::float_fftw_planner::ESTIMATE;
            }
          else if (arg1 == "measure")
            {
              meth = octave::fftw_planner::MEASURE;
              methf = octave::float_fftw_planner::MEASURE;
            }
          else if (arg1 == "patient")
            {
              meth = octave::fftw_planner::PATIENT;
              methf = octave::float_fftw_planner::PATIENT;
            }
          else if (arg1 == "exhaustive")
            {
              meth = octave::fftw_planner::EXHAUSTIVE;
              methf = octave::float_fftw_planner::EXHAUSTIVE;
            }
          else if (arg1 == "hybrid")
            {
              meth = octave::fftw_planner::HYBRID;
              methf = octave::float_fftw_planner::HYBRID;
            }
          else
            error ("fftw: unrecognized planner METHOD");

          meth = octave::fftw_planner::method (meth);
          octave::float_fftw_planner::method (methf);

          if (meth == octave::fftw_planner::MEASURE)
            retval = octave_value ("measure");
          else if (meth == octave::fftw_planner::PATIENT)
            retval = octave_value ("patient");
          else if (meth == octave::fftw_planner::EXHAUSTIVE)
            retval = octave_value ("exhaustive");
          else if (meth == octave::fftw_planner::HYBRID)
            retval = octave_value ("hybrid");
          else
            retval = octave_value ("estimate");
        }
      else //planner getter
        {
          octave::fftw_planner::FftwMethod meth =
            octave::fftw_planner::method ();

          if (meth == octave::fftw_planner::MEASURE)
            retval = octave_value ("measure");
          else if (meth == octave::fftw_planner::PATIENT)
            retval = octave_value ("patient");
          else if (meth == octave::fftw_planner::EXHAUSTIVE)
            retval = octave_value ("exhaustive");
          else if (meth == octave::fftw_planner::HYBRID)
            retval = octave_value ("hybrid");
          else
            retval = octave_value ("estimate");
        }
    }
  else if (arg0 == "dwisdom")
    {
      if (nargin == 2)  //dwisdom setter
        {
          // Use STL function to convert to lower case
          std::transform (arg0.begin (), arg0.end (), arg0.begin (),
                          tolower);

          std::string arg1 = args(1).xstring_value ("fftw: WISDOM must be a string");

          char *str = fftw_export_wisdom_to_string ();
          std::string wisdom_str (str);
          free (str);

          if (arg1.length () < 1)
            fftw_forget_wisdom ();
          else if (! fftw_import_wisdom_from_string (arg1.c_str ()))
            error ("fftw: could not import supplied WISDOM");

          retval = octave_value (wisdom_str);
        }
      else //dwisdom getter
        {
          char *str = fftw_export_wisdom_to_string ();
          std::string wisdom_str (str);
          free (str);
          retval = octave_value (wisdom_str);
        }
    }
  else if (arg0 == "swisdom")
    {
      //swisdom uses fftwf_ functions (float), dwisdom fftw_ (real)
      if (nargin == 2)  //swisdom setter
        {
          // Use STL function to convert to lower case
          std::transform (arg0.begin (), arg0.end (), arg0.begin (),
                          tolower);

          std::string arg1 = args(1).xstring_value ("fftw: WISDOM must be a string");

          char *str = fftwf_export_wisdom_to_string ();
          std::string wisdom_str (str);
          free (str);

          if (arg1.length () < 1)
            fftwf_forget_wisdom ();
          else if (! fftwf_import_wisdom_from_string (arg1.c_str ()))
            error ("fftw: could not import supplied WISDOM");

          retval = octave_value (wisdom_str);
        }
      else //swisdom getter
        {
          char *str = fftwf_export_wisdom_to_string ();
          std::string wisdom_str (str);
          free (str);
          retval = octave_value (wisdom_str);
        }
    }
  else if (arg0 == "threads")
    {
      if (nargin == 2)  //threads setter
        {
          if (! args(1).is_real_scalar ())
            error ("fftw: setting threads needs one integer argument");

          int nthreads = args(1).int_value();
          if (nthreads < 1)
            error ("fftw: number of threads must be >=1");

#if defined (HAVE_FFTW3_THREADS)
          octave::fftw_planner::threads (nthreads);
#else
          err_disabled_feature ("fftw", "multithreaded FFTW");
#endif
#if defined (HAVE_FFTW3F_THREADS)
          octave::float_fftw_planner::threads (nthreads);
#else
          err_disabled_feature ("fftw", "multithreaded FFTW");
#endif
        }
      else //threads getter
#if defined (HAVE_FFTW3_THREADS)
        retval = octave_value (octave::fftw_planner::threads());
#else
        retval = 1;
#endif
    }
  else
    error ("fftw: unrecognized argument");

  return retval;

#else

  octave_unused_parameter (args);

  err_disabled_feature ("fftw", "the FFTW3 planner");

#endif
}

/*
%!testif HAVE_FFTW
%! def_method = fftw ("planner");
%! unwind_protect
%!   method = "estimate";
%!   fftw ("planner", method);
%!   assert (fftw ("planner"), method);
%!   method = "measure";
%!   fftw ("planner", method);
%!   assert (fftw ("planner"), method);
%!   method = "patient";
%!   fftw ("planner", method);
%!   assert (fftw ("planner"), method);
%!   method = "exhaustive";
%!   fftw ("planner", method);
%!   assert (fftw ("planner"), method);
%!   method = "hybrid";
%!   fftw ("planner", method);
%!   assert (fftw ("planner"), method);
%! unwind_protect_cleanup
%!   fftw ("planner", def_method);
%! end_unwind_protect

%!testif HAVE_FFTW
%! def_dwisdom = fftw ("dwisdom");
%! def_swisdom = fftw ("swisdom");
%! unwind_protect
%!   wisdom = fftw ("dwisdom");
%!   assert (ischar (wisdom));
%!   fftw ("dwisdom", wisdom);
%!   assert (fftw ("dwisdom"), wisdom);
%!   wisdom = fftw ("swisdom");
%!   assert (ischar (wisdom));
%!   fftw ("swisdom", wisdom);
%!   assert (fftw ("swisdom"), wisdom);
%! unwind_protect_cleanup
%!   fftw ("dwisdom", def_dwisdom);
%!   fftw ("swisdom", def_swisdom);
%! end_unwind_protect

%!testif HAVE_FFTW3_THREADS
%! n = fftw ("threads");
%! unwind_protect
%!   fftw ("threads", 3);
%!   assert (fftw ("threads"), 3);
%! unwind_protect_cleanup
%!   fftw ("threads", n);
%! end_unwind_protect

%!error <Invalid call to fftw|was unavailable or disabled> fftw ()
%!error <Invalid call to fftw|was unavailable or disabled> fftw ("planner", "estimate", "measure")
%!error fftw (3)
%!error fftw ("invalid")
%!error fftw ("planner", "invalid")
%!error fftw ("planner", 2)
%!error fftw ("dwisdom", "invalid")
%!error fftw ("swisdom", "invalid")
%!error fftw ("threads", "invalid")
%!error fftw ("threads", -3)
 */

