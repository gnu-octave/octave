/*

Copyright (C) 2006-2011 David Bateman

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <algorithm>

#include "oct-fftw.h"

#include "defun-dld.h"
#include "error.h"
#include "ov.h"

DEFUN_DLD (fftw, args, ,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {@var{method} =} fftw ('planner')\n\
@deftypefnx {Loadable Function} {} fftw ('planner', @var{method})\n\
@deftypefnx {Loadable Function} {@var{wisdom} =} fftw ('dwisdom')\n\
@deftypefnx {Loadable Function} {} fftw ('dwisdom', @var{wisdom})\n\
\n\
Manage @sc{fftw} wisdom data.  Wisdom data can be used to significantly\n\
accelerate the calculation of the FFTs, but implies an initial cost\n\
in its calculation.  When the @sc{fftw} libraries are initialized, they read\n\
a system wide wisdom file (typically in @file{/etc/fftw/wisdom}), allowing\n\
wisdom to be shared between applications other than Octave.  Alternatively,\n\
the @code{fftw} function can be used to import wisdom.  For example,\n\
\n\
@example\n\
@var{wisdom} = fftw ('dwisdom')\n\
@end example\n\
\n\
@noindent\n\
will save the existing wisdom used by Octave to the string @var{wisdom}.\n\
This string can then be saved to a file and restored using the @code{save}\n\
and @code{load} commands respectively.  This existing wisdom can be\n\
reimported as follows\n\
\n\
@example\n\
fftw ('dwisdom', @var{wisdom})\n\
@end example \n\
\n\
If @var{wisdom} is an empty matrix, then the wisdom used is cleared.\n\
\n\
During the calculation of Fourier transforms further wisdom is generated.\n\
The fashion in which this wisdom is generated is also controlled by\n\
the @code{fftw} function.  There are five different manners in which the\n\
wisdom can be treated:\n\
\n\
@table @asis\n\
@item 'estimate'\n\
Specifies that no run-time measurement of the optimal means of\n\
calculating a particular is performed, and a simple heuristic is used\n\
to pick a (probably sub-optimal) plan.  The advantage of this method is\n\
that there is little or no overhead in the generation of the plan, which\n\
is appropriate for a Fourier transform that will be calculated once.\n\
\n\
@item 'measure'\n\
In this case a range of algorithms to perform the transform is considered\n\
and the best is selected based on their execution time.\n\
\n\
@item 'patient'\n\
Similar to 'measure', but a wider range of algorithms is considered.\n\
\n\
@item 'exhaustive'\n\
Like 'measure', but all possible algorithms that may be used to\n\
treat the transform are considered.\n\
\n\
@item 'hybrid'\n\
As run-time measurement of the algorithm can be expensive, this is a\n\
compromise where 'measure' is used for transforms up to the size of 8192\n\
and beyond that the 'estimate' method is used.\n\
@end table\n\
\n\
The default method is 'estimate'.  The current method can\n\
be queried with\n\
\n\
@example\n\
@var{method} = fftw ('planner')\n\
@end example\n\
\n\
@noindent\n\
or set by using\n\
\n\
@example\n\
fftw ('planner', @var{method})\n\
@end example\n\
\n\
Note that calculated wisdom will be lost when restarting Octave.  However,\n\
the wisdom data can be reloaded if it is saved to a file as described\n\
above.  Saved wisdom files should not be used on different platforms since\n\
they will not be efficient and the point of calculating the wisdom is lost.\n\
@seealso{fft, ifft, fft2, ifft2, fftn, ifftn}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length();

  if (nargin < 1 || nargin > 2)
    {
      print_usage ();
      return retval;
    }

#if defined (HAVE_FFTW)
  if (args(0).is_string ())
    {
      std::string arg0 = args(0).string_value ();

      if (!error_state)
        {
          // Use STL function to convert to lower case
          std::transform (arg0.begin (), arg0.end (), arg0.begin (), tolower);
          
          if (nargin == 2)
            {
              std::string arg1 = args(1).string_value ();
              if (!error_state)
                {
                  if (arg0 == "planner")
                    {
                      std::transform (arg1.begin (), arg1.end (), 
                                      arg1.begin (), tolower);
                      octave_fftw_planner::FftwMethod meth
                        = octave_fftw_planner::UNKNOWN;
                      octave_float_fftw_planner::FftwMethod methf
                        = octave_float_fftw_planner::UNKNOWN;

                      if (arg1 == "estimate")
                        {
                          meth = octave_fftw_planner::ESTIMATE;
                          methf = octave_float_fftw_planner::ESTIMATE;
                        }
                      else if (arg1 == "measure")
                        {
                          meth = octave_fftw_planner::MEASURE;
                          methf = octave_float_fftw_planner::MEASURE;
                        }
                      else if (arg1 == "patient")
                        {
                          meth = octave_fftw_planner::PATIENT;
                          methf = octave_float_fftw_planner::PATIENT;
                        }
                      else if (arg1 == "exhaustive")
                        {
                          meth = octave_fftw_planner::EXHAUSTIVE;
                          methf = octave_float_fftw_planner::EXHAUSTIVE;
                        }
                      else if (arg1 == "hybrid")
                        {
                          meth = octave_fftw_planner::HYBRID;
                          methf = octave_float_fftw_planner::HYBRID;
                        }
                      else
                        error ("unrecognized planner METHOD");

                      if (!error_state)
                        {
                          meth = octave_fftw_planner::method (meth);
                          octave_float_fftw_planner::method (methf);

                          if (meth == octave_fftw_planner::MEASURE)
                            retval = octave_value ("measure");
                          else if (meth == octave_fftw_planner::PATIENT)
                            retval = octave_value ("patient");
                          else if (meth == octave_fftw_planner::EXHAUSTIVE)
                            retval = octave_value ("exhaustive");
                          else if (meth == octave_fftw_planner::HYBRID)
                            retval = octave_value ("hybrid");
                          else
                            retval = octave_value ("estimate");
                        }
                    }
                  else if (arg0 == "dwisdom")
                    {
                      char *str = fftw_export_wisdom_to_string ();

                      if (arg1.length() < 1)
                        fftw_forget_wisdom ();
                      else if (! fftw_import_wisdom_from_string (arg1.c_str()))
                        error ("could not import supplied WISDOM");

                      if (!error_state)
                        retval = octave_value (std::string (str));

                      free (str);
                    }
                  else if (arg0 == "swisdom")
                    {
                      char *str = fftwf_export_wisdom_to_string ();

                      if (arg1.length() < 1)
                        fftwf_forget_wisdom ();
                      else if (! fftwf_import_wisdom_from_string (arg1.c_str()))
                        error ("could not import supplied WISDOM");

                      if (!error_state)
                        retval = octave_value (std::string (str));

                      free (str);
                    }
                  else
                    error ("unrecognized argument");
                }
            }
          else
            {
              if (arg0 == "planner")
                {
                  octave_fftw_planner::FftwMethod meth = 
                    octave_fftw_planner::method ();

                  if (meth == octave_fftw_planner::MEASURE)
                    retval = octave_value ("measure");
                  else if (meth == octave_fftw_planner::PATIENT)
                    retval = octave_value ("patient");
                  else if (meth == octave_fftw_planner::EXHAUSTIVE)
                    retval = octave_value ("exhaustive");
                  else if (meth == octave_fftw_planner::HYBRID)
                    retval = octave_value ("hybrid");
                  else
                    retval = octave_value ("estimate");
                }
              else if (arg0 == "dwisdom")
                {
                  char *str = fftw_export_wisdom_to_string ();
                  retval = octave_value (std::string (str));
                  free (str);
                }
              else if (arg0 == "swisdom")
                {
                  char *str = fftwf_export_wisdom_to_string ();
                  retval = octave_value (std::string (str));
                  free (str);
                }
              else
                error ("unrecognized argument");
            }
        }
    }
#else

  warning ("fftw: this copy of Octave was not configured to use the FFTW3 planner");

#endif

  return retval;
}
