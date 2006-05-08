/*

Copyright (C) 2004 David Bateman

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if defined (HAVE_FFTW3)
#include <fftw3.h>
#endif

#include <sstream>

#include "defaults.h"
#include "defun-dld.h"
#include "error.h"
#include "file-ops.h"
#include "gripes.h"
#include "lo-mappers.h"
#include "oct-env.h"
#include "oct-obj.h"
#include "sighandlers.h"
#include "utils.h"

DEFUN_DLD (fftw_wisdom, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} fftw_wisdom (@var{file}, @var{ow})\n\
@deftypefnx {Loadable Function} {} fftw_wisdom (@var{n})\n\
\n\
This function is used to manipulate the FFTW wisdom data. It can\n\
be used to load previously stored wisdom from a file, create wisdom\n\
needed by Octave and to save the current wisdom to a file. Wisdom\n\
data can be used to significantly accelerate the calculation of the FFTs,\n\
but is only profitable if the same FFT is called many times due to the\n\
overhead in calculating the wisdom data.\n\
\n\
Called with a single string argument, @code{fftw_wisdom (@var{file})}\n\
will load the wisdom data found in @var{file}. If @var{file} does\n\
not exist, then the current wisdom used by FFTW is saved to this\n\
file. If the flag @var{ow} is non-zero, then even if @var{file}\n\
exists, it will be overwritten.\n\
\n\
It is assumed that each row of @var{n} represents the size of a FFT for\n\
which it is desired to pre-calculate the wisdom needed to accelerate it.\n\
Any value of the matrix that is less than 1, is assumed to indicate an\n\
absent dimension. That is\n\
\n\
@example\n\
fftw_wisdom ([102, 0, 0; 103, 103, 0; 102, 103, 105]);\n\
a = fft (rand (1,102));\n\
b = fft (rand (103,103));\n\
c = fftn (rand ([102, 103, 105]));\n\
@end example\n\
\n\
calculates the wisdom necessary to accelerate the 103, 102x102 and\n\
the 102x103x105 FFTs. Note that calculated wisdom will be lost when\n\
restarting Octave. However, if it is saved as discussed above, it\n\
can be reloaded. Also, any system-wide wisdom file that has been found\n\
will also be used. Saved wisdom files should not be used on different\n\
platforms since they will not be efficient and the point of calculating\n\
the wisdom is lost.\n\
\n\
Note that the program @code{fftw-wisdom} supplied with FFTW can equally\n\
be used to create a file containing wisdom that can be imported into\n\
Octave.\n\
@seealso{fft, ifft, fft2, ifft2, fftn, ifftn}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length();

  if (nargin < 1 || nargin > 2)
    {
      print_usage ("fftw_wisdom");
      return retval;
    }

#if defined (HAVE_FFTW3)

  if (args(0).is_string ())
    {
      bool overwrite = false;

      if (nargin != 1)
	{
	  double dval = args (1).double_value ();
	  if (NINTbig (dval) != 0)
	    overwrite = true;
	}

      std::string str = args(0).string_value ();
      std::string wisdom = octave_env::make_absolute
	(Vload_path_dir_path.find_first_of (str), octave_env::getcwd ());

      // FIXME -- should probably protect FILE* resources with
      // auto_ptr or similar...

      if (wisdom.empty () || overwrite)
	{
	  if (str.empty ())
	    error ("fftw_wisdom: can not save to file");
	  else
	    {
	      FILE *ofile = fopen (str.c_str (), "wb");
	      if (! ofile)
		error ("fftw_wisdom: can not save to file %s", str.c_str());
	      else
		{
		  fftw_export_wisdom_to_file (ofile);
		  fclose (ofile);
		}
	    }
	}
      else
	{
	  FILE *ifile = fopen (wisdom.c_str (), "r");
	  if (! fftw_import_wisdom_from_file (ifile))
	    error ("fftw_wisdom: can not import wisdom from file"); 
	  fclose (ifile);
	}

    } 
  else 
    {
      Matrix m = args (0).matrix_value ();

      if (error_state)
	{
	  error ("fftw_wisdom: argument must be a matrix or a string");
	  return retval;
	}

      std::string name = file_ops::tempnam ("", "oct-");

      if (name.empty ())
	{
	  error ("fftw_wisdom: can not open temporary file");
	  return retval;
	}

      std::ostringstream cmd_buf; 
      cmd_buf << Vfftw_wisdom_program << " -n -o \"" << name << "\"";

      for (octave_idx_type k = 0; k < m.rows (); k++)
	{
	  bool first = true;

	  cmd_buf << " ";

	  // Note reversal of dimensions for column major storage in FFTW
	  for (octave_idx_type j = m.columns()-1; j >= 0; j--)
	    if (NINTbig(m(k,j)) > 0)
	      {
		if (first)
		  first = false;
		else
		  cmd_buf << "x";
		cmd_buf << NINTbig(m(k,j)) ;
	      }
	} 

      volatile octave_interrupt_handler old_interrupt_handler
	= octave_ignore_interrupts ();

      std::string cmd_buf_str = cmd_buf.str ();

      int status = system (cmd_buf_str.c_str ());

      octave_set_interrupt_handler (old_interrupt_handler);

      if (WIFEXITED (status))
	{
	  FILE *ifile = fopen (name.c_str (), "r");
	  if (! fftw_import_wisdom_from_file (ifile))
	    error ("fftw_wisdom: can not import wisdom from temporary file"); 
	  fclose (ifile);
	}
      else
	error ("fftw_wisdom: error running %s", Vfftw_wisdom_program.c_str ());
    }

#else

  warning ("fftw_wisdom: this copy of Octave was not configured to use FFTW3");

#endif

  return retval;
}
