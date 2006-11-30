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

#include "file-stat.h"

#include "defaults.h"
#include "defun-dld.h"
#include "error.h"
#include "file-ops.h"
#include "gripes.h"
#include "lo-mappers.h"
#include "load-path.h"
#include "oct-env.h"
#include "oct-obj.h"
#include "sighandlers.h"
#include "utils.h"

DEFUN_DLD (fftw_wisdom, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} fftw_wisdom (@var{file}, @var{mode})\n\
Save or load FFTW wisdom data to @var{file}.  The optional argument\n\
@var{mode} may be either @samp{\"r\"} or @samp{\"w\"}.  The default\n\
value is @samp{\"r\"}.\n\
\n\
@deftypefnx {Loadable Function} {} fftw_wisdom (@var{n})\n\
Pre-calculate FFTW wisdom data for an FFT of size @var{n}.\n\
Each row of @var{n} represents the size of an FFT for\n\
which it is desired to pre-calculate the wisdom needed to accelerate it.\n\
Any value of the matrix that is less than 1, is assumed to indicate an\n\
absent dimension.  For example,\n\
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
restarting Octave. However, the wisdom data can be reloaded if it is\n\
saved to a file as described above.  Also, any system-wide wisdom\n\
file that has been found will also be used. Saved wisdom files\n\
should not be used on different platforms since they will not be\n\
efficient and the point of calculating the wisdom is lost.\n\
\n\
Wisdom data can be used to significantly accelerate the calculation\n\
of the FFTs but is only profitable if the same FFT is called many\n\
times due to the overhead in calculating the wisdom data.\n\
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
      print_usage ();
      return retval;
    }

#if defined (HAVE_FFTW3)

  if (args(0).is_string ())
    {
      bool write_wisdom = false;

      if (nargin == 2)
	{
	  std::string mode = args(1).string_value ();

	  if (! error_state && (mode == "r" || mode == "w"))
	    write_wisdom = mode == "w";
	  else
	    {
	      error ("fftw_wisdom: expecting second argument to be \"r\" or \"w\"");
	      return retval;
	    }
	}

      std::string name = args(0).string_value ();

      std::string wisdom = file_ops::tilde_expand (name);

      if (! (write_wisdom || octave_env::absolute_pathname (wisdom)))
	{
	  file_stat fs (wisdom);

	  if (! fs.exists ())
	    {
	      std::string tmp = octave_env::make_absolute
		(load_path::find_file (wisdom), octave_env::getcwd ());

	      if (! tmp.empty ())
		{
		  warning_with_id ("Octave:fftw-wisdom-file-in-path",
				   "fftw_wisdom: file found in load path");
		  wisdom = tmp;
		}
	    }
	}

      if (write_wisdom)
	{
	  FILE *ofile = fopen (wisdom.c_str (), "wb");

	  if (! ofile)
	    error ("fftw_wisdom: unable to open file `%s' for writing",
		   wisdom.c_str());
	  else
	    {
	      fftw_export_wisdom_to_file (ofile);
	      fclose (ofile);
	    }
	}
      else
	{
	  FILE *ifile = fopen (wisdom.c_str (), "r");

	  if (! ifile)
	    error ("fftw_wisdom: unable to open file `%s' for reading",
		   wisdom.c_str ());
	  else
	    {
	      if (! fftw_import_wisdom_from_file (ifile))
		error ("fftw_wisdom: can not import wisdom from file"); 

	      fclose (ifile);
	    }
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
