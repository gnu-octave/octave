/*

Copyright (C) 1996, 1997 John W. Eaton

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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

// Originally written by John C. Campbell <jcc@bevo.che.wisc.edu>
//
// Thomas Baier <baier@ci.tuwien.ac.at> added the original versions of
// the following functions:
//
//   popen
//   pclose
//   execute       (now popen2.m)
//   sync_system   (now merged with system)
//   async_system  (now merged with system)

// Completely rewritten by John W. Eaton <jwe@bevo.che.wisc.edu>,
// April 1996.

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <climits>

#include <iostream>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "file-ops.h"

#include "defun.h"
#include "error.h"
#include "lo-ieee.h"
#include "oct-fstrm.h"
#include "oct-iostrm.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "oct-prcstrm.h"
#include "oct-stream.h"
#include "oct-strstrm.h"
#include "pager.h"
#include "sysdep.h"
#include "utils.h"
#include "variables.h"

static octave_value stdin_file;
static octave_value stdout_file;
static octave_value stderr_file;

void
initialize_file_io (void)
{
  octave_stream stdin_stream = octave_istream::create (&std::cin, "stdin");

  // This uses octave_stdout (see pager.h), not std::cout so that Octave's
  // standard output stream will pass through the pager.

  octave_stream stdout_stream
    = octave_ostream::create (&octave_stdout, "stdout");

  octave_stream stderr_stream = octave_ostream::create (&std::cerr, "stderr");

  stdin_file = octave_stream_list::insert (stdin_stream);
  stdout_file = octave_stream_list::insert (stdout_stream);
  stderr_file = octave_stream_list::insert (stderr_stream);
}

void
close_files (void)
{
  octave_stream_list::clear ();
}

static std::ios::openmode
fopen_mode_to_ios_mode (const std::string& mode)
{
  std::ios::openmode retval = std::ios::in;

  if (! mode.empty ())
    {
      // Could probably be faster, but does it really matter?

      if (mode == "r")
	retval = std::ios::in;
      else if (mode == "w")
	retval = std::ios::out | std::ios::trunc;
      else if (mode == "a")
	retval = std::ios::out | std::ios::app;
      else if (mode == "r+")
	retval = std::ios::in | std::ios::out;
      else if (mode == "w+")
	retval = std::ios::in | std::ios::out | std::ios::trunc;
      else if (mode == "a+")
	retval = std::ios::in | std::ios::out | std::ios::ate;
      else if (mode == "rb")
	retval = std::ios::in | std::ios::binary;
      else if (mode == "wb")
	retval = std::ios::out | std::ios::trunc | std::ios::binary;
      else if (mode == "ab")
	retval = std::ios::out | std::ios::app | std::ios::binary;
      else if (mode == "r+b")
	retval = std::ios::in | std::ios::out | std::ios::binary;
      else if (mode == "w+b")
	retval = (std::ios::in | std::ios::out | std::ios::trunc
		  | std::ios::binary);
      else if (mode == "a+b")
	retval = (std::ios::in | std::ios::out | std::ios::ate
		  | std::ios::binary);
      else
	::error ("invalid mode specified");
    }

  return retval;
}

DEFUN (isstream, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isstream (@var{x})\n\
Return true if @var{x} is a stream object.  Otherwise, return false.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_stream ();
  else
    print_usage ("isstream");

  return retval;
}

DEFUN (fclose, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} fclose (@var{fid})\n\
Closes the specified file.  If an error is encountered while trying to\n\
close the file, an error message is printed and @code{fclose} returns\n\
0.  Otherwise, it returns 1.\n\
@end deftypefn")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 1)
    retval = static_cast<double> (octave_stream_list::remove (args(0),
				  "fclose"));
  else
    print_usage ("fclose");

  return retval;
}

DEFUN (fflush, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} fflush (@var{fid})\n\
Flush output to @var{fid}.  This is useful for ensuring that all\n\
pending output makes it to the screen before some other event occurs.\n\
For example, it is always a good idea to flush the standard output\n\
stream before calling @code{input}.\n\
@end deftypefn")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 1)
    {
      // XXX FIXME XXX -- any way to avoid special case for stdout?

      int fid = octave_stream_list::get_file_number (args (0));

      if (fid == 1)
	{
	  flush_octave_stdout ();

	  retval = 0.0;
	}
      else
	{
	  octave_stream os = octave_stream_list::lookup (fid, "fflush");

	  if (! error_state)
	    retval = static_cast<double> (os.flush ());
	}
    }
  else
    print_usage ("fflush");

  return retval;
}

DEFUN (fgetl, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} fgetl (@var{fid}, @var{len})\n\
Read characters from a file, stopping after a newline, or EOF,\n\
or @var{len} characters have been read.  The characters read, excluding\n\
the possible trailing newline, are returned as a string.\n\
\n\
If @var{len} is omitted, @code{fgetl} reads until the next newline\n\
character.\n\
\n\
If there are no more characters to read, @code{fgetl} returns @minus{}1.\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = 0.0;
  retval(0) = -1.0;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      octave_stream os = octave_stream_list::lookup (args(0), "fgetl");

      if (! error_state)
	{
	  octave_value len_arg = (nargin == 2)
	    ? args(1) : octave_value (static_cast<double> (INT_MAX));

	  bool err = false;

	  std::string tmp = os.getl (len_arg, err);

	  if (! err)
	    {
	      retval(1) = static_cast<double> (tmp.length ());
	      retval(0) = tmp;
	    }
	}
    }
  else
    print_usage ("fgetl");

  return retval;
}

DEFUN (fgets, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} fgets (@var{fid}, @var{len})\n\
Read characters from a file, stopping after a newline, or EOF,\n\
or @var{len} characters have been read.  The characters read, including\n\
the possible trailing newline, are returned as a string.\n\
\n\
If @var{len} is omitted, @code{fgets} reads until the next newline\n\
character.\n\
\n\
If there are no more characters to read, @code{fgets} returns @minus{}1.\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = 0.0;
  retval(0) = -1.0;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      octave_stream os = octave_stream_list::lookup (args(0), "fgets");

      if (! error_state)
	{
	  octave_value len_arg = (nargin == 2)
	    ? args(1) : octave_value (static_cast<double> (INT_MAX));

	  bool err = false;

	  std::string tmp = os.gets (len_arg, err);

	  if (! err)
	    {
	      retval(1) = static_cast<double> (tmp.length ());
	      retval(0) = tmp;
	    }
	}
    }
  else
    print_usage ("fgets");

  return retval;
}

static octave_stream
do_stream_open (const std::string& name, const std::string& mode,
		const std::string& arch, int& fid)
{
  octave_stream retval;

  fid = -1;

  std::ios::openmode md = fopen_mode_to_ios_mode (mode);

  if (! error_state)
    {
      oct_mach_info::float_format flt_fmt =
	oct_mach_info::string_to_float_format (arch);

      if (! error_state)
	retval = octave_fstream::create (name, md, flt_fmt);
    }

  return retval;
}

static octave_stream
do_stream_open (const octave_value& tc_name, const octave_value& tc_mode,
		const octave_value& tc_arch, const char *fcn, int& fid)
{
  octave_stream retval;

  fid = -1;

  std::string name = tc_name.string_value ();

  if (! error_state)
    {
      std::string mode = tc_mode.string_value ();

      if (! error_state)
	{
	  std::string arch = tc_arch.string_value ();

	  if (! error_state)
	    retval = do_stream_open (name, mode, arch, fid);
	  else
	    ::error ("%s: architecture type must be a string", fcn);
	}
      else
	::error ("%s: file mode must be a string", fcn);
    }
  else
    ::error ("%s: file name must be a string", fcn);

  return retval;
}

DEFUN (fopen, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{fid}, @var{msg}] =} fopen (@var{name}, @var{mode}, @var{arch})\n\
@deftypefnx {Built-in Function} {@var{fid_list} =} fopen (\"all\")\n\
@deftypefnx {Built-in Function} {@var{file} =} fopen (@var{fid})\n\
The first form of the @code{fopen} function opens the named file with\n\
the specified mode (read-write, read-only, etc.) and architecture\n\
interpretation (IEEE big endian, IEEE little endian, etc.), and returns\n\
an integer value that may be used to refer to the file later.  If an\n\
error occurs, @var{fid} is set to @minus{}1 and @var{msg} contains the\n\
corresponding system error message.  The @var{mode} is a one or two\n\
character string that specifies whether the file is to be opened for\n\
reading, writing, or both.\n\
\n\
The second form of the @code{fopen} function returns a vector of file ids\n\
corresponding to all the currently open files, excluding the\n\
@code{stdin}, @code{stdout}, and @code{stderr} streams.\n\
\n\
The third form of the @code{fopen} function returns the name of a\n\
currently open file given its file id.\n\
\n\
For example,\n\
\n\
@example\n\
myfile = fopen (\"splat.dat\", \"r\", \"ieee-le\");\n\
@end example\n\
\n\
@noindent\n\
opens the file @file{splat.dat} for reading.  If necessary, binary\n\
numeric values will be read assuming they are stored in IEEE format with\n\
the least significant bit first, and then converted to the native\n\
representation.\n\
\n\
Opening a file that is already open simply opens it again and returns a\n\
separate file id.  It is not an error to open a file several times,\n\
though writing to the same file through several different file ids may\n\
produce unexpected results.\n\
\n\
The possible values @samp{mode} may have are\n\
\n\
@table @asis\n\
@item @samp{r}\n\
Open a file for reading.\n\
\n\
@item @samp{w}\n\
Open a file for writing.  The previous contents are discared.\n\
\n\
@item @samp{a}\n\
Open or create a file for writing at the end of the file.\n\
\n\
@item @samp{r+}\n\
Open an existing file for reading and writing.\n\
\n\
@item @samp{w+}\n\
Open a file for reading or writing.  The previous contents are\n\
discarded.\n\
\n\
@item @samp{a+}\n\
Open or create a file for reading or writing at the end of the\n\
file.\n\
@end table\n\
\n\
The parameter @var{arch} is a string specifying the default data format\n\
for the file.  Valid values for @var{arch} are:\n\
\n\
@table @asis\n\
@samp{native}\n\
The format of the current machine (this is the default).\n\
\n\
@samp{ieee-be}\n\
IEEE big endian format.\n\
\n\
@samp{ieee-le}\n\
IEEE little endian format.\n\
\n\
@samp{vaxd}\n\
VAX D floating format.\n\
\n\
@samp{vaxg}\n\
VAX G floating format.\n\
\n\
@samp{cray}\n\
Cray floating format.\n\
@end table\n\
\n\
@noindent\n\
however, conversions are currently only supported for @samp{native}\n\
@samp{ieee-be}, and @samp{ieee-le} formats.\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(0) = -1.0;

  int nargin = args.length ();

  if (nargin == 1)
    {
      if (args(0).is_string ())
	{
	  // If there is only one argument and it is a string but it
	  // is not the string "all", we assume it is a file to open
	  // with MODE = "r".  To open a file called "all", you have
	  // to supply more than one argument.

	  if (args(0).string_value () == "all")
	    return octave_stream_list::open_file_numbers ();
	}
      else
	{
	  string_vector tmp = octave_stream_list::get_info (args(0));

	  if (! error_state)
	    {
	      retval(2) = tmp(2);
	      retval(1) = tmp(1);
	      retval(0) = tmp(0);
	    }

	  return retval;
	}
    }

  if (nargin > 0 && nargin < 4)
    {
      octave_value mode = (nargin == 2 || nargin == 3)
	? args(1) : octave_value ("r");

      octave_value arch = (nargin == 3)
	? args(2) : octave_value ("native");

      int fid = -1;

      octave_stream os = do_stream_open (args(0), mode, arch, "fopen", fid);

      if (os.is_valid ())
	{
	  if (os && ! error_state)
	    {
	      retval(1) = "";
	      retval(0) = octave_stream_list::insert (os);
	    }
	  else
	    {
	      int error_number = 0;

	      retval(1) = os.error (false, error_number);
	      retval(0) = -1.0;
	    }
	}
      else
	error ("fopen: internal error");
    }
  else
    print_usage ("fopen");

  return retval;
}

DEFUN (freport, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} freport ()\n\
Print a list of which files have been opened, and whether they are open\n\
for reading, writing, or both.  For example,\n\
\n\
@example\n\
@group\n\
freport ()\n\
\n\
     @print{}  number  mode  name\n\
     @print{} \n\
     @print{}       0     r  stdin\n\
     @print{}       1     w  stdout\n\
     @print{}       2     w  stderr\n\
     @print{}       3     r  myfile\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 0)
    warning ("freport: ignoring extra arguments");

  octave_stdout << octave_stream_list::list_open_files ();

  return retval;
}

DEFUN (frewind, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} frewind (@var{fid})\n\
Move the file pointer to the beginning of the file @var{fid}, returning\n\
1 for success, and 0 if an error was encountered.  It is equivalent to\n\
@code{fseek (@var{fid}, 0, SEEK_SET)}.\n\
@end deftypefn")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_stream os = octave_stream_list::lookup (args(0), "frewind");

      if (! error_state)
	retval = static_cast<double> (os.rewind ());
    }
  else
    print_usage ("frewind");

  return retval;
}

DEFUN (fseek, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} fseek (@var{fid}, @var{offset}, @var{origin})\n\
Set the file pointer to any location within the file @var{fid}.  The\n\
pointer is positioned @var{offset} characters from the @var{origin},\n\
which may be one of the predefined variables @code{SEEK_CUR} (current\n\
position), @code{SEEK_SET} (beginning), or @code{SEEK_END} (end of\n\
file). If @var{origin} is omitted, @code{SEEK_SET} is assumed.  The\n\
offset must be zero, or a value returned by @code{ftell} (in which case\n\
@var{origin} must be @code{SEEK_SET}.\n\
@end deftypefn")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 2 || nargin == 3)
    {
      octave_stream os = octave_stream_list::lookup (args(0), "fseek");

      if (! error_state)
	{
	  octave_value origin_arg = (nargin == 3)
	    ? args(2) : octave_value (-1.0);

	  retval = static_cast<double> (os.seek (args(1), origin_arg));
	}
    }
  else
    print_usage ("fseek");

  return retval;
}

DEFUN (ftell, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ftell (@var{fid})\n\
Return the position of the file pointer as the number of characters\n\
from the beginning of the file @var{fid}.\n\
@end deftypefn")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_stream os = octave_stream_list::lookup (args(0), "ftell");

      if (! error_state)
	retval = static_cast<double> (os.tell ());
    }
  else
    print_usage ("ftell");

  return retval;
}

DEFUN (fprintf, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} fprintf (@var{fid}, @var{template}, @dots{})\n\
This function is just like @code{printf}, except that the output is\n\
written to the stream @var{fid} instead of @code{stdout}.\n\
@end deftypefn")
{
  double retval = -1.0;
  bool return_char_count = true;

  int nargin = args.length ();

  if (nargin > 1 || (nargin > 0 && args(0).is_string ()))
    {
      octave_stream os;
      int fmt_n = 0;

      if (args(0).is_string ()) 
	{
	  os = octave_stream_list::lookup (1, "fprintf");

	  // For compatibility with Matlab, which does not return the
	  // character count when behaving like printf (no file id
	  // parameter).

	  return_char_count = (nargout != 0);
	}
      else
	{
	  fmt_n = 1;
	  os = octave_stream_list::lookup (args(0), "fprintf");
	}

      if (! error_state)
	{
	  if (args(fmt_n).is_string ())
	    {
	      std::string fmt = args(fmt_n).string_value ();

	      octave_value_list tmp_args;

	      if (nargin > 1 + fmt_n)
		{
		  tmp_args.resize (nargin-fmt_n-1, octave_value ());

		  for (int i = fmt_n + 1; i < nargin; i++)
		    tmp_args(i-fmt_n-1) = args(i);
		}

	      retval = os.printf (fmt, tmp_args);
	    }
	  else
	    ::error ("fprintf: format must be a string");
	}
    }
  else
    print_usage ("fprintf");

  if (return_char_count)
    return retval;
  else
    return octave_value();
}

DEFUN (fputs, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} fputs (@var{fid}, @var{string})\n\
Write a string to a file with no formatting.\n\
@end deftypefn")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 2)
    {
      octave_stream os = octave_stream_list::lookup (args(0), "fputs");

      if (! error_state)
	retval = os.puts (args(1));
    }
  else
    print_usage ("fputs");

  return retval;
}

DEFUN (sprintf, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} sprintf (@var{template}, @dots{})\n\
This is like @code{printf}, except that the output is returned as a\n\
string.  Unlike the C library function, which requires you to provide a\n\
suitably sized string as an argument, Octave's @code{sprintf} function\n\
returns the string, automatically sized to hold all of the items\n\
converted.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 0)
    {
      retval(2) = -1.0;
      retval(1) = "unknown error";
      retval(0) = "";

      octave_ostrstream *ostr = new octave_ostrstream ();

      octave_stream os (ostr);

      if (os.is_valid ())
	{
	  if (args(0).is_string ())
	    {
	      std::string fmt = args(0).string_value ();

	      octave_value_list tmp_args;

	      if (nargin > 1)
		{
		  tmp_args.resize (nargin-1, octave_value ());

		  for (int i = 1; i < nargin; i++)
		    tmp_args(i-1) = args(i);
		}

	      retval(2) = static_cast<double> (os.printf (fmt, tmp_args));
	      retval(1) = os.error ();
	      retval(0) = ostr->str ();
	    }
	  else
	    ::error ("sprintf: format must be a string");
	}
      else
	::error ("sprintf: unable to create output buffer");
    }
  else
    print_usage ("sprintf");

  return retval;
}

DEFUN (fscanf, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{val}, @var{count}] =} fscanf (@var{fid}, @var{template}, @var{size})\n\
@deftypefnx {Built-in Function} {[@var{v1}, @var{v2}, @dots{}, @var{count}] = } fscanf (@var{fid}, @var{template}, \"C\")\n\
In the first form, read from @var{fid} according to @var{template},\n\
returning the result in the matrix @var{val}.\n\
\n\
The optional argument @var{size} specifies the amount of data to read\n\
and may be one of\n\
\n\
@table @code\n\
@item Inf\n\
Read as much as possible, returning a column vector.\n\
\n\
@item @var{nr}\n\
Read up to @var{nr} elements, returning a column vector.\n\
\n\
@item [@var{nr}, Inf]\n\
Read as much as possible, returning a matrix with @var{nr} rows.  If the\n\
number of elements read is not an exact multiple of @var{nr}, the last\n\
column is padded with zeros.\n\
\n\
@item [@var{nr}, @var{nc}]\n\
Read up to @code{@var{nr} * @var{nc}} elements, returning a matrix with\n\
@var{nr} rows.  If the number of elements read is not an exact multiple\n\
of @var{nr}, the last column is padded with zeros.\n\
@end table\n\
\n\
@noindent\n\
If @var{size} is omitted, a value of @code{Inf} is assumed.\n\
\n\
A string is returned if @var{template} specifies only character\n\
conversions.\n\
\n\
The number of items successfully read is returned in @var{count}.\n\
\n\
In the second form, read from @var{fid} according to @var{template},\n\
with each conversion specifier in @var{template} corresponding to a\n\
single scalar return value.  This form is more `C-like', and also\n\
compatible with previous versions of Octave.  The number of successful\n\
conversions is returned in @var{count}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 3 && args(2).is_string ())
    {
      octave_stream os = octave_stream_list::lookup (args(0), "fscanf");

      if (! error_state)
	{
	  if (args(1).is_string ())
	    {
	      std::string fmt = args(1).string_value ();

	      retval = os.oscanf (fmt);
	    }
	  else
	    ::error ("fscanf: format must be a string");
	}
    }
  else
    {
      retval (1) = 0.0;
      retval (0) = Matrix ();

      if (nargin == 2 || nargin == 3)
	{
	  octave_stream os = octave_stream_list::lookup (args(0), "fscanf");

	  if (! error_state)
	    {
	      if (args(1).is_string ())
		{
		  std::string fmt = args(1).string_value ();

		  int count = 0;

		  Array<double> size = (nargin == 3)
		    ? args(2).vector_value () : Array<double> (1, octave_Inf);

		  if (! error_state)
		    {
		      octave_value tmp = os.scanf (fmt, size, count);

		      retval(1) = static_cast<double> (count);
		      retval(0) = tmp;
		    }
		}
	      else
		::error ("fscanf: format must be a string");
	    }
	}
      else
	print_usage ("fscanf");
    }

  return retval;
}

DEFUN (sscanf, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{val}, @var{count}] =} sscanf (@var{string}, @var{template}, @var{size})\n\
@deftypefnx {Built-in Function} {[@var{v1}, @var{v2}, @dots{}, @var{count}] = } sscanf (@var{string}, @var{template}, \"C\")\n\
This is like @code{fscanf}, except that the characters are taken from the\n\
string @var{string} instead of from a stream.  Reaching the end of the\n\
string is treated as an end-of-file condition.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 3 && args(2).is_string ())
    {
      if (args(0).is_string ())
	{
	  std::string data = args(0).string_value ();

	  octave_stream os = octave_istrstream::create (data);

	  if (os.is_valid ())
	    {
	      if (args(1).is_string ())
		{
		  std::string fmt = args(1).string_value ();

		  retval = os.oscanf (fmt);
		}
	      else
		::error ("sscanf: format must be a string");
	    }
	  else
	    ::error ("sscanf: unable to create temporary input buffer");
	}
      else
	::error ("sscanf: first argument must be a string");
    }
  else
    {
      if (nargin == 2 || nargin == 3)
	{
	  retval(3) = -1.0;
	  retval(2) = "unknown error";
	  retval(1) = 0.0;
	  retval(0) = Matrix ();

	  if (args(0).is_string ())
	    {
	      std::string data = args(0).string_value ();

	      octave_stream os = octave_istrstream::create (data);

	      if (os.is_valid ())
		{
		  if (args(1).is_string ())
		    {
		      std::string fmt = args(1).string_value ();

		      int count = 0;

		      Array<double> size = (nargin == 3)
			? args(2).vector_value ()
			: Array<double> (1, octave_Inf);

		      octave_value tmp = os.scanf (fmt, size, count);

		      // XXX FIXME XXX -- is this the right thing to do?
		      // Extract error message first, because getting
		      // position will clear it.
		      std::string errmsg = os.error ();

		      retval(3) = static_cast<double> (os.tell () + 1);
		      retval(2) = errmsg;
		      retval(1) = static_cast<double> (count);
		      retval(0) = tmp;
		    }
		  else
		    ::error ("sscanf: format must be a string");
		}
	      else
		::error ("sscanf: unable to create temporary input buffer");
	    }
	  else
	    ::error ("sscanf: first argument must be a string");
	}
      else
	print_usage ("sscanf");
    }

  return retval;
}

DEFUN (scanf, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{val}, @var{count}] =} scanf (@var{template}, @var{size})\n\
@deftypefnx {Built-in Function} {[@var{v1}, @var{v2}, @dots{}, @var{count}]] = } scanf (@var{template}, \"C\")\n\
This is equivalent to calling @code{fscanf} with @var{fid} = @code{stdin}.\n\
\n\
It is currently not useful to call @code{scanf} in interactive\n\
programs.\n\
@end deftypefn")
{
  int nargin = args.length ();

  octave_value_list tmp_args (nargin+1, octave_value ());

  tmp_args (0) = 0.0;
  for (int i = 0; i < nargin; i++)
    tmp_args (i+1) = args (i);

  return Ffscanf (tmp_args, nargout);
}

static octave_value
do_fread (octave_stream& os, const octave_value& size_arg,
	  const octave_value& prec_arg, const octave_value& skip_arg,
	  const octave_value& arch_arg, int& count)
{
  octave_value retval;

  count = -1;

  Array<double> size = size_arg.vector_value ();

  if (! error_state)
    {
      std::string prec = prec_arg.string_value ();

      if (! error_state)
	{
	  oct_data_conv::data_type dt
	    = oct_data_conv::string_to_data_type (prec);

	  if (! error_state)
	    {
	      int skip = skip_arg.int_value (true);

	      if (! error_state)
		{
		  std::string arch = arch_arg.string_value ();

		  if (! error_state)
		    {
		      oct_mach_info::float_format flt_fmt
			= oct_mach_info::string_to_float_format (arch);

		      if (! error_state)
			retval = os.read (size, dt, skip, flt_fmt, count);
		    }
		  else
		    ::error ("fread: architecture type must be a string");
		}
	      else
		::error ("fread: skip must be an integer");
	    }
	  else
	    ::error ("fread: invalid data type specified");
	}
      else
	::error ("fread: precision must be a string");
    }
  else
    ::error ("fread: invalid size specified");

  return retval;
}

DEFUN (fread, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{val}, @var{count}] =} fread (@var{fid}, @var{size}, @var{precision}, @var{skip}, @var{arch})\n\
Read binary data of type @var{precision} from the specified file ID\n\
@var{fid}.\n\
\n\
The optional argument @var{size} specifies the amount of data to read\n\
and may be one of\n\
\n\
@table @code\n\
@item Inf\n\
Read as much as possible, returning a column vector.\n\
\n\
@item @var{nr}\n\
Read up to @var{nr} elements, returning a column vector.\n\
\n\
@item [@var{nr}, Inf]\n\
Read as much as possible, returning a matrix with @var{nr} rows.  If the\n\
number of elements read is not an exact multiple of @var{nr}, the last\n\
column is padded with zeros.\n\
\n\
@item [@var{nr}, @var{nc}]\n\
Read up to @code{@var{nr} * @var{nc}} elements, returning a matrix with\n\
@var{nr} rows.  If the number of elements read is not an exact multiple\n\
of @var{nr}, the last column is padded with zeros.\n\
@end table\n\
\n\
@noindent\n\
If @var{size} is omitted, a value of @code{Inf} is assumed.\n\
\n\
The optional argument @var{precision} is a string specifying the type of\n\
data to read and may be one of\n\
\n\
@table @code\n\
@item \"char\"\n\
@itemx \"char*1\"\n\
@itemx \"integer*1\"\n\
@itemx \"int8\"\n\
Single character.\n\
\n\
@item \"signed char\"\n\
@itemx \"schar\"\n\
Signed character.\n\
\n\
@item \"unsigned char\"\n\
@itemx \"uchar\"\n\
Unsigned character.\n\
\n\
@item \"short\"\n\
Short integer.\n\
\n\
@item \"unsigned short\"\n\
@itemx \"ushort\"\n\
Unsigned short integer.\n\
\n\
@item \"int\"\n\
Integer.\n\
\n\
@item \"unsigned int\"\n\
@itemx \"uint\"\n\
Unsigned integer.\n\
\n\
@item \"long\"\n\
Long integer.\n\
\n\
@item \"unsigned long\"\n\
@itemx \"ulong\"\n\
Unsigned long integer.\n\
\n\
@item \"float\"\n\
@itemx \"float32\"\n\
@itemx \"real*4\"\n\
Single precision float.\n\
\n\
@item \"double\"\n\
@itemx \"float64\"\n\
@itemx \"real*8\"\n\
Double precision float.\n\
\n\
@item \"integer*2\"\n\
@itemx \"int16\"\n\
Two byte integer.\n\
\n\
@item \"integer*4\"\n\
@itemx \"int32\"\n\
Four byte integer.\n\
@end table\n\
\n\
@noindent\n\
The default precision is @code{\"uchar\"}.\n\
\n\
The optional argument @var{skip} specifies the number of bytes to skip\n\
before each element is read.  If it is not specified, a value of 0 is\n\
assumed.\n\
\n\
The optional argument @var{arch} is a string specifying the data format\n\
for the file.  Valid values are\n\
\n\
@table @code\n\
@item \"native\"\n\
The format of the current machine.\n\
\n\
@item \"ieee-le\"\n\
IEEE big endian.\n\
\n\
@item \"ieee-be\"\n\
IEEE little endian.\n\
\n\
@item \"vaxd\"\n\
VAX D floating format.\n\
\n\
@item \"vaxg\"\n\
VAX G floating format.\n\
\n\
@item \"cray\"\n\
Cray floating format.\n\
@end table\n\
\n\
@noindent\n\
Conversions are currently only supported for @code{\"ieee-be\"} and\n\
@code{\"ieee-le\"} formats.\n\
\n\
The data read from the file is returned in @var{val}, and the number of\n\
values read is returned in @code{count}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 0 && nargin < 6)
    {
      retval(1) = -1.0;
      retval(0) = Matrix ();

      octave_stream os = octave_stream_list::lookup (args(0), "fread");

      if (! error_state)
	{
	  octave_value size = (nargin > 1)
	    ? args(1) : octave_value (octave_Inf);

	  octave_value prec = (nargin > 2)
	    ? args(2) : octave_value ("uchar");

	  octave_value skip = (nargin > 3)
	    ? args(3) : octave_value (0.0);

	  octave_value arch = (nargin > 4)
	    ? args(4) : octave_value ("unknown");

	  int count = -1;

	  octave_value tmp = do_fread (os, size, prec, skip, arch, count);

	  retval(1) = static_cast<double> (count);
	  retval(0) = tmp;
	}
    }
  else
    print_usage ("fread");

  return retval;
}

static int
do_fwrite (octave_stream& os, const octave_value& data,
	   const octave_value& prec_arg, const octave_value& skip_arg,
	   const octave_value& arch_arg)
{
  int retval = -1;

  std::string prec = prec_arg.string_value ();

  if (! error_state)
    {
      oct_data_conv::data_type dt
	= oct_data_conv::string_to_data_type (prec);

      if (! error_state)
	{
	  int skip = skip_arg.int_value (true);

	  if (! error_state)
	    {
	      std::string arch = arch_arg.string_value ();

	      if (! error_state)
		{
		  oct_mach_info::float_format flt_fmt
		    = oct_mach_info::string_to_float_format (arch);

		  if (! error_state)
		    retval = os.write (data, dt, skip, flt_fmt);
		}
	      else
		::error ("fwrite: architecture type must be a string");
	    }
	  else
	    ::error ("fwrite: skip must be an integer");
	}
      else
	::error ("fwrite: invalid precision specified");
    }
  else
    ::error ("fwrite: precision must be a string");

  return retval;
}

DEFUN (fwrite, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{count} =} fwrite (@var{fid}, @var{data}, @var{precision}, @var{skip}, @var{arch})\n\
Write data in binary form of type @var{precision} to the specified file\n\
ID @var{fid}, returning the number of values successfully written to the\n\
file.\n\
\n\
The argument @var{data} is a matrix of values that are to be written to\n\
the file.  The values are extracted in column-major order.\n\
\n\
The remaining arguments @var{precision}, @var{skip}, and @var{arch} are\n\
optional, and are interpreted as described for @code{fread}.\n\
\n\
The behavior of @code{fwrite} is undefined if the values in @var{data}\n\
are too large to fit in the specified precision.\n\
@end deftypefn")
{
  octave_value retval = -1.0;

  int nargin = args.length ();

  if (nargin > 1 && nargin < 6)
    {
      octave_stream os = octave_stream_list::lookup (args(0), "fwrite");

      if (! error_state)
	{
	  octave_value data = args(1);

	  octave_value prec = (nargin > 2)
	    ? args(2) : octave_value ("uchar");

	  octave_value skip = (nargin > 3)
	    ? args(3) : octave_value (0.0);

	  octave_value arch = (nargin > 4)
	    ? args(4) : octave_value ("unknown");

	  double status = do_fwrite (os, data, prec, skip, arch);

	  retval = status;
	}
    }
  else
    print_usage ("fwrite");

  return retval;
}

DEFUN (feof, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} feof (@var{fid})\n\
Return 1 if an end-of-file condition has been encountered for a given\n\
file and 0 otherwise.  Note that it will only return 1 if the end of the\n\
file has already been encountered, not if the next read operation will\n\
result in an end-of-file condition.\n\
@end deftypefn")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_stream os = octave_stream_list::lookup (args(0), "feof");

      if (! error_state)
	retval = os.eof () ? 1.0 : 0.0;
    }
  else
    print_usage ("feof");

  return retval;
}

DEFUN (ferror, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ferror (@var{fid})\n\
Return 1 if an error condition has been encountered for a given file\n\
and 0 otherwise.  Note that it will only return 1 if an error has\n\
already been encountered, not if the next operation will result in an\n\
error condition.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      octave_stream os = octave_stream_list::lookup (args(0), "ferror");

      if (! error_state)
	{
	  bool clear = false;

	  if (nargin == 2)
	    {
	      std::string opt = args(1).string_value ();

	      if (! error_state)
		clear = (opt == "clear");
	      else
		return retval;
	    }

	  int error_number = 0;

	  std::string error_message = os.error (clear, error_number);

	  retval(1) = static_cast<double> (error_number);
	  retval(0) = error_message;
	}
    }
  else
    print_usage ("ferror");

  return retval;
}

DEFUN (popen, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {fid =} popen (@var{command}, @var{mode})\n\
Start a process and create a pipe.  The name of the command to run is\n\
given by @var{command}.  The file identifier corresponding to the input\n\
or output stream of the process is returned in @var{fid}.  The argument\n\
@var{mode} may be\n\
\n\
@table @code\n\
@item \"r\"\n\
The pipe will be connected to the standard output of the process, and\n\
open for reading.\n\
\n\
@item \"w\"\n\
The pipe will be connected to the standard input of the process, and\n\
open for writing.\n\
@end table\n\
\n\
For example,\n\
\n\
@example\n\
@group\n\
fid = popen (\"ls -ltr / | tail -3\", \"r\");\n\
while (isstr (s = fgets (fid)))\n\
  fputs (stdout, s);\n\
endwhile\n\
     @print{} drwxr-xr-x  33 root  root  3072 Feb 15 13:28 etc\n\
     @print{} drwxr-xr-x   3 root  root  1024 Feb 15 13:28 lib\n\
     @print{} drwxrwxrwt  15 root  root  2048 Feb 17 14:53 tmp\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  octave_value retval = -1.0;

  int nargin = args.length ();

  if (nargin == 2)
    {
      std::string name = args(0).string_value ();

      if (! error_state)
	{
	  std::string mode = args(1).string_value ();

	  if (! error_state)
	    {
	      if (mode == "r")
		{
		  octave_stream ips = octave_iprocstream::create (name);

		  retval = octave_stream_list::insert (ips);
		}
	      else if (mode == "w")
		{
		  octave_stream ops = octave_oprocstream::create (name);

		  retval = octave_stream_list::insert (ops);
		}
	      else
		::error ("popen: invalid mode specified");
	    }
	  else
	    ::error ("popen: mode must be a string");
	}
      else
	::error ("popen: name must be a string");
    }
  else
    print_usage ("popen");

  return retval;
}

DEFUN (pclose, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} pclose (@var{fid})\n\
Close a file identifier that was opened by @code{popen}.  You may also\n\
use @code{fclose} for the same purpose.\n\
@end deftypefn")
{
  double retval = -1.0;

  int nargin = args.length ();

  if (nargin == 1)
    retval = static_cast<double> (octave_stream_list::remove (args(0),
				  "pclose"));
  else
    print_usage ("pclose");

  return retval;
}

DEFUN (tmpnam, args, ,
 "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} tmpnam ()\n\
Return a unique temporary file name as a string.\n\
\n\
Since the named file is not opened, by @code{tmpnam}, it\n\
is possible (though relatively unlikely) that it will not be available\n\
by the time your program attempts to open it.\n\
@end deftypefn")
{
  octave_value retval;

  int len = args.length ();

  if (len < 3)
    {
      std::string dir = len > 0 ? args(0).string_value () : std::string ();
      std::string pfx = len > 1 ? args(1).string_value () : std::string ("oct-");

      if (! error_state)
	retval = file_ops::tempnam (dir, pfx);
    }
  else
    print_usage ("tmpnam");

  return retval;
}

DEFALIAS (octave_tmp_file_name, tmpnam);

static int
convert (int x, int ibase, int obase)
{
  int retval = 0;

  int tmp = x % obase;

  if (tmp > ibase - 1)
    ::error ("umask: invalid digit");
  else
    {
      retval = tmp;
      int mult = ibase;
      while ((x = (x - tmp) / obase))
	{
	  tmp = x % obase;
	  if (tmp > ibase - 1)
	    {
	      ::error ("umask: invalid digit");
	      break;
	    }
	  retval += mult * tmp;
	  mult *= ibase;
	}
    }

  return retval;
}

DEFUN (umask, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} umask (@var{mask})\n\
Set the permission mask for file creation.  The parameter @var{mask}\n\
 is an integer, interpreted as an octal number.  If successful,\n\
 returns the previous value of the mask (as an integer to be\n\
 interpreted as an octal number); otherwise an error message is printed.\n\
@end deftypefn")
{
  octave_value_list retval;

  int status = 0;

  if (args.length () == 1)
    {
      int mask = args(0).int_value (true);

      if (! error_state)
	{
	  if (mask < 0)
	    {
	      status = -1;
	      ::error ("umask: MASK must be a positive integer value");
	    }
	  else
	    {
	      int oct_mask = convert (mask, 8, 10);

	      if (! error_state)
		status = convert (file_ops::umask (oct_mask), 10, 8);
	    }
	}
      else
	{
	  status = -1;
	  ::error ("umask: expecting integer argument");
	}
    }
  else
    print_usage ("umask");

  if (status >= 0)
    retval(0) = static_cast<double> (status);

  return retval;
}

void
symbols_of_file_io (void)
{
  // NOTE: the values of SEEK_SET, SEEK_CUR, and SEEK_END have to be
  // this way for Matlab compatibility.

  DEFCONSTX ("SEEK_SET", SBV_SEEK_SET, -1.0,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} SEEK_SET\n\
@defvrx {Built-in Variable} SEEK_CUR\n\
@defvrx {Built-in Variable} SEEK_END\n\
These variables may be used as the optional third argument for the\n\
function @code{fseek}.\n\
\n\
@table @code\n\
@item SEEK_SET\n\
Position file relative to the beginning.\n\
\n\
@item SEEK_CUR\n\
Position file relative to the current position.\n\
\n\
@item SEEK_END\n\
used with fseek to position file relative to the end.\n\
@end table\n\
@end defvr");

  DEFCONSTX ("SEEK_CUR", SBV_SEEK_CUR, 0.0,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} SEEK_CUR\n\
See SEEK_SET.\n\
@end defvr");

  DEFCONSTX ("SEEK_END", SBV_SEEK_END, 1.0,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} SEEK_END\n\
See SEEK_SET.\n\
@end defvr");

  DEFCONSTX ("stdin", SBV_stdin, stdin_file,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} stdin\n\
The standard input stream (file id 0).  When Octave is used\n\
interactively, this is filtered through the command line editing\n\
functions.\n\
@end defvr");

  DEFCONSTX ("stdout", SBV_stdout, stdout_file,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} stdout\n\
The standard output stream (file id 1).  Data written to the\n\
standard output is normally filtered through the pager.\n\
@end defvr");

  DEFCONSTX ("stderr", SBV_stderr, stderr_file,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} stderr\n\
The standard error stream (file id 2).  Even if paging is turned on,\n\
the standard error is not sent to the pager.  It is useful for error\n\
messages and prompts.\n\
@end defvr");

}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
