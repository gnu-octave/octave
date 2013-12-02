/*

Copyright (C) 1993-2013 John W. Eaton

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

// Extensively revised by John W. Eaton <jwe@octave.org>,
// April 1996.

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cerrno>
#include <cstdio>

#include <iostream>
#include <limits>
#include <stack>
#include <vector>

#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>

#ifdef HAVE_ZLIB_H
#include <zlib.h>
#endif

#include "error.h"
#include "file-ops.h"
#include "file-stat.h"
#include "lo-ieee.h"
#include "oct-env.h"
#include "oct-locbuf.h"

#include "defun.h"
#include "file-io.h"
#include "load-path.h"
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

static octave_stream stdin_stream;
static octave_stream stdout_stream;
static octave_stream stderr_stream;

void
initialize_file_io (void)
{
  stdin_stream = octave_istream::create (&std::cin, "stdin");

  // This uses octave_stdout (see pager.h), not std::cout so that Octave's
  // standard output stream will pass through the pager.

  stdout_stream = octave_ostream::create (&octave_stdout, "stdout");

  stderr_stream = octave_ostream::create (&std::cerr, "stderr");

  stdin_file = octave_stream_list::insert (stdin_stream);
  stdout_file = octave_stream_list::insert (stdout_stream);
  stderr_file = octave_stream_list::insert (stderr_stream);
}

void
close_files (void)
{
  octave_stream_list::clear ();
}

// List of files to delete when we exit or crash.
//
// FIXME: this should really be static,
//        but that causes problems on some systems.
std::stack <std::string> tmp_files;

void
mark_for_deletion (const std::string& file)
{
  tmp_files.push (file);
}

void
cleanup_tmp_files (void)
{
  while (! tmp_files.empty ())
    {
      std::string filename = tmp_files.top ();
      tmp_files.pop ();
      gnulib::unlink (filename.c_str ());
    }
}

static void
normalize_fopen_mode (std::string& mode, bool& use_zlib)
{
  use_zlib = false;

  if (! mode.empty ())
    {
      // Could probably be faster, but does it really matter?

      // Accept 'W', 'R', and 'A' as 'w', 'r', and 'a' but we warn about
      // them because Matlab says they don't perform "automatic
      // flushing" but we don't know precisely what action that implies.

      size_t pos = mode.find ('W');

      if (pos != std::string::npos)
        {
          warning_with_id ("Octave:fopen-mode",
                           "fopen: treating mode \"W\" as equivalent to \"w\"");
          mode[pos] = 'w';
        }

      pos = mode.find ('R');

      if (pos != std::string::npos)
        {
          warning_with_id ("Octave:fopen-mode",
                           "fopen: treating mode \"R\" as equivalent to \"r\"");
          mode[pos] = 'r';
        }

      pos = mode.find ('A');

      if (pos != std::string::npos)
        {
          warning_with_id ("Octave:fopen-mode",
                           "fopen: treating mode \"A\" as equivalent to \"a\"");
          mode[pos] = 'a';
        }

      pos = mode.find ('z');

      if (pos != std::string::npos)
        {
#if defined (HAVE_ZLIB)
          use_zlib = true;
          mode.erase (pos, 1);
#else
          error ("this version of Octave does not support gzipped files");
#endif
        }

      if (! error_state)
        {
          // Use binary mode if 't' is not specified, but don't add
          // 'b' if it is already present.

          size_t bpos = mode.find ('b');
          size_t tpos = mode.find ('t');

          if (bpos == std::string::npos && tpos == std::string::npos)
            mode += 'b';
        }
    }
}

static std::ios::openmode
fopen_mode_to_ios_mode (const std::string& mode)
{
  std::ios::openmode retval = std::ios::in;

  if (! error_state)
    {
      if (mode == "rt")
        retval = std::ios::in;
      else if (mode == "wt")
        retval = std::ios::out | std::ios::trunc;
      else if (mode == "at")
        retval = std::ios::out | std::ios::app;
      else if (mode == "r+t" || mode == "rt+")
        retval = std::ios::in | std::ios::out;
      else if (mode == "w+t" || mode == "wt+")
        retval = std::ios::in | std::ios::out | std::ios::trunc;
      else if (mode == "a+t" || mode == "at+")
        retval = std::ios::in | std::ios::out | std::ios::app;
      else if (mode == "rb" || mode == "r")
        retval = std::ios::in | std::ios::binary;
      else if (mode == "wb" || mode == "w")
        retval = std::ios::out | std::ios::trunc | std::ios::binary;
      else if (mode == "ab" || mode == "a")
        retval = std::ios::out | std::ios::app | std::ios::binary;
      else if (mode == "r+b" || mode == "rb+" || mode == "r+")
        retval = std::ios::in | std::ios::out | std::ios::binary;
      else if (mode == "w+b" || mode == "wb+" || mode == "w+")
        retval = (std::ios::in | std::ios::out | std::ios::trunc
                  | std::ios::binary);
      else if (mode == "a+b" || mode == "ab+" || mode == "a+")
        retval = (std::ios::in | std::ios::out | std::ios::app
                  | std::ios::binary);
      else
        ::error ("invalid mode specified");
    }

  return retval;
}

DEFUN (fclose, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} fclose (@var{fid})\n\
@deftypefnx {Built-in Function} {} fclose (\"all\")\n\
Close the specified file.  If successful, @code{fclose} returns 0,\n\
otherwise, it returns -1.  The second form of the @code{fclose} call closes\n\
all open files except @code{stdout}, @code{stderr}, and @code{stdin}.\n\
@seealso{fopen, freport}\n\
@end deftypefn")
{
  octave_value retval = -1;

  int nargin = args.length ();

  if (nargin == 1)
    retval = octave_stream_list::remove (args(0), "fclose");
  else
    print_usage ();

  return retval;
}

DEFUN (fclear, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} fclear (@var{fid})\n\
Clear the stream state for the specified file.\n\
@seealso{fopen}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      int fid = octave_stream_list::get_file_number (args (0));

      octave_stream os = octave_stream_list::lookup (fid, "fclear");

      if (! error_state)
        os.clearerr ();
    }
  else
    print_usage ();

  return retval;
}

DEFUN (fflush, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} fflush (@var{fid})\n\
Flush output to @var{fid}.  This is useful for ensuring that all\n\
pending output makes it to the screen before some other event occurs.\n\
For example, it is always a good idea to flush the standard output\n\
stream before calling @code{input}.\n\
\n\
@code{fflush} returns 0 on success and an OS dependent error value\n\
(@minus{}1 on Unix) on error.\n\
@seealso{fopen, fclose}\n\
@end deftypefn")
{
  octave_value retval = -1;

  int nargin = args.length ();

  if (nargin == 1)
    {
      // FIXME: any way to avoid special case for stdout?

      int fid = octave_stream_list::get_file_number (args (0));

      if (fid == 1)
        {
          flush_octave_stdout ();

          retval = 0;
        }
      else
        {
          octave_stream os = octave_stream_list::lookup (fid, "fflush");

          if (! error_state)
            retval = os.flush ();
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUN (fgetl, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{str} =} fgetl (@var{fid})\n\
@deftypefnx {Built-in Function} {@var{str} =} fgetl (@var{fid}, @var{len})\n\
Read characters from a file, stopping after a newline, or EOF,\n\
or @var{len} characters have been read.  The characters read, excluding\n\
the possible trailing newline, are returned as a string.\n\
\n\
If @var{len} is omitted, @code{fgetl} reads until the next newline\n\
character.\n\
\n\
If there are no more characters to read, @code{fgetl} returns @minus{}1.\n\
\n\
To read a line and return the terminating newline see @code{fgets}.\n\
@seealso{fgets, fscanf, fread, fopen}\n\
@end deftypefn")
{
  static std::string who = "fgetl";

  octave_value_list retval;

  retval(1) = 0;
  retval(0) = -1;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      octave_stream os = octave_stream_list::lookup (args(0), who);

      if (! error_state)
        {
          octave_value len_arg = (nargin == 2) ? args(1) : octave_value ();

          bool err = false;

          std::string tmp = os.getl (len_arg, err, who);

          if (! (error_state || err))
            {
              retval(1) = tmp.length ();
              retval(0) = tmp;
            }
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUN (fgets, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{str} =} fgets (@var{fid})\n\
@deftypefnx {Built-in Function} {@var{str} =} fgets (@var{fid}, @var{len})\n\
Read characters from a file, stopping after a newline, or EOF,\n\
or @var{len} characters have been read.  The characters read, including\n\
the possible trailing newline, are returned as a string.\n\
\n\
If @var{len} is omitted, @code{fgets} reads until the next newline\n\
character.\n\
\n\
If there are no more characters to read, @code{fgets} returns @minus{}1.\n\
\n\
To read a line and discard the terminating newline see @code{fgetl}.\n\
@seealso{fputs, fgetl, fscanf, fread, fopen}\n\
@end deftypefn")
{
  static std::string who = "fgets";

  octave_value_list retval;

  retval(1) = 0.0;
  retval(0) = -1.0;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      octave_stream os = octave_stream_list::lookup (args(0), who);

      if (! error_state)
        {
          octave_value len_arg = (nargin == 2) ? args(1) : octave_value ();

          bool err = false;

          std::string tmp = os.gets (len_arg, err, who);

          if (! (error_state || err))
            {
              retval(1) = tmp.length ();
              retval(0) = tmp;
            }
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUN (fskipl, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{nlines} =} fskipl (@var{fid})\n\
@deftypefnx {Built-in Function} {@var{nlines} =} fskipl (@var{fid}, @var{count})\n\
@deftypefnx {Built-in Function} {@var{nlines} =} fskipl (@var{fid}, Inf)\n\
Read and skip @var{count} lines from the file descriptor @var{fid}.\n\
@code{fskipl} discards characters until an end-of-line is encountered exactly\n\
@var{count}-times, or until the end-of-file marker is found.\n\
\n\
If @var{count} is omitted, it defaults to 1.  @var{count} may also be\n\
@code{Inf}, in which case lines are skipped until the end of the file.\n\
This form is suitable for counting the number of lines in a file.\n\
\n\
Returns the number of lines skipped (end-of-line sequences encountered).\n\
@seealso{fgetl, fgets, fscanf, fopen}\n\
@end deftypefn")
{
  static std::string who = "fskipl";

  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      octave_stream os = octave_stream_list::lookup (args(0), who);

      if (! error_state)
        {
          octave_value count_arg = (nargin == 2) ? args(1) : octave_value ();

          bool err = false;

          off_t tmp = os.skipl (count_arg, err, who);

          if (! (error_state || err))
            retval = tmp;
        }
    }
  else
    print_usage ();

  return retval;
}


static octave_stream
do_stream_open (const std::string& name, const std::string& mode_arg,
                const std::string& arch, int& fid)
{
  octave_stream retval;

  fid = -1;

  std::string mode = mode_arg;
  bool use_zlib = false;
  normalize_fopen_mode (mode, use_zlib);

  std::ios::openmode md = fopen_mode_to_ios_mode (mode);

  if (! error_state)
    {
      oct_mach_info::float_format flt_fmt =
        oct_mach_info::string_to_float_format (arch);

      if (! error_state)
        {
          std::string fname = file_ops::tilde_expand (name);

          file_stat fs (fname);

          if (! (md & std::ios::out
                 || octave_env::absolute_pathname (fname)
                 || octave_env::rooted_relative_pathname (fname)))
            {
              if (! fs.exists ())
                {
                  std::string tmp
                    = octave_env::make_absolute (load_path::find_file (fname));

                  if (! tmp.empty ())
                    {
                      warning_with_id ("Octave:fopen-file-in-path",
                                       "fopen: file found in load path");
                      fname = tmp;
                    }
                }
            }

          if (! fs.is_dir ())
            {
#if defined (HAVE_ZLIB)
              if (use_zlib)
                {
                  FILE *fptr = gnulib::fopen (fname.c_str (), mode.c_str ());

                  int fd = fileno (fptr);

                  gzFile gzf = ::gzdopen (fd, mode.c_str ());

                  if (fptr)
                    retval = octave_zstdiostream::create (fname, gzf, fd,
                                                          md, flt_fmt);
                  else
                    retval.error (gnulib::strerror (errno));
                }
              else
#endif
                {
                  FILE *fptr = gnulib::fopen (fname.c_str (), mode.c_str ());

                  retval = octave_stdiostream::create (fname, fptr, md,
                                                       flt_fmt);

                  if (! fptr)
                    retval.error (gnulib::strerror (errno));
                }

            }
        }
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

DEFUN (fopen, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {[@var{fid}, @var{msg}] =} fopen (@var{name}, @var{mode}, @var{arch})\n\
@deftypefnx {Built-in Function} {@var{fid_list} =} fopen (\"all\")\n\
@deftypefnx {Built-in Function} {[@var{file}, @var{mode}, @var{arch}] =} fopen (@var{fid})\n\
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
The third form of the @code{fopen} function returns information about the\n\
open file given its file id.\n\
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
Open a file for writing.  The previous contents are discarded.\n\
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
Append a @qcode{\"t\"} to the mode string to open the file in text mode or a\n\
@qcode{\"b\"} to open in binary mode.  On Windows and Macintosh systems, text\n\
mode reading and writing automatically converts linefeeds to the\n\
appropriate line end character for the system (carriage-return linefeed\n\
on Windows, carriage-return on Macintosh).  The default if no mode is\n\
specified is binary mode.\n\
\n\
Additionally, you may append a @qcode{\"z\"} to the mode string to open a\n\
gzipped file for reading or writing.  For this to be successful, you\n\
must also open the file in binary mode.\n\
\n\
The parameter @var{arch} is a string specifying the default data format\n\
for the file.  Valid values for @var{arch} are:\n\
\n\
@table @samp\n\
@item native\n\
The format of the current machine (this is the default).\n\
\n\
@item ieee-be\n\
IEEE big endian format.\n\
\n\
@item ieee-le\n\
IEEE little endian format.\n\
@end table\n\
\n\
@noindent\n\
however, conversions are currently only supported for @samp{native}\n\
@samp{ieee-be}, and @samp{ieee-le} formats.\n\
@seealso{fclose, fgets, fgetl, fscanf, fread, fputs, fdisp, fprintf, fwrite, fskipl, fseek, frewind, ftell, feof, ferror, fclear, fflush, freport}\n\
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

          if (nargout < 2 && args(0).string_value () == "all")
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
    print_usage ();

  return retval;
}

DEFUN (freport, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} freport ()\n\
Print a list of which files have been opened, and whether they are open\n\
for reading, writing, or both.  For example:\n\
\n\
@example\n\
@group\n\
freport ()\n\
\n\
     @print{}  number  mode  name\n\
     @print{}\n\
     @print{}       0     r  stdin\n\
     @print{}       1     w  stdout\n\
     @print{}       2     w  stderr\n\
     @print{}       3     r  myfile\n\
@end group\n\
@end example\n\
@seealso{fopen, fclose}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 0)
    warning ("freport: ignoring extra arguments");

  octave_stdout << octave_stream_list::list_open_files ();

  return retval;
}

DEFUN (frewind, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} frewind (@var{fid})\n\
Move the file pointer to the beginning of the file @var{fid}, returning\n\
0 for success, and -1 if an error was encountered.  It is equivalent to\n\
@code{fseek (@var{fid}, 0, SEEK_SET)}.\n\
@seealso{fseek, ftell, fopen}\n\
@end deftypefn")
{
  octave_value retval;

  int result = -1;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_stream os = octave_stream_list::lookup (args(0), "frewind");

      if (! error_state)
        result = os.rewind ();
    }
  else
    print_usage ();

  if (nargout > 0)
    retval = result;

  return retval;
}

DEFUN (fseek, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} fseek (@var{fid}, @var{offset})\n\
@deftypefnx {Built-in Function} {} fseek (@var{fid}, @var{offset}, @var{origin})\n\
@deftypefnx {Built-in Function} {@var{status} =} fseek (@dots{})\n\
Set the file pointer to any location within the file @var{fid}.\n\
\n\
The pointer is positioned @var{offset} characters from the @var{origin},\n\
which may be one of the predefined variables @w{@code{SEEK_CUR}} (current\n\
position), @w{@code{SEEK_SET}} (beginning), or @w{@code{SEEK_END}} (end of\n\
file) or strings @qcode{\"cof\"}, @qcode{\"bof\"} or @qcode{\"eof\"}.  If\n\
@var{origin} is omitted, @w{@code{SEEK_SET}} is assumed.  @var{offset} may\n\
be positive, negative, or zero but not all combinations of @var{origin} and\n\
@var{offset} can be realized.\n\
\n\
Return 0 on success and -1 on error.\n\
@seealso{fskipl, frewind, ftell, fopen}\n\
@end deftypefn")
{
  octave_value retval = -1;

  int nargin = args.length ();

  if (nargin == 2 || nargin == 3)
    {
      octave_stream os = octave_stream_list::lookup (args(0), "fseek");

      if (! error_state)
        {
          octave_value origin_arg = (nargin == 3)
                                    ? args(2) : octave_value (-1.0);

          retval = os.seek (args(1), origin_arg);
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUN (ftell, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ftell (@var{fid})\n\
Return the position of the file pointer as the number of characters\n\
from the beginning of the file @var{fid}.\n\
@seealso{fseek, feof, fopen}\n\
@end deftypefn")
{
  octave_value retval = -1;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_stream os = octave_stream_list::lookup (args(0), "ftell");

      if (! error_state)
        retval = os.tell ();
    }
  else
    print_usage ();

  return retval;
}

DEFUN (fprintf, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} fprintf (@var{fid}, @var{template}, @dots{})\n\
This function is just like @code{printf}, except that the output is\n\
written to the stream @var{fid} instead of @code{stdout}.\n\
If @var{fid} is omitted, the output is written to @code{stdout}.\n\
@seealso{fputs, fdisp, fwrite, fscanf, printf, sprintf, fopen}\n\
@end deftypefn")
{
  static std::string who = "fprintf";

  octave_value retval;

  int result = -1;

  int nargin = args.length ();

  if (nargin > 1 || (nargin > 0 && args(0).is_string ()))
    {
      octave_stream os;
      int fmt_n = 0;

      if (args(0).is_string ())
        {
          os = octave_stream_list::lookup (1, who);
        }
      else
        {
          fmt_n = 1;
          os = octave_stream_list::lookup (args(0), who);
        }

      if (! error_state)
        {
          if (args(fmt_n).is_string ())
            {
              octave_value_list tmp_args;

              if (nargin > 1 + fmt_n)
                {
                  tmp_args.resize (nargin-fmt_n-1, octave_value ());

                  for (int i = fmt_n + 1; i < nargin; i++)
                    tmp_args(i-fmt_n-1) = args(i);
                }

              result = os.printf (args(fmt_n), tmp_args, who);
            }
          else
            ::error ("%s: format TEMPLATE must be a string", who.c_str ());
        }
    }
  else
    print_usage ();

  if (nargout > 0)
    retval = result;

  return retval;
}

DEFUN (printf, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} printf (@var{template}, @dots{})\n\
Print optional arguments under the control of the template string\n\
@var{template} to the stream @code{stdout} and return the number of\n\
characters printed.\n\
@ifclear OCTAVE_MANUAL\n\
\n\
See the Formatted Output section of the GNU Octave manual for a\n\
complete description of the syntax of the template string.\n\
@end ifclear\n\
@seealso{fprintf, sprintf, scanf}\n\
@end deftypefn")
{
  static std::string who = "printf";

  octave_value retval;

  int result = -1;

  int nargin = args.length ();

  if (nargin > 0)
    {
      if (args(0).is_string ())
        {
          octave_value_list tmp_args;

          if (nargin > 1)
            {
              tmp_args.resize (nargin-1, octave_value ());

              for (int i = 1; i < nargin; i++)
                tmp_args(i-1) = args(i);
            }

          result = stdout_stream.printf (args(0), tmp_args, who);
        }
      else
        ::error ("%s: format TEMPLATE must be a string", who.c_str ());
    }
  else
    print_usage ();

  if (nargout > 0)
    retval = result;

  return retval;
}

DEFUN (fputs, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} fputs (@var{fid}, @var{string})\n\
Write a string to a file with no formatting.\n\
\n\
Return a non-negative number on success and EOF on error.\n\
@seealso{fdisp, fprintf, fwrite, fopen}\n\
@end deftypefn")
{
  static std::string who = "fputs";

  octave_value retval = -1;

  int nargin = args.length ();

  if (nargin == 2)
    {
      octave_stream os = octave_stream_list::lookup (args(0), who);

      if (! error_state)
        retval = os.puts (args(1), who);
    }
  else
    print_usage ();

  return retval;
}

DEFUN (puts, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} puts (@var{string})\n\
Write a string to the standard output with no formatting.\n\
\n\
Return a non-negative number on success and EOF on error.\n\
@seealso{fputs, disp}\n\
@end deftypefn")
{
  static std::string who = "puts";

  octave_value retval = -1;

  if (args.length () == 1)
    retval = stdout_stream.puts (args(0), who);
  else
    print_usage ();

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
@seealso{printf, fprintf, sscanf}\n\
@end deftypefn")
{
  static std::string who = "sprintf";

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
          octave_value fmt_arg = args(0);

          if (fmt_arg.is_string ())
            {
              octave_value_list tmp_args;

              if (nargin > 1)
                {
                  tmp_args.resize (nargin-1, octave_value ());

                  for (int i = 1; i < nargin; i++)
                    tmp_args(i-1) = args(i);
                }

              retval(2) = os.printf (fmt_arg, tmp_args, who);
              retval(1) = os.error ();
              retval(0) = octave_value (ostr->str (),
                                        fmt_arg.is_sq_string () ? '\'' : '"');
            }
          else
            ::error ("%s: format TEMPLATE must be a string", who.c_str ());
        }
      else
        ::error ("%s: unable to create output buffer", who.c_str ());
    }
  else
    print_usage ();

  return retval;
}

DEFUN (fscanf, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {[@var{val}, @var{count}, @var{errmsg}] =} fscanf (@var{fid}, @var{template}, @var{size})\n\
@deftypefnx {Built-in Function} {[@var{v1}, @var{v2}, @dots{}, @var{count}, @var{errmsg}] =} fscanf (@var{fid}, @var{template}, \"C\")\n\
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
If an error occurs, @var{errmsg} contains a system-dependent error message.\n\
\n\
In the second form, read from @var{fid} according to @var{template},\n\
with each conversion specifier in @var{template} corresponding to a\n\
single scalar return value.  This form is more ``C-like'', and also\n\
compatible with previous versions of Octave.  The number of successful\n\
conversions is returned in @var{count}\n\
@ifclear OCTAVE_MANUAL\n\
\n\
See the Formatted Input section of the GNU Octave manual for a\n\
complete description of the syntax of the template string.\n\
@end ifclear\n\
@seealso{fgets, fgetl, fread, scanf, sscanf, fopen}\n\
@end deftypefn")
{
  static std::string who = "fscanf";

  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 3 && args(2).is_string ())
    {
      octave_stream os = octave_stream_list::lookup (args(0), who);

      if (! error_state)
        {
          if (args(1).is_string ())
            retval = os.oscanf (args(1), who);
          else
            ::error ("%s: format TEMPLATE must be a string", who.c_str ());
        }
    }
  else
    {
      retval(2) = "unknown error";
      retval(1) = 0.0;
      retval(0) = Matrix ();

      if (nargin == 2 || nargin == 3)
        {
          octave_stream os = octave_stream_list::lookup (args(0), who);

          if (! error_state)
            {
              if (args(1).is_string ())
                {
                  octave_idx_type count = 0;

                  Array<double> size = (nargin == 3)
                                       ? args(2).vector_value ()
                                       : Array<double> (dim_vector (1, 1),
                                                        lo_ieee_inf_value ());

                  if (! error_state)
                    {
                      octave_value tmp = os.scanf (args(1), size, count, who);

                      if (! error_state)
                        {
                          retval(2) = os.error ();
                          retval(1) = count;
                          retval(0) = tmp;
                        }
                    }
                }
              else
                ::error ("%s: format must be a string", who.c_str ());
            }
        }
      else
        print_usage ();
    }

  return retval;
}

static std::string
get_sscanf_data (const octave_value& val)
{
  std::string retval;

  if (val.is_string ())
    {
      octave_value tmp = val.reshape (dim_vector (1, val.numel ()));

      retval = tmp.string_value ();
    }
  else
    ::error ("sscanf: argument STRING must be a string");

  return retval;
}

DEFUN (sscanf, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {[@var{val}, @var{count}, @var{errmsg}, @var{pos}] =} sscanf (@var{string}, @var{template}, @var{size})\n\
@deftypefnx {Built-in Function} {[@var{v1}, @var{v2}, @dots{}, @var{count}, @var{errmsg}] =} sscanf (@var{string}, @var{template}, \"C\")\n\
This is like @code{fscanf}, except that the characters are taken from the\n\
string @var{string} instead of from a stream.  Reaching the end of the\n\
string is treated as an end-of-file condition.  In addition to the values\n\
returned by @code{fscanf}, the index of the next character to be read\n\
is returned in @var{pos}.\n\
@seealso{fscanf, scanf, sprintf}\n\
@end deftypefn")
{
  static std::string who = "sscanf";

  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 3 && args(2).is_string ())
    {
      std::string data = get_sscanf_data (args(0));

      if (! error_state)
        {
          octave_stream os = octave_istrstream::create (data);

          if (os.is_valid ())
            {
              if (args(1).is_string ())
                retval = os.oscanf (args(1), who);
              else
                ::error ("%s: format TEMPLATE must be a string", who.c_str ());
            }
          else
            ::error ("%s: unable to create temporary input buffer",
                     who.c_str ());
        }
      else
        ::error ("%s: argument STRING must be a string", who.c_str ());
    }
  else
    {
      if (nargin == 2 || nargin == 3)
        {
          retval(3) = -1.0;
          retval(2) = "unknown error";
          retval(1) = 0.0;
          retval(0) = Matrix ();

          std::string data = get_sscanf_data (args(0));

          if (! error_state)
            {
              octave_stream os = octave_istrstream::create (data);

              if (os.is_valid ())
                {
                  if (args(1).is_string ())
                    {
                      octave_idx_type count = 0;

                      Array<double> size = (nargin == 3)
                                           ? args(2).vector_value ()
                                           : Array<double> (dim_vector (1, 1),
                                                            lo_ieee_inf_value ());

                      octave_value tmp = os.scanf (args(1), size, count, who);

                      if (! error_state)
                        {
                          // FIXME: is this the right thing to do?
                          // Extract error message first, because getting
                          // position will clear it.
                          std::string errmsg = os.error ();

                          retval(3)
                            = (os.eof () ? data.length () : os.tell ()) + 1;
                          retval(2) = errmsg;
                          retval(1) = count;
                          retval(0) = tmp;
                        }
                    }
                  else
                    ::error ("%s: format TEMPLATE must be a string",
                             who.c_str ());
                }
              else
                ::error ("%s: unable to create temporary input buffer",
                         who.c_str  ());
            }
        }
      else
        print_usage ();
    }

  return retval;
}

DEFUN (scanf, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {[@var{val}, @var{count}, @var{errmsg}] =} scanf (@var{template}, @var{size})\n\
@deftypefnx {Built-in Function} {[@var{v1}, @var{v2}, @dots{}, @var{count}, @var{errmsg}]] =} scanf (@var{template}, \"C\")\n\
This is equivalent to calling @code{fscanf} with @var{fid} = @code{stdin}.\n\
\n\
It is currently not useful to call @code{scanf} in interactive\n\
programs.\n\
@seealso{fscanf, sscanf, printf}\n\
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
          const octave_value& arch_arg, octave_idx_type& count)
{
  octave_value retval;

  count = -1;

  Array<double> size = size_arg.vector_value ();

  if (! error_state)
    {
      std::string prec = prec_arg.string_value ();

      if (! error_state)
        {
          int block_size = 1;
          oct_data_conv::data_type input_type;
          oct_data_conv::data_type output_type;

          oct_data_conv::string_to_data_type (prec, block_size,
                                              input_type, output_type);

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
                        retval = os.read (size, block_size, input_type,
                                          output_type, skip, flt_fmt, count);
                    }
                  else
                    ::error ("fread: ARCH architecture type must be a string");
                }
              else
                ::error ("fread: SKIP must be an integer");
            }
          else
            ::error ("fread: invalid PRECISION specified");
        }
      else
        ::error ("fread: PRECISION must be a string");
    }
  else
    ::error ("fread: invalid SIZE specified");

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
@table @asis\n\
@item  @qcode{\"schar\"}\n\
@itemx @qcode{\"signed char\"}\n\
Signed character.\n\
\n\
@item  @qcode{\"uchar\"}\n\
@itemx @qcode{\"unsigned char\"}\n\
Unsigned character.\n\
\n\
@item  @qcode{\"int8\"}\n\
@itemx @qcode{\"integer*1\"}\n\
\n\
8-bit signed integer.\n\
\n\
@item  @qcode{\"int16\"}\n\
@itemx @qcode{\"integer*2\"}\n\
16-bit signed integer.\n\
\n\
@item  @qcode{\"int32\"}\n\
@itemx @qcode{\"integer*4\"}\n\
32-bit signed integer.\n\
\n\
@item  @qcode{\"int64\"}\n\
@itemx @qcode{\"integer*8\"}\n\
64-bit signed integer.\n\
\n\
@item @qcode{\"uint8\"}\n\
8-bit unsigned integer.\n\
\n\
@item @qcode{\"uint16\"}\n\
16-bit unsigned integer.\n\
\n\
@item @qcode{\"uint32\"}\n\
32-bit unsigned integer.\n\
\n\
@item @qcode{\"uint64\"}\n\
64-bit unsigned integer.\n\
\n\
@item  @qcode{\"single\"}\n\
@itemx @qcode{\"float32\"}\n\
@itemx @qcode{\"real*4\"}\n\
32-bit floating point number.\n\
\n\
@item  @qcode{\"double\"}\n\
@itemx @qcode{\"float64\"}\n\
@itemx @qcode{\"real*8\"}\n\
64-bit floating point number.\n\
\n\
@item  @qcode{\"char\"}\n\
@itemx @qcode{\"char*1\"}\n\
Single character.\n\
\n\
@item @qcode{\"short\"}\n\
Short integer (size is platform dependent).\n\
\n\
@item @qcode{\"int\"}\n\
Integer (size is platform dependent).\n\
\n\
@item @qcode{\"long\"}\n\
Long integer (size is platform dependent).\n\
\n\
@item  @qcode{\"ushort\"}\n\
@itemx @qcode{\"unsigned short\"}\n\
Unsigned short integer (size is platform dependent).\n\
\n\
@item  @qcode{\"uint\"}\n\
@itemx @qcode{\"unsigned int\"}\n\
Unsigned integer (size is platform dependent).\n\
\n\
@item  @qcode{\"ulong\"}\n\
@itemx @qcode{\"unsigned long\"}\n\
Unsigned long integer (size is platform dependent).\n\
\n\
@item @qcode{\"float\"}\n\
Single precision floating point number (size is platform dependent).\n\
@end table\n\
\n\
@noindent\n\
The default precision is @qcode{\"uchar\"}.\n\
\n\
The @var{precision} argument may also specify an optional repeat\n\
count.  For example, @samp{32*single} causes @code{fread} to read\n\
a block of 32 single precision floating point numbers.  Reading in\n\
blocks is useful in combination with the @var{skip} argument.\n\
\n\
The @var{precision} argument may also specify a type conversion.\n\
For example, @samp{int16=>int32} causes @code{fread} to read 16-bit\n\
integer values and return an array of 32-bit integer values.  By\n\
default, @code{fread} returns a double precision array.  The special\n\
form @samp{*TYPE} is shorthand for @samp{TYPE=>TYPE}.\n\
\n\
The conversion and repeat counts may be combined.  For example, the\n\
specification @samp{32*single=>single} causes @code{fread} to read\n\
blocks of single precision floating point values and return an array\n\
of single precision values instead of the default array of double\n\
precision values.\n\
\n\
The optional argument @var{skip} specifies the number of bytes to skip\n\
after each element (or block of elements) is read.  If it is not\n\
specified, a value of 0 is assumed.  If the final block read is not\n\
complete, the final skip is omitted.  For example,\n\
\n\
@example\n\
fread (f, 10, \"3*single=>single\", 8)\n\
@end example\n\
\n\
@noindent\n\
will omit the final 8-byte skip because the last read will not be\n\
a complete block of 3 values.\n\
\n\
The optional argument @var{arch} is a string specifying the data format\n\
for the file.  Valid values are\n\
\n\
@table @code\n\
@item @qcode{\"native\"}\n\
The format of the current machine.\n\
\n\
@item \"ieee-be\"\n\
IEEE big endian.\n\
\n\
@item \"ieee-le\"\n\
IEEE little endian.\n\
@end table\n\
\n\
The data read from the file is returned in @var{val}, and the number of\n\
values read is returned in @code{count}\n\
@seealso{fwrite, fgets, fgetl, fscanf, fopen}\n\
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
          octave_value size = lo_ieee_inf_value ();
          octave_value prec = "uchar";
          octave_value skip = 0;
          octave_value arch = "unknown";

          int idx = 1;

          if (nargin > idx && ! args(idx).is_string ())
            size = args(idx++);

          if (nargin > idx)
            prec = args(idx++);

          if (nargin > idx)
            skip = args(idx++);

          if (nargin > idx)
            arch = args(idx++);
          else if (skip.is_string ())
            {
              arch = skip;
              skip = 0;
            }

          octave_idx_type count = -1;

          octave_value tmp = do_fread (os, size, prec, skip, arch, count);

          retval(1) = count;
          retval(0) = tmp;
        }
    }
  else
    print_usage ();

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
      int block_size = 1;
      oct_data_conv::data_type output_type;

      oct_data_conv::string_to_data_type (prec, block_size, output_type);

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
                    retval = os.write (data, block_size, output_type,
                                       skip, flt_fmt);
                }
              else
                ::error ("fwrite: ARCH architecture type must be a string");
            }
          else
            ::error ("fwrite: SKIP must be an integer");
        }
      else
        ::error ("fwrite: invalid PRECISION specified");
    }
  else
    ::error ("fwrite: PRECISION must be a string");

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
@seealso{fread, fputs, fprintf, fopen}\n\
@end deftypefn")
{
  octave_value retval = -1;

  int nargin = args.length ();

  if (nargin > 1 && nargin < 6)
    {
      octave_stream os = octave_stream_list::lookup (args(0), "fwrite");

      if (! error_state)
        {
          octave_value prec = "uchar";
          octave_value skip = 0;
          octave_value arch = "unknown";

          int idx = 1;

          octave_value data = args(idx++);

          if (nargin > idx)
            prec = args(idx++);

          if (nargin > idx)
            skip = args(idx++);

          if (nargin > idx)
            arch = args(idx++);
          else if (skip.is_string ())
            {
              arch = skip;
              skip = 0;
            }

          double status = do_fwrite (os, data, prec, skip, arch);

          retval = status;
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUNX ("feof", Ffeof, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} feof (@var{fid})\n\
Return 1 if an end-of-file condition has been encountered for a given\n\
file and 0 otherwise.  Note that it will only return 1 if the end of the\n\
file has already been encountered, not if the next read operation will\n\
result in an end-of-file condition.\n\
@seealso{fread, fopen}\n\
@end deftypefn")
{
  octave_value retval = -1;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_stream os = octave_stream_list::lookup (args(0), "feof");

      if (! error_state)
        retval = os.eof () ? 1.0 : 0.0;
    }
  else
    print_usage ();

  return retval;
}

DEFUNX ("ferror", Fferror, args, ,
        "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {[@var{err}, @var{msg}] =} ferror (@var{fid})\n\
@deftypefnx {Built-in Function} {[@var{err}, @var{msg}] =} ferror (@var{fid}, \"clear\")\n\
Return 1 if an error condition has been encountered for the file ID\n\
@var{fid} and 0 otherwise.  Note that it will only return 1 if an error\n\
has already been encountered, not if the next operation will result in\n\
an error condition.\n\
\n\
The second argument is optional.  If it is supplied, also clear the\n\
error condition.\n\
@seealso{fclear, fopen}\n\
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

          retval(1) = error_number;
          retval(0) = error_message;
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUN (popen, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{fid} =} popen (@var{command}, @var{mode})\n\
Start a process and create a pipe.  The name of the command to run is\n\
given by @var{command}.  The file identifier corresponding to the input\n\
or output stream of the process is returned in @var{fid}.  The argument\n\
@var{mode} may be\n\
\n\
@table @code\n\
@item @qcode{\"r\"}\n\
The pipe will be connected to the standard output of the process, and\n\
open for reading.\n\
\n\
@item @qcode{\"w\"}\n\
The pipe will be connected to the standard input of the process, and\n\
open for writing.\n\
@end table\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
fid = popen (\"ls -ltr / | tail -3\", \"r\");\n\
while (ischar (s = fgets (fid)))\n\
  fputs (stdout, s);\n\
endwhile\n\
\n\
   @print{} drwxr-xr-x  33 root  root  3072 Feb 15 13:28 etc\n\
   @print{} drwxr-xr-x   3 root  root  1024 Feb 15 13:28 lib\n\
   @print{} drwxrwxrwt  15 root  root  2048 Feb 17 14:53 tmp\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  octave_value retval = -1;

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
                ::error ("popen: invalid MODE specified");
            }
          else
            ::error ("popen: MODE must be a string");
        }
      else
        ::error ("popen: COMMAND must be a string");
    }
  else
    print_usage ();

  return retval;
}

DEFUNX ("pclose", Fpclose, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} pclose (@var{fid})\n\
Close a file identifier that was opened by @code{popen}.  You may also\n\
use @code{fclose} for the same purpose.\n\
@end deftypefn")
{
  octave_value retval = -1;

  int nargin = args.length ();

  if (nargin == 1)
    retval = octave_stream_list::remove (args(0), "pclose");
  else
    print_usage ();

  return retval;
}

DEFUNX ("tmpnam", Ftmpnam, args, ,
        "-*- texinfo -*-\n\
@c List other forms of function in documentation index\n\
@findex octave_tmp_file_name\n\
\n\
@deftypefn  {Built-in Function} {} tmpnam ()\n\
@deftypefnx {Built-in Function} {} tmpnam (@var{dir})\n\
@deftypefnx {Built-in Function} {} tmpnam (@var{dir}, @var{prefix})\n\
Return a unique temporary file name as a string.\n\
\n\
If @var{prefix} is omitted, a value of @qcode{\"oct-\"} is used.\n\
If @var{dir} is also omitted, the default directory for temporary files\n\
is used.  If @var{dir} is provided, it must exist, otherwise the default\n\
directory for temporary files is used.  Since the named file is not\n\
opened, by @code{tmpnam}, it is possible (though relatively unlikely)\n\
that it will not be available by the time your program attempts to open it.\n\
@seealso{tmpfile, mkstemp, P_tmpdir}\n\
@end deftypefn")
{
  octave_value retval;

  int len = args.length ();

  if (len < 3)
    {
      std::string dir = len > 0 ? args(0).string_value () : std::string ();

      if (! error_state)
        {
          std::string pfx
            = len > 1 ? args(1).string_value () : std::string ("oct-");

          if (! error_state)
            retval = octave_tempnam (dir, pfx);
          else
            ::error ("PREFIX must be a string");
        }
      else
        ::error ("DIR argument must be a string");
    }
  else
    print_usage ();

  return retval;
}

DEFALIAS (octave_tmp_file_name, tmpnam);

DEFUN (tmpfile, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{fid}, @var{msg}] =} tmpfile ()\n\
Return the file ID corresponding to a new temporary file with a unique\n\
name.  The file is opened in binary read/write (@qcode{\"w+b\"}) mode.\n\
The file will be deleted automatically when it is closed or when Octave\n\
exits.\n\
\n\
If successful, @var{fid} is a valid file ID and @var{msg} is an empty\n\
string.  Otherwise, @var{fid} is -1 and @var{msg} contains a\n\
system-dependent error message.\n\
@seealso{tmpnam, mkstemp, P_tmpdir}\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = -1;

  int nargin = args.length ();

  if (nargin == 0)
    {
      FILE *fid = gnulib::tmpfile ();

      if (fid)
        {
          std::string nm;

          std::ios::openmode md = fopen_mode_to_ios_mode ("w+b");

          octave_stream s = octave_stdiostream::create (nm, fid, md);

          if (s)
            retval(0) = octave_stream_list::insert (s);
          else
            error ("tmpfile: failed to create octave_stdiostream object");

        }
      else
        {
          retval(1) = gnulib::strerror (errno);
          retval(0) = -1;
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUN (mkstemp, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{fid}, @var{name}, @var{msg}] =} mkstemp (@var{template}, @var{delete})\n\
Return the file ID corresponding to a new temporary file with a unique\n\
name created from @var{template}.  The last six characters of @var{template}\n\
must be @code{XXXXXX} and these are replaced with a string that makes the\n\
filename unique.  The file is then created with mode read/write and\n\
permissions that are system dependent (on GNU/Linux systems, the permissions\n\
will be 0600 for versions of glibc 2.0.7 and later).  The file is opened\n\
in binary mode and with the @w{@code{O_EXCL}} flag.\n\
\n\
If the optional argument @var{delete} is supplied and is true,\n\
the file will be deleted automatically when Octave exits.\n\
\n\
If successful, @var{fid} is a valid file ID, @var{name} is the name of\n\
the file, and @var{msg} is an empty string.  Otherwise, @var{fid}\n\
is -1, @var{name} is empty, and @var{msg} contains a system-dependent\n\
error message.\n\
@seealso{tmpfile, tmpnam, P_tmpdir}\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(2) = std::string ();
  retval(1) = std::string ();
  retval(0) = -1;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      std::string tmpl8 = args(0).string_value ();

      if (! error_state)
        {
          OCTAVE_LOCAL_BUFFER (char, tmp, tmpl8.size () + 1);
          strcpy (tmp, tmpl8.c_str ());

          int fd = gnulib::mkostemp (tmp, O_BINARY);

          if (fd < 0)
            {
              retval(2) = gnulib::strerror (errno);
              retval(0) = fd;
            }
          else
            {
              const char *fopen_mode = "w+b";

              FILE *fid = fdopen (fd, fopen_mode);

              if (fid)
                {
                  std::string nm = tmp;

                  std::ios::openmode md = fopen_mode_to_ios_mode (fopen_mode);

                  octave_stream s = octave_stdiostream::create (nm, fid, md);

                  if (s)
                    {
                      retval(1) = nm;
                      retval(0) = octave_stream_list::insert (s);

                      if (nargin == 2 && args(1).is_true ())
                        mark_for_deletion (nm);
                    }
                  else
                    error ("mkstemp: failed to create octave_stdiostream object");
                }
              else
                {
                  retval(2) = gnulib::strerror (errno);
                  retval(0) = -1;
                }
            }
        }
      else
        error ("mkstemp: TEMPLATE argument must be a string");
    }
  else
    print_usage ();

  return retval;
}

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

DEFUNX ("umask", Fumask, args, ,
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
                status = convert (octave_umask (oct_mask), 10, 8);
            }
        }
      else
        {
          status = -1;
          ::error ("umask: MASK must be an integer");
        }
    }
  else
    print_usage ();

  if (status >= 0)
    retval(0) = status;

  return retval;
}

static octave_value
const_value (const char *, const octave_value_list& args, int val)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0)
    retval = val;
  else
    print_usage ();

  return retval;
}

DEFUNX ("P_tmpdir", FP_tmpdir, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} P_tmpdir ()\n\
Return the default name of the directory for temporary files on\n\
this system.  The name of this directory is system dependent.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0)
    retval = get_P_tmpdir ();
  else
    print_usage ();

  return retval;
}

// NOTE: the values of SEEK_SET, SEEK_CUR, and SEEK_END have to be
// this way for Matlab compatibility.

DEFUNX ("SEEK_SET", FSEEK_SET, args, ,
        "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} SEEK_SET ()\n\
@deftypefnx {Built-in Function} {} SEEK_CUR ()\n\
@deftypefnx {Built-in Function} {} SEEK_END ()\n\
Return the numerical value to pass to @code{fseek} to perform\n\
one of the following actions:\n\
\n\
@table @code\n\
@item SEEK_SET\n\
Position file relative to the beginning.\n\
\n\
@item SEEK_CUR\n\
Position file relative to the current position.\n\
\n\
@item SEEK_END\n\
Position file relative to the end.\n\
@end table\n\
@seealso{fseek}\n\
@end deftypefn")
{
  return const_value ("SEEK_SET", args, -1);
}

DEFUNX ("SEEK_CUR", FSEEK_CUR, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} SEEK_CUR ()\n\
Return the numerical value to pass to @code{fseek} to\n\
position the file pointer relative to the current position.\n\
@seealso{SEEK_SET, SEEK_END}\n\
@end deftypefn")
{
  return const_value ("SEEK_CUR", args, 0);
}

DEFUNX ("SEEK_END", FSEEK_END, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} SEEK_END ()\n\
Return the numerical value to pass to @code{fseek} to\n\
position the file pointer relative to the end of the file.\n\
@seealso{SEEK_SET, SEEK_CUR}\n\
@end deftypefn")
{
  return const_value ("SEEK_END", args, 1);
}

static octave_value
const_value (const char *, const octave_value_list& args,
             const octave_value& val)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0)
    retval = val;
  else
    print_usage ();

  return retval;
}

DEFUNX ("stdin", Fstdin, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} stdin ()\n\
Return the numeric value corresponding to the standard input stream.\n\
When Octave is used interactively, this is filtered through the command\n\
line editing functions.\n\
@seealso{stdout, stderr}\n\
@end deftypefn")
{
  return const_value ("stdin", args, stdin_file);
}

DEFUNX ("stdout", Fstdout, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} stdout ()\n\
Return the numeric value corresponding to the standard output stream.\n\
Data written to the standard output is normally filtered through the pager.\n\
@seealso{stdin, stderr}\n\
@end deftypefn")
{
  return const_value ("stdout", args, stdout_file);
}

DEFUNX ("stderr", Fstderr, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} stderr ()\n\
Return the numeric value corresponding to the standard error stream.\n\
Even if paging is turned on, the standard error is not sent to the\n\
pager.  It is useful for error messages and prompts.\n\
@seealso{stdin, stdout}\n\
@end deftypefn")
{
  return const_value ("stderr", args, stderr_file);
}
