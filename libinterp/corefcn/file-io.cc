////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cerrno>
#include <cstdio>

#include <iomanip>
#include <string>

#if defined (HAVE_ZLIB_H)
#  include <zlib.h>
#endif

#include "file-ops.h"
#include "file-stat.h"
#include "iconv-wrappers.h"
#include "lo-ieee.h"
#include "lo-sysdep.h"
#include "localcharset-wrapper.h"
#include "mkostemp-wrapper.h"
#include "oct-env.h"
#include "oct-locbuf.h"
#include "unistd-wrappers.h"

#include "builtin-defun-decls.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "load-path.h"
#include "oct-fstrm.h"
#include "oct-iostrm.h"
#include "oct-map.h"
#include "oct-prcstrm.h"
#include "oct-stream.h"
#include "oct-strstrm.h"
#include "ov.h"
#include "ovl.h"
#include "pager.h"
#include "sysdep.h"
#include "utils.h"
#include "variables.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static void
normalize_fopen_mode (std::string& mode, bool& use_zlib)
{
  use_zlib = false;

  if (! mode.empty ())
    {
      // Matlab uses 'A' and 'W' to indicate that buffered writing should
      // take place.  Octave already does that.  Theoretically, we should
      // warn about using 'a', 'r', or 'w' because Octave does not enable
      // automatic flushing with these modes.  The performance hit is ~4X
      // when using automatic flushing and seems completely unnecessary.
      // See bug #52644.

      std::size_t pos = mode.find ('W');

      if (pos != std::string::npos)
        mode[pos] = 'w';

      pos = mode.find ('R');

      if (pos != std::string::npos)
        mode[pos] = 'r';

      pos = mode.find ('A');

      if (pos != std::string::npos)
        mode[pos] = 'a';

      pos = mode.find ('z');

      if (pos != std::string::npos)
        {
#if defined (HAVE_ZLIB)
          use_zlib = true;
          mode.erase (pos, 1);
#else
          err_disabled_feature ("", "gzipped files (zlib)");
#endif
        }

      // Use binary mode if 't' is not specified, but don't add
      // 'b' if it is already present.

      std::size_t bpos = mode.find ('b');
      std::size_t tpos = mode.find ('t');

      if (bpos == std::string::npos && tpos == std::string::npos)
        mode += 'b';
    }
}

static std::ios::openmode
fopen_mode_to_ios_mode (const std::string& mode)
{
  std::ios::openmode retval = std::ios::in;

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
    error ("invalid mode specified");

  return retval;
}

DEFMETHOD (fclose, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{status} =} fclose (@var{fid})
@deftypefnx {} {@var{status} =} fclose ("all")
Close the file specified by the file descriptor @var{fid}.

If successful, @code{fclose} returns 0, otherwise, it returns -1.  The
second form of the @code{fclose} call closes all open files except
@code{stdin}, @code{stdout}, @code{stderr}, and any FIDs associated
with gnuplot.
@seealso{fopen, fflush, freport}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  stream_list& streams = interp.get_stream_list ();

  return ovl (streams.remove (args(0), "fclose"));
}

DEFMETHOD (fclear, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} fclear (@var{fid})
Clear the stream state for the file specified by the file descriptor
@var{fid}.
@seealso{ferror, fopen}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  stream_list& streams = interp.get_stream_list ();

  int fid = streams.get_file_number (args(0));

  stream os = streams.lookup (fid, "fclear");

  os.clearerr ();

  return ovl ();
}

DEFMETHOD (fflush, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{status} =} fflush (@var{fid})
Flush output to file descriptor @var{fid}.

@code{fflush} returns 0 on success and an OS dependent error value
(@minus{}1 on Unix) on error.

Programming Note: Flushing is useful for ensuring that all pending output
makes it to the screen before some other event occurs.  For example, it is
always a good idea to flush the standard output stream before calling
@code{input}.
@seealso{fopen, fclose}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  octave_value retval = -1;

  stream_list& streams = interp.get_stream_list ();

  // FIXME: any way to avoid special case for stdout?
  int fid = streams.get_file_number (args(0));

  if (fid == 1)
    {
      flush_stdout ();

      retval = 0;
    }
  else
    {
      stream os = streams.lookup (fid, "fflush");

      retval = os.flush ();
    }

  return retval;
}

DEFMETHOD (fgetl, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{str} =} fgetl (@var{fid})
@deftypefnx {} {@var{str} =} fgetl (@var{fid}, @var{len})
Read characters from a file, stopping after a newline, or EOF,
or @var{len} characters have been read.

The characters read, excluding the possible trailing newline, are returned
as a string.

If @var{len} is omitted, @code{fgetl} reads until the next newline
character.

If there are no more characters to read, @code{fgetl} returns @minus{}1.

To read a line and return the terminating newline,
@pxref{XREFfgets,,@code{fgets}}.
@seealso{fgets, fscanf, fread, fopen}
@end deftypefn */)
{
  static const std::string who = "fgetl";

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  stream_list& streams = interp.get_stream_list ();

  stream os = streams.lookup (args(0), who);

  octave_value len_arg = (nargin == 2) ? args(1) : octave_value ();

  bool err = false;

  std::string tmp = os.getl (len_arg, err, who);

  if (! err)
    return ovl (tmp, tmp.length ());
  else
    return ovl (-1, 0);
}

DEFMETHOD (fgets, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{str} =} fgets (@var{fid})
@deftypefnx {} {@var{str} =} fgets (@var{fid}, @var{len})
Read characters from a file, stopping after a newline, or EOF,
or @var{len} characters have been read.

The characters read, including the possible trailing newline, are returned
as a string.

If @var{len} is omitted, @code{fgets} reads until the next newline
character.

If there are no more characters to read, @code{fgets} returns @minus{}1.

To read a line and discard the terminating newline,
@pxref{XREFfgetl,,@code{fgetl}}.
@seealso{fputs, fgetl, fscanf, fread, fopen}
@end deftypefn */)
{
  static const std::string who = "fgets";

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  stream_list& streams = interp.get_stream_list ();

  stream os = streams.lookup (args(0), who);

  octave_value len_arg = (nargin == 2) ? args(1) : octave_value ();

  bool err = false;

  std::string tmp = os.gets (len_arg, err, who);

  if (! err)
    return ovl (tmp, tmp.length ());
  else
    return ovl (-1.0, 0.0);
}

DEFMETHOD (fskipl, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{nlines} =} fskipl (@var{fid})
@deftypefnx {} {@var{nlines} =} fskipl (@var{fid}, @var{count})
@deftypefnx {} {@var{nlines} =} fskipl (@var{fid}, Inf)
Read and skip @var{count} lines from the file specified by the file
descriptor @var{fid}.

@code{fskipl} discards characters until an end-of-line is encountered
exactly @var{count}-times, or until the end-of-file marker is found.

If @var{count} is omitted, it defaults to 1.  @var{count} may also be
@code{Inf}, in which case lines are skipped until the end of the file.
This form is suitable for counting the number of lines in a file.

Returns the number of lines skipped (end-of-line sequences encountered).
@seealso{fgetl, fgets, fscanf, fopen}
@end deftypefn */)
{
  static const std::string who = "fskipl";

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  stream_list& streams = interp.get_stream_list ();

  stream os = streams.lookup (args(0), who);

  octave_value count_arg = (nargin == 2) ? args(1) : octave_value ();

  bool err = false;

  off_t tmp = os.skipl (count_arg, err, who);

  if (! err)
    return ovl (tmp);
  else
    return ovl ();
}

static stream
do_stream_open (const std::string& name, const std::string& mode_arg,
                const std::string& arch, std::string encoding, int& fid)
{
  stream retval;

  fid = -1;

  if (encoding.compare ("utf-8"))
    {
      // check if encoding is valid
      void *codec = octave_iconv_open_wrapper (encoding.c_str (), "utf-8");
      if (codec == reinterpret_cast<void *> (-1))
        {
          if (errno == EINVAL)
            error ("fopen: conversion from codepage '%s' not supported",
                   encoding.c_str ());
        }
      else
        octave_iconv_close_wrapper (codec);
    }

  std::string mode = mode_arg;
  bool use_zlib = false;
  normalize_fopen_mode (mode, use_zlib);

  std::ios::openmode md = fopen_mode_to_ios_mode (mode);

  mach_info::float_format flt_fmt
    = mach_info::string_to_float_format (arch);

  std::string fname = sys::file_ops::tilde_expand (name);

  sys::file_stat fs (fname);

  if (! (md & std::ios::out))
    fname = find_data_file_in_load_path ("fopen", fname);

  if (! fs.is_dir ())
    {
#if defined (HAVE_ZLIB)
      if (use_zlib)
        {
          FILE *fptr = sys::fopen (fname.c_str (), mode.c_str ());

          if (fptr)
            {
              int fd = fileno (fptr);

              gzFile gzf = ::gzdopen (fd, mode.c_str ());

              retval = zstdiostream::create (fname, gzf, fd, md, flt_fmt,
                                             encoding);
            }
          else
            retval.error (std::strerror (errno));
        }
      else
#endif
        {
          FILE *fptr = sys::fopen (fname, mode);

          retval = stdiostream::create (fname, fptr, md, flt_fmt, encoding);

          if (! fptr)
            retval.error (std::strerror (errno));
        }

    }

  return retval;
}

static stream
do_stream_open (const octave_value& tc_name, const octave_value& tc_mode,
                const octave_value& tc_arch, const octave_value& tc_encoding,
                const char *fcn, int& fid)
{
  stream retval;

  fid = -1;

  std::string name = tc_name.xstring_value ("%s: filename must be a string", fcn);
  std::string mode = tc_mode.xstring_value ("%s: file mode must be a string", fcn);
  std::string arch = tc_arch.xstring_value ("%s: architecture type must be a string", fcn);
  std::string encoding = tc_encoding.xstring_value ("%s: ENCODING must be a string", fcn);

  // Valid names for encodings consist of ASCII characters only.
  std::transform (encoding.begin (), encoding.end (), encoding.begin (),
                  ::tolower);

  if (encoding == "system")
    encoding = octave_locale_charset_wrapper ();

#if defined (OCTAVE_HAVE_STRICT_ENCODING_FACET)
  if (encoding != "utf-8")
    {
      warning_with_id ("Octave:fopen:encoding-unsupported",
                       "fopen: encoding must be 'UTF-8' for this version");
      encoding = "utf-8";
    }
#endif

  retval = do_stream_open (name, mode, arch, encoding, fid);

  return retval;
}

DEFMETHOD (fopen, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{fid} =} fopen (@var{name})
@deftypefnx {} {@var{fid} =} fopen (@var{name}, @var{mode})
@deftypefnx {} {@var{fid} =} fopen (@var{name}, @var{mode}, @var{arch})
@deftypefnx {} {@var{fid} =} fopen (@var{name}, @var{mode}, @var{arch}, @var{encoding})
@deftypefnx {} {[@var{fid}, @var{msg}] =} fopen (@dots{})
@deftypefnx {} {@var{fid_list} =} fopen ("all")
@deftypefnx {} {[@var{file}, @var{mode}, @var{arch}, @var{encoding}] =} fopen (@var{fid})
Open a file for low-level I/O or query open files and file descriptors.

The first form of the @code{fopen} function opens the named file with
the specified mode (read-write, read-only, etc.@:), architecture
interpretation (IEEE big endian, IEEE little endian, etc.@:) and file encoding,
and returns an integer value that may be used to refer to the file later.  If
an error occurs, @var{fid} is set to @minus{}1 and @var{msg} contains the
corresponding system error message.  The @var{mode} is a one or two
character string that specifies whether the file is to be opened for
reading, writing, or both.  The @var{encoding} is a character string with a
valid encoding identifier.  This encoding is used when strings are read from
or written to the file.  By default, that is UTF-8.

The second form of the @code{fopen} function returns a vector of file ids
corresponding to all the currently open files, excluding the
@code{stdin}, @code{stdout}, and @code{stderr} streams.

The third form of the @code{fopen} function returns information about the
open file given its file id.

For example,

@example
myfile = fopen ("splat.dat", "r", "ieee-le");
@end example

@noindent
opens the file @file{splat.dat} for reading.  If necessary, binary
numeric values will be read assuming they are stored in IEEE format with
the least significant bit first, and then converted to the native
representation.

Opening a file that is already open simply opens it again and returns a
separate file id.  It is not an error to open a file several times,
though writing to the same file through several different file ids may
produce unexpected results.

The possible values of @var{mode} are

@table @asis
@item @samp{r} (default)
Open a file for reading.

@item @samp{w}
Open a file for writing.  The previous contents are discarded.

@item @samp{a}
Open or create a file for writing at the end of the file.

@item @samp{r+}
Open an existing file for reading and writing.

@item @samp{w+}
Open a file for reading or writing.  The previous contents are
discarded.

@item @samp{a+}
Open or create a file for reading or writing at the end of the
file.
@end table

Append a @qcode{"t"} to the mode string to open the file in text mode or a
@qcode{"b"} to open in binary mode.  On Windows systems,
text mode reading and writing automatically converts linefeeds to the
appropriate line end character for the system (carriage-return linefeed on
Windows).  The default when no mode is specified is binary.

Additionally, you may append a @qcode{"z"} to the mode string to open a
gzipped file for reading or writing.  For this to be successful, you
must also open the file in binary mode.

The parameter @var{arch} is a string specifying the default data format
for the file.  Valid values for @var{arch} are:

@table @asis
@item @qcode{"native"} or @qcode{"n"} (default)
The format of the current machine.

@item @qcode{"ieee-be"} or @qcode{"b"}
IEEE big endian format.

@item @qcode{"ieee-le"} or @qcode{"l"}
IEEE little endian format.
@end table

When opening a new file that does not yet exist, permissions will be set to
@code{0666 - @var{umask}}.

Compatibility Note: Octave opens files using buffered I/O.  Small writes are
accumulated until an internal buffer is filled, and then everything is written
in a single operation.  This is very efficient and improves performance.
@sc{matlab}, however, opens files using flushed I/O where every write operation
is immediately performed.  If the write operation must be performed immediately
after data has been written then the write should be followed by a call to
@code{fflush} to flush the internal buffer.
@seealso{fclose, fgets, fgetl, fscanf, fread, fputs, fdisp, fprintf, fwrite,
fskipl, fseek, frewind, ftell, feof, ferror, fclear, fflush, freport, umask}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 4)
    print_usage ();

  octave_value_list retval = ovl (-1.0);

  stream_list& streams = interp.get_stream_list ();

  if (nargin == 1)
    {
      if (args(0).is_string ())
        {
          // If there is only one argument and it is a string but it
          // is not the string "all", we assume it is a file to open
          // with MODE = "r".  To open a file called "all", you have
          // to supply more than one argument.
          if (nargout < 2 && args(0).string_value () == "all")
            return streams.open_file_numbers ();
        }
      else
        {
          string_vector tmp = streams.get_info (args(0));

          retval = ovl (tmp(0), tmp(1), tmp(2), tmp(3));

          return retval;
        }
    }

  octave_value mode = (nargin > 1) ? args(1) : octave_value ("r");

  octave_value arch = (nargin > 2) ? args(2) : octave_value ("native");

  octave_value encoding = (nargin > 3) ? args(3) : octave_value ("utf-8");

  int fid = -1;

  stream os = do_stream_open (args(0), mode, arch, encoding, "fopen",
                              fid);

  if (os)
    retval = ovl (streams.insert (os), "");
  else
    {
      int error_number = 0;

      retval = ovl (-1.0, os.error (false, error_number));
    }

  return retval;
}

/*
## Further tests are in io.tst
%!test   # Uses hardcoded value of 1 for stdout
%! [name, mode, arch, encoding] = fopen (1);
%! assert (name, "stdout");
%! assert (mode, "w");
%! assert (encoding, "utf-8");

%!test   # Query of non-existent stream returns all ""
%! [name, mode, arch] = fopen (-1);
%! assert (name, "");
%! assert (mode, "");
%! assert (arch, "");

## FIXME: should be conditional on OCTAVE_HAVE_STRICT_ENCODING_FACET
%!testif HAVE_LLVM_LIBCXX
%! fname = tempname ();
%! unwind_protect
%!   fail ("fid = fopen (fname, 'wb', 'n', 'Windows-1252')", ...
%!         "warning", "encoding must be 'UTF-8'");
%!   [name, mode, arch, encoding] = fopen (fid);
%!   assert (name, fname);
%!   assert (mode, "wb");
%!   assert (encoding, "utf-8");  # fallback after warning
%! unwind_protect_cleanup
%!   fclose (fid);
%!   unlink (fname);
%! end_unwind_protect
*/

DEFMETHOD (freport, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} freport ()
Print a list of which files have been opened, and whether they are open
for reading, writing, or both.

For example:

@example
@group
freport ()

     @print{}  number  mode  arch       name
     @print{}  ------  ----  ----       ----
     @print{}     0     r    ieee-le    stdin
     @print{}     1     w    ieee-le    stdout
     @print{}     2     w    ieee-le    stderr
     @print{}     3     r    ieee-le    myfile
@end group
@end example
@seealso{fopen, fclose, is_valid_file_id}
@end deftypefn */)
{
  if (args.length () > 0)
    warning ("freport: ignoring extra arguments");

  stream_list& streams = interp.get_stream_list ();

  octave_stdout << streams.list_open_files ();

  return ovl ();
}

DEFMETHOD (frewind, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} frewind (@var{fid})
@deftypefnx {} {@var{status} =} frewind (@var{fid})
Move the file pointer to the beginning of the file specified by file
descriptor @var{fid}.

If an output @var{status} is requested then @code{frewind} returns 0 for
success, and -1 if an error is encountered.

Programming Note: @code{frewind} is equivalent to
@code{fseek (@var{fid}, 0, SEEK_SET)}.
@seealso{fseek, ftell, fopen}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  int result = -1;

  stream_list& streams = interp.get_stream_list ();

  stream os = streams.lookup (args(0), "frewind");

  result = os.rewind ();

  if (nargout > 0)
    return ovl (result);
  else
    return ovl ();
}

DEFMETHOD (fseek, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{status} =} fseek (@var{fid}, @var{offset})
@deftypefnx {} {@var{status} =} fseek (@var{fid}, @var{offset}, @var{origin})
Set the file pointer to the location @var{offset} within the file @var{fid}.

The pointer is positioned @var{offset} characters from the @var{origin}, which
may be one of the predefined variables @w{@qcode{SEEK_SET}} (beginning),
@w{@qcode{SEEK_CUR}} (current position), or @w{@qcode{SEEK_END}} (end of file)
or strings @nospell{@qcode{"bof"}}, @nospell{@qcode{"cof"}}, or
@nospell{@qcode{"eof"}}.  If @var{origin} is omitted, @w{@qcode{SEEK_SET}} is
assumed.  @var{offset} may be positive, negative, or zero but not all
combinations of @var{origin} and @var{offset} can be realized.

@code{fseek} returns 0 on success and -1 on error.
@seealso{fskipl, frewind, ftell, fopen, SEEK_SET, SEEK_CUR, SEEK_END}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2 || nargin > 3)
    print_usage ();

  stream_list& streams = interp.get_stream_list ();

  stream os = streams.lookup (args(0), "fseek");

  octave_value origin_arg = (nargin == 3) ? args(2) : octave_value (-1.0);

  return ovl (os.seek (args(1), origin_arg));
}

DEFMETHOD (ftell, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{pos} =} ftell (@var{fid})
Return the position of the file pointer as the number of characters from the
beginning of the file specified by file descriptor @var{fid}.
@seealso{fseek, frewind, feof, fopen}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  stream_list& streams = interp.get_stream_list ();

  stream os = streams.lookup (args(0), "ftell");

  return ovl (os.tell ());
}

static octave_value_list
printf_internal (interpreter& interp, const std::string& who,
                 const octave_value_list& args, int nargout)
{
  int nargin = args.length ();

  if (! (nargin > 1 || (nargin > 0 && args(0).is_string ())))
    print_usage ();

  int result;

  stream os;
  int fmt_n = 0;

  stream_list& streams = interp.get_stream_list ();

  if (args(0).is_string ())
    os = streams.lookup (1, who);
  else
    {
      fmt_n = 1;
      os = streams.lookup (args(0), who);
    }

  if (! args(fmt_n).is_string ())
    error ("%s: format TEMPLATE must be a string", who.c_str ());

  octave_value_list tmp_args;

  if (nargin > 1 + fmt_n)
    {
      tmp_args.resize (nargin-fmt_n-1, octave_value ());

      for (int i = fmt_n + 1; i < nargin; i++)
        tmp_args(i-fmt_n-1) = args(i);
    }

  result = os.printf (args(fmt_n), tmp_args, who);

  if (nargout > 0)
    return ovl (result);
  else
    return ovl ();
}

DEFMETHOD (fprintf, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} fprintf (@var{fid}, @var{template}, @dots{})
@deftypefnx {} {} fprintf (@var{template}, @dots{})
@deftypefnx {} {@var{numbytes} =} fprintf (@dots{})
This function is equivalent to @code{printf}, except that the output is
written to the file descriptor @var{fid} instead of @code{stdout}.

If @var{fid} is omitted, the output is written to @code{stdout} making the
function exactly equivalent to @code{printf}.

The optional output @var{numbytes} returns the number of bytes written to the
file.

Implementation Note: For compatibility with @sc{matlab}, escape sequences in
the template string (e.g., @qcode{"@backslashchar{}n"} => newline) are
expanded even when the template string is defined with single quotes.
@seealso{fputs, fdisp, fwrite, fscanf, printf, sprintf, fopen}
@end deftypefn */)
{
  static const std::string who = "fprintf";

  return printf_internal (interp, who, args, nargout);
}

DEFMETHOD (printf, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} printf (@var{template}, @dots{})
@deftypefnx {} {@var{numbytes} =} printf (@dots{})
Print optional arguments under the control of the template string
@var{template} to the stream @code{stdout} and return the number of characters
printed.
@ifclear OCTAVE_MANUAL

See the Formatted Output section of the GNU Octave manual for a complete
description of the syntax of the template string.
@end ifclear

The optional output @var{numbytes} returns the number of bytes printed.

Implementation Note: For compatibility with @sc{matlab}, escape sequences in
the template string (e.g., @qcode{"@backslashchar{}n"} => newline) are
expanded even when the template string is defined with single quotes.
@seealso{fprintf, sprintf, scanf}
@end deftypefn */)
{
  static const std::string who = "printf";

  octave_value_list tmp_args = args;

  return printf_internal (interp, who, tmp_args.prepend (octave_value (1)),
                          nargout);
}

static octave_value_list
puts_internal (interpreter& interp, const std::string& who,
               const octave_value_list& args)
{
  if (args.length () != 2)
    print_usage ();

  stream_list& streams = interp.get_stream_list ();

  stream os = streams.lookup (args(0), who);

  return ovl (- (os.puts (args(1), who) < 0));
}

DEFMETHOD (fputs, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{status} =} fputs (@var{fid}, @var{string})
Write the string @var{string} to the file with file descriptor @var{fid}.

The string is written to the file with no additional formatting.  Use
@code{fdisp} instead to automatically append a newline character appropriate
for the local machine.

The optional output @var{status} is 0 for success, or -1 if an error was
encountered.
@seealso{fdisp, fprintf, fwrite, fopen}
@end deftypefn */)
{
  static const std::string who = "fputs";

  return puts_internal (interp, who, args);
}

/*
## Check if text is correctly converted to output encoding
# FIXME: should be conditional on OCTAVE_HAVE_STRICT_ENCODING_FACET
%!testif ; ! __have_feature__ ("LLVM_LIBCXX")  <*61839>
%! str = "aäöu";  # string with non-ASCII characters
%! fname = tempname ();
%! fid = fopen (fname, "wt", "n", "ISO-8859-1");
%! unwind_protect
%!   fprintf (fid, '%s\n', str);
%!   fdisp (fid, str);
%!   fputs (fid, str);
%!   fclose (fid);
%!   ## re-open file for reading in binary mode
%!   fid = fopen (fname, "rb");
%!   fb = fread (fid);
%!   fclose (fid);
%!   ## check file content
%!   encoded = [97 228 246 117];  # original string in ISO-8859-1 encoding
%!   if (ispc ())
%!     eol = double ("\r\n");
%!   else
%!     eol = double ("\n");
%!   endif
%!   assert (fb.', [encoded eol encoded eol encoded])
%! unwind_protect_cleanup
%!   unlink (fname);
%! end_unwind_protect
*/

DEFMETHOD (puts, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{status} =} puts (@var{string})
Write a string to the standard output with no formatting.

The string is written verbatim to the standard output.  Use @code{disp} to
automatically append a newline character appropriate for the local machine.

The optional output @var{status} is 0 for success, or -1 if an error was
encountered.
@seealso{fputs, disp}
@end deftypefn */)
{
  static const std::string who = "puts";

  octave_value_list tmp_args = args;

  return puts_internal (interp, who, tmp_args.prepend (octave_value (1)));
}

DEFUN (sprintf, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{str} =} sprintf (@var{template}, @dots{})
This is like @code{printf}, except that the output is returned as a
string.

Unlike the C library function, which requires you to provide a suitably
sized string as an argument, Octave's @code{sprintf} function returns the
string, automatically sized to hold all of the items converted.

Implementation Note: For compatibility with @sc{matlab}, escape sequences in
the template string (e.g., @qcode{"@backslashchar{}n"} => newline) are
expanded even when the template string is defined with single quotes.
@seealso{printf, fprintf, sscanf}
@end deftypefn */)
{
  static const std::string who = "sprintf";

  int nargin = args.length ();

  if (nargin == 0)
    print_usage ();

  // We don't use ostrstream::create here because need direct
  // access to the OSTR object so that we can extract a string object
  // from it to return.
  ostrstream *ostr = new ostrstream ();

  // The stream destructor will delete OSTR for us.
  stream os (ostr);

  if (! os.is_valid ())
    error ("%s: unable to create output buffer", who.c_str ());

  octave_value fmt_arg = args(0);

  if (! fmt_arg.is_string ())
    error ("%s: format TEMPLATE must be a string", who.c_str ());

  octave_value_list retval (3);

  octave_value_list tmp_args;
  if (nargin > 1)
    {
      tmp_args.resize (nargin-1, octave_value ());

      for (int i = 1; i < nargin; i++)
        tmp_args(i-1) = args(i);
    }

  // NOTE: Call to os.error must precede next call to ostr which might reset it.
  retval(2) = os.printf (fmt_arg, tmp_args, who);
  retval(1) = os.error ();

  std::string result = ostr->str ();
  char type = (fmt_arg.is_sq_string () ? '\'' : '"');

  retval(0) = (result.empty () ? octave_value (charMatrix (1, 0), type)
               : octave_value (result, type));

  return retval;
}

static octave_value_list
scanf_internal (interpreter& interp, const std::string& who,
                const octave_value_list& args)
{
  int nargin = args.length ();

  if (nargin < 2 || nargin > 3)
    print_usage ();

  octave_value_list retval;

  stream_list& streams = interp.get_stream_list ();

  stream os = streams.lookup (args(0), who);

  if (! args(1).is_string ())
    error ("%s: format TEMPLATE must be a string", who.c_str ());

  if (nargin == 3 && args(2).is_string ())
    {
      retval = os.oscanf (args(1), who);
    }
  else
    {
      octave_idx_type count = 0;

      Array<double> size
        = (nargin == 3
           ? args(2).vector_value ()
           : Array<double> (dim_vector (1, 1), lo_ieee_inf_value ()));

      octave_value tmp = os.scanf (args(1), size, count, who);

      retval = ovl (tmp, count, os.error ());
    }

  return retval;
}

DEFMETHOD (fscanf, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {[@var{val}, @var{count}, @var{errmsg}] =} fscanf (@var{fid}, @var{template}, @var{size})
@deftypefnx {} {[@var{v1}, @var{v2}, @dots{}, @var{count}, @var{errmsg}] =} fscanf (@var{fid}, @var{template}, "C")
In the first form, read from @var{fid} according to @var{template},
returning the result in the matrix @var{val}.

The optional argument @var{size} specifies the amount of data to read
and may be one of

@table @code
@item Inf
Read as much as possible, returning a column vector.

@item @var{nr}
Read up to @var{nr} elements, returning a column vector.

@item [@var{nr}, Inf]
Read as much as possible, returning a matrix with @var{nr} rows.  If the
number of elements read is not an exact multiple of @var{nr}, the last
column is padded with zeros.

@item [@var{nr}, @var{nc}]
Read up to @code{@var{nr} * @var{nc}} elements, returning a matrix with
@var{nr} rows.  If the number of elements read is not an exact multiple
of @var{nr}, the last column is padded with zeros.
@end table

@noindent
If @var{size} is omitted, a value of @code{Inf} is assumed.

A string is returned if @var{template} specifies only character conversions.

The number of items successfully read is returned in @var{count}.

If an error occurs, @var{errmsg} contains a system-dependent error message.

In the second form, read from @var{fid} according to @var{template},
with each conversion specifier in @var{template} corresponding to a
single scalar return value.  This form is more ``C-like'', and also
compatible with previous versions of Octave.  The number of successful
conversions is returned in @var{count}
@ifclear OCTAVE_MANUAL

See the Formatted Input section of the GNU Octave manual for a
complete description of the syntax of the template string.
@end ifclear
@seealso{fgets, fgetl, fread, scanf, sscanf, fopen}
@end deftypefn */)
{
  static const std::string who = "fscanf";

  return scanf_internal (interp, who, args);
}

static std::string
get_scan_string_data (const octave_value& val, const std::string& who)
{
  std::string retval;

  if (! val.is_string ())
    error ("%s: argument STRING must be a string", who.c_str ());

  octave_value tmp = val.reshape (dim_vector (1, val.numel ()));

  retval = tmp.string_value ();

  return retval;
}

DEFUN (sscanf, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {[@var{val}, @var{count}, @var{errmsg}, @var{pos}] =} sscanf (@var{string}, @var{template}, @var{size})
@deftypefnx {} {[@var{v1}, @var{v2}, @dots{}, @var{count}, @var{errmsg}] =} sscanf (@var{string}, @var{template}, "C")
This is like @code{fscanf}, except that the characters are taken from the
string @var{string} instead of from a stream.

Reaching the end of the string is treated as an end-of-file condition.  In
addition to the values returned by @code{fscanf}, the index of the next
character to be read is returned in @var{pos}.
@seealso{fscanf, scanf, sprintf}
@end deftypefn */)
{
  static const std::string who = "sscanf";

  int nargin = args.length ();

  if (nargin < 2 || nargin > 3)
    print_usage ();

  octave_value_list retval;

  std::string data = get_scan_string_data (args(0), who);

  stream os = istrstream::create (data);

  if (! os.is_valid ())
    error ("%s: unable to create temporary input buffer", who.c_str ());

  if (! args(1).is_string ())
    error ("%s: format TEMPLATE must be a string", who.c_str ());

  if (nargin == 3 && args(2).is_string ())
    {
      retval = os.oscanf (args(1), who);
    }
  else
    {
      octave_idx_type count = 0;

      Array<double> size = (nargin == 3) ? args(2).vector_value ()
                           : Array<double> (dim_vector (1, 1),
                                            lo_ieee_inf_value ());

      octave_value tmp = os.scanf (args(1), size, count, who);

      // FIXME: is this the right thing to do?
      // Extract error message first, because getting
      // position will clear it.
      std::string errmsg = os.error ();

      retval = ovl (tmp, count, errmsg,
                    (os.eof () ? data.length () : os.tell ()) + 1);
    }

  return retval;
}

/*
%!test <*56396>
%! [val, count, errmsg, nextpos] = sscanf ('1234a6', '%2d', 3);
%! assert (val, [12; 34]);
%! assert (count, 2);
%! assert (errmsg, "sscanf: format failed to match");
%! assert (nextpos, 5);
*/

DEFMETHOD (scanf, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {[@var{val}, @var{count}, @var{errmsg}] =} scanf (@var{template}, @var{size})
@deftypefnx {} {[@var{v1}, @var{v2}, @dots{}, @var{count}, @var{errmsg}] =} scanf (@var{template}, "C")
This is equivalent to calling @code{fscanf} with @var{fid} = @code{stdin}.

It is currently not useful to call @code{scanf} in interactive programs.
@seealso{fscanf, sscanf, printf}
@end deftypefn */)
{
  static const std::string who = "scanf";

  octave_value_list tmp_args = args;

  return scanf_internal (interp, who, tmp_args.prepend (octave_value (0)));
}

static octave_value_list
textscan_internal (interpreter& interp, const std::string& who,
                   const octave_value_list& args)
{
  if (args.length () < 1)
    print_usage (who);

  stream os;

  if (args(0).is_string ())
    {
      std::string data = get_scan_string_data (args(0), who);

      os = istrstream::create (data);

      if (! os.is_valid ())
        error ("%s: unable to create temporary input buffer", who.c_str ());
    }
  else
    {
      stream_list& streams = interp.get_stream_list ();

      os = streams.lookup (args(0), who);
    }

  int nskip = 1;

  std::string fmt;

  if (args.length () == 1)
    {
      // omitted format = %f.  explicit "" = width from file
      fmt = "%f";
    }
  else if (args(1).is_string ())
    {
      fmt = args(1).string_value ();

      if (args(1).is_sq_string ())
        fmt = do_string_escapes (fmt);

      nskip++;
    }
  else
    error ("%s: FORMAT must be a string", who.c_str ());

  octave_idx_type ntimes = -1;

  if (args.length () > 2)
    {
      if (args(2).isnumeric ())
        {
          ntimes = args(2).idx_type_value ();

          if (ntimes < args(2).double_value ())
            error ("%s: REPEAT = %g is too large",
                   who.c_str (), args(2).double_value ());

          nskip++;
        }
    }

  octave_value_list options = args.splice (0, nskip);

  octave_idx_type count = 0;

  octave_value result = os.textscan (fmt, ntimes, options, who, count);

  std::string errmsg = os.error ();

  return ovl (result, count, errmsg);
}

DEFMETHOD (textscan, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{C} =} textscan (@var{fid}, @var{format})
@deftypefnx {} {@var{C} =} textscan (@var{fid}, @var{format}, @var{repeat})
@deftypefnx {} {@var{C} =} textscan (@var{fid}, @var{format}, @var{param}, @var{value}, @dots{})
@deftypefnx {} {@var{C} =} textscan (@var{fid}, @var{format}, @var{repeat}, @var{param}, @var{value}, @dots{})
@deftypefnx {} {@var{C} =} textscan (@var{str}, @dots{})
@deftypefnx {} {[@var{C}, @var{position}, @var{errmsg}] =} textscan (@dots{})
Read data from a text file or string.

The string @var{str} or file associated with @var{fid} is read from and
parsed according to @var{format}.  The function is an extension of
@code{strread} and @code{textread}.  Differences include: the ability to
read from either a file or a string, additional options, and additional
format specifiers.

The input is interpreted as a sequence of words, delimiters (such as
whitespace), and literals.  The characters that form delimiters and
whitespace are determined by the options.  The format consists of format
specifiers interspersed between literals.  In the format, whitespace forms
a delimiter between consecutive literals, but is otherwise ignored.

The output @var{C} is a cell array where the number of columns is determined
by the number of format specifiers.

The first word of the input is matched to the first specifier of the format
and placed in the first column of the output; the second is matched to the
second specifier and placed in the second column and so forth.  If there
are more words than specifiers then the process is repeated until all words
have been processed or the limit imposed by @var{repeat} has been met (see
below).

The string @var{format} describes how the words in @var{str} should be
parsed.  As in @var{fscanf}, any (non-whitespace) text in the format that is
not one of these specifiers is considered a literal.  If there is a literal
between two format specifiers then that same literal must appear in the
input stream between the matching words.

The following specifiers are valid:

@table @code
@item  %f
@itemx %f64
@itemx %n
The word is parsed as a number and converted to double.

@item  %f32
The word is parsed as a number and converted to single (float).

@item  %d
@itemx %d8
@itemx %d16
@itemx %d32
@itemx %d64
The word is parsed as a number and converted to int8, int16, int32, or
int64.  If no size is specified then int32 is used.

@item  %u
@itemx %u8
@itemx %u16
@itemx %u32
@itemx %u64
The word is parsed as a number and converted to uint8, uint16, uint32, or
uint64.  If no size is specified then uint32 is used.

@item %s
The word is parsed as a string ending at the last character before
whitespace, an end-of-line, or a delimiter specified in the options.

@item %q
The word is parsed as a "quoted string".
If the first character of the string is a double quote (") then the string
includes everything until a matching double quote---including whitespace,
delimiters, and end-of-line characters.  If a pair of consecutive double
quotes appears in the input, it is replaced in the output by a single
double quote.  For examples, the input "He said ""Hello""" would
return the value 'He said "Hello"'.

@item  %c
The next character of the input is read.
This includes delimiters, whitespace, and end-of-line characters.

@item  %[@dots{}]
@itemx %[^@dots{}]
In the first form, the word consists of the longest run consisting of only
characters between the brackets.  Ranges of characters can be specified by
a hyphen; for example, %[0-9a-zA-Z] matches all alphanumeric characters (if
the underlying character set is ASCII).  Since @sc{matlab} treats hyphens
literally, this expansion only applies to alphanumeric characters.  To
include '-' in the set, it should appear first or last in the brackets; to
include ']', it should be the first character.  If the first character is
'^' then the word consists of characters @strong{not} listed.

@item %N@dots{}
For %s, %c %d, %f, %n, %u, an optional width can be specified as %Ns, etc.
where N is an integer > 1.  For %c, this causes exactly N characters to be
read instead of a single character.  For the other specifiers, it is an
upper bound on the number of characters read; normal delimiters can cause
fewer characters to be read.  For complex numbers, this limit applies to
the real and imaginary components individually.  For %f and %n, format
specifiers like %N.Mf are allowed, where M is an upper bound on number of
characters after the decimal point to be considered; subsequent digits are
skipped.  For example, the specifier %8.2f would read 12.345e6 as 1.234e7.

@item %*@dots{}
The word specified by the remainder of the conversion specifier is skipped.

@item literals
In addition the format may contain literal character strings; these will be
skipped during reading.  If the input string does not match this literal,
the processing terminates.
@end table

Parsed words corresponding to the first specifier are returned in the first
output argument and likewise for the rest of the specifiers.

By default, if there is only one input argument, @var{format} is @t{"%f"}.
This means that numbers are read from the input into a single column vector.
If @var{format} is explicitly empty (@qcode{""}) then textscan will
return data in a number of columns matching the number of fields on the
first data line of the input.  Either of these is suitable only when the
input is exclusively numeric.

For example, the string

@smallexample
@group
@var{str} = "\
Bunny Bugs   5.5\n\
Duck Daffy  -7.5e-5\n\
Penguin Tux   6"
@end group
@end smallexample

@noindent
can be read using

@example
@var{a} = textscan (@var{str}, "%s %s %f");
@end example

The optional numeric argument @var{repeat} can be used for limiting the
number of items read:

@table @asis
@item -1
Read all of the string or file until the end (default).

@item N
Read until the first of two conditions occurs: 1) the format has been
processed N times, or 2) N lines of the input have been processed.  Zero
(0) is an acceptable value for @var{repeat}.  Currently, end-of-line
characters inside %q, %c, and %[@dots{}]$ conversions do not contribute to
the line count.  This is incompatible with @sc{matlab} and may change in
future.
@end table

The behavior of @code{textscan} can be changed via property/value pairs.
The following properties are recognized:

@table @asis
@item @qcode{"BufSize"}
This specifies the number of bytes to use for the internal buffer.
A modest speed improvement may be obtained by setting this to a large value
when reading a large file, especially if the input contains long strings.
The default is 4096, or a value dependent on @var{n} if that is specified.

@item @qcode{"CollectOutput"}
A value of 1 or true instructs @code{textscan} to concatenate consecutive
columns of the same class in the output cell array.  A value of 0 or false
(default) leaves output in distinct columns.

@item @qcode{"CommentStyle"}
Specify parts of the input which are considered comments and will be
skipped.  @var{value} is the comment style and can be either (1) A string
or 1x1 cell string, to skip everything to the right of it; (2) A cell array
of two strings, to skip everything between the first and second strings.
Comments are only parsed where whitespace is accepted and do not act as
delimiters.

@item @qcode{"Delimiter"}
If @var{value} is a string, any character in @var{value} will be used to
split the input into words.  If @var{value} is a cell array of strings,
any string in the array will be used to split the input into words.
(default value = any whitespace.)

@item @qcode{"EmptyValue"}
Value to return for empty numeric values in non-whitespace delimited data.
The default is NaN@.  When the data type does not support NaN (int32 for
example), then the default is zero.

@item @qcode{"EndOfLine"}
@var{value} can be either an empty or one character specifying the
end-of-line character, or the pair
@qcode{"@backslashchar{}r@backslashchar{}n"} (CRLF).
In the latter case, any of
@qcode{"@backslashchar{}r"}, @qcode{"@backslashchar{}n"} or
@qcode{"@backslashchar{}r@backslashchar{}n"} is counted as a (single)
newline.  If no value is given,
@qcode{"@backslashchar{}r@backslashchar{}n"} is used.
@c If set to "" (empty string) EOLs are ignored as delimiters and added
@c to whitespace.

@c When reading from a character string, optional input argument @var{n}
@c specifies the number of times @var{format} should be used (i.e., to limit
@c the amount of data read).
@c When reading from file, @var{n} specifies the number of data lines to read;
@c in this sense it differs slightly from the format repeat count in strread.

@item @qcode{"HeaderLines"}
The first @var{value} number of lines of @var{fid} are skipped.  Note that
this does not refer to the first non-comment lines, but the first lines of
any type.

@item @qcode{"MultipleDelimsAsOne"}
If @var{value} is nonzero, treat a series of consecutive delimiters,
without whitespace in between, as a single delimiter.  Consecutive
delimiter series need not be vertically aligned.  Without this option, a
single delimiter before the end of the line does not cause the line to be
considered to end with an empty value, but a single delimiter at the start
of a line causes the line to be considered to start with an empty value.

@item @qcode{"TreatAsEmpty"}
Treat single occurrences (surrounded by delimiters or whitespace) of the
string(s) in @var{value} as missing values.

@item @qcode{"ReturnOnError"}
If set to numerical 1 or true, return normally as soon as an error is
encountered, such as trying to read a string using @code{%f}.
If set to 0 or false, return an error and no data.

@item @qcode{"Whitespace"}
Any character in @var{value} will be interpreted as whitespace and trimmed;
The default value for whitespace is
@c Note: the next line specifically has a newline which generates a space
@c       in the output of qcode, but keeps the next line < 80 characters.
@qcode{"
@backslashchar{}b@backslashchar{}r@backslashchar{}n@backslashchar{}t"}
(note the space).  Unless whitespace is set to @qcode{""} (empty) AND at
least one @qcode{"%s"} format conversion specifier is supplied, a space is
always part of whitespace.

@end table

When the number of words in @var{str} or @var{fid} doesn't match an exact
multiple of the number of format conversion specifiers, @code{textscan}'s
behavior depends on whether the last character of the string or file is an
end-of-line as specified by the @code{EndOfLine} option:

@table @asis
@item last character = end-of-line
Data columns are padded with empty fields, NaN or 0 (for integer fields) so
that all columns have equal length

@item last character is not end-of-line
Data columns are not padded; @code{textscan} returns columns of unequal
length
@end table

The second output @var{position} provides the location, in characters
from the beginning of the file or string, where processing stopped.

@seealso{dlmread, fscanf, load, strread, textread}
@end deftypefn */)
{
  static const std::string who = "textscan";

  return textscan_internal (interp, who, args);
}

DEFMETHOD (__textscan__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{C} =} __textscan__ (@var{who}, @dots{})
Like @code{textscan} but accept additional argument @var{who} to use
as the name of the function when reporting errors.
@end deftypefn */)
{
  if (args.length () == 0)
    print_usage ();

  return textscan_internal (interp, args(0).string_value (),
                            args.splice (0, 1));
}

/*
%!test
%! str = "1,  2,  3,  4\n 5,  ,  ,  8\n 9, 10, 11, 12";
%! fmtstr = "%f %d %f %s";
%! c = textscan (str, fmtstr, 2, "delimiter", ",", "emptyvalue", -Inf);
%! assert (c{1}, [1;5]);
%! assert (c{3}, [3; -Inf]);
%! assert (iscellstr (c{4}));

%!test
%! b = [10:10:100];
%! b = [b; 8*b/5];
%! str = sprintf ("%g miles/hr = %g kilometers/hr\n", b);
%! fmt = "%f miles/hr = %f kilometers/hr";
%! c = textscan (str, fmt);
%! assert (c{1}, b(1,:)', 1e-5);
%! assert (c{2}, b(2,:)', 1e-5);

%!test
%! str = "13, -, NA, str1, -25\r\n// Middle line\r\n36, na, 05, str3, 6";
%! c = textscan (str, "%d %n %f %s %n", "delimiter", ",",
%!                    "treatAsEmpty", {"NA", "na", "-"}, "commentStyle", "//");
%! assert (c{1}, int32 ([13; 36]));
%! assert (c{2}, [NaN; NaN]);
%! assert (c{3}, [NaN; 5]);
%! assert (c{4}, {"str1"; "str3"});
%! assert (c{5}, [-25; 6]);

%!test
%! str = "Km:10 = hhhBjjj miles16hour\r\n";
%! str = [str "Km:15 = hhhJjjj miles241hour\r\n"];
%! str = [str "Km:2 = hhhRjjj miles3hour\r\n"];
%! str = [str "Km:25 = hhhZ\r\n"];
%! fmt = "Km:%d = hhh%1sjjj miles%dhour";
%! c = textscan (str, fmt, "delimiter", " ");
%! assert (c{1}', int32 ([10, 15, 2, 25]));
%! assert (c{2}', {'B' 'J' 'R' 'Z'});
%! assert (c{3}', int32 ([16, 241, 3, 0]));

## Test with default EndOfLine parameter
%!test
%! c = textscan ("L1\nL2", "%s");
%! assert (c{:}, {"L1"; "L2"});

## Test with EndofLine parameter set to "" (empty) - newline should be in word
%!test
%! c = textscan ("L1\nL2", "%s", "endofline", "");
%! assert (int8 ([c{:}{:}]), int8 ([76, 49, 10, 76, 50]));

##  Matlab fails this test.  A literal after a conversion is not a delimiter
%!#test
%! ## No delimiters at all besides EOL.  Skip fields, even empty fields
%! str = "Text1Text2Text\nTextText4Text\nText57Text";
%! c = textscan (str, "Text%*dText%dText");
%! assert (c{1}, int32 ([2; 4; 0]));

## CollectOutput test
%!test
%! b = [10:10:100];
%! b = [b; 8*b/5; 8*b*1000/5];
%! str = sprintf ("%g miles/hr = %g (%g) kilometers (meters)/hr\n", b);
%! fmt = "%f miles%s %s %f (%f) kilometers %*s";
%! c = textscan (str, fmt, "collectoutput", 1);
%! assert (size (c{3}), [10, 2]);
%! assert (size (c{2}), [10, 2]);

## CollectOutput test with uneven column length files
%!test
%! b = [10:10:100];
%! b = [b; 8*b/5; 8*b*1000/5];
%! str = sprintf ("%g miles/hr = %g (%g) kilometers (meters)/hr\n", b);
%! str = [str "110 miles/hr"];
%! fmt = "%f miles%s %s %f (%f) kilometers %*s";
%! c = textscan (str, fmt, "collectoutput", 1);
%! assert (size (c{1}), [11, 1]);
%! assert (size (c{3}), [11, 2]);
%! assert (size (c{2}), [11, 2]);
%! assert (c{3}(end), NaN);
%! assert (c{2}{11, 1}, "/hr");
%! assert (isempty (c{2}{11, 2}), true);

## Double quoted string
%!test
%! str = 'First    "the second called ""the middle""" third';
%! fmt = "%q";
%! c = textscan (str, fmt);
%! assert (c{1}, {"First"; 'the second called "the middle"'; "third"});

## Arbitrary character
%!test
%! c = textscan ("a first, \n second, third", "%s %c %11c", "delimiter", " ,");
%! assert (c{1}, {"a"; "ond"});
%! assert (c{2}, {"f"; "t"});
%! assert (c{3}, {"irst, \n sec"; "hird"});

## Field width and non-standard delimiters
%!test
%! str = "12;34;123456789;7";
%! c = textscan (str, "%4d %4d", "delimiter", ";", "collectOutput", 1);
%! assert (c, {[12, 34; 1234, 5678; 9, 7]});

## Field width and non-standard delimiters (2)
%!test
%! str = "12;34;123456789;7";
%! c = textscan (str, "%4f %f", "delimiter", ";", "collectOutput", 1);
%! assert (c, {[12, 34; 1234, 56789; 7, NaN]});

## FIXME: Not Matlab compatible.  Matlab prioritizes precision over field width
## so "12.234e+2", when read with "%10.2f %f", yields "12.23" and "4e+2".
## Ignore trailing delimiter, but use leading one
%!#test
%! str = "12.234e+2,34, \n12345.789-9876j,78\n,10|3";
%! c = textscan (str, "%10.2f %f", "delimiter", ",", "collectOutput", 1,
%!                    "expChars", "e|");
%! assert (c, {[1223, 34; 12345.79-9876j, 78; NaN, 10000]}, 1e-6);

## Multi-character delimiter
%!test
%! str = "99end2 space88gap 4564";
%! c = textscan (str, "%d %s", "delimiter", {"end", "gap", "space"});
%! assert (c{1}, int32 ([99; 88]));
%! assert (c{2}, {"2 "; "4564"});

## FIXME: Following two tests still fail (4/13/2016).
## Delimiters as part of literals, and following literals
%!#test
%! str = "12 R&D & 7";
%! c = textscan (str, "%f R&D %f", "delimiter", "&", "collectOutput", 1,
%!                    "EmptyValue", -99);
%! assert (c, {[12, -99; 7, -99]});

## Delimiters as part of literals, and before literals
%!#test
%! str = "12 & R&D 7";
%! c = textscan (str, "%f R&D %f", "delimiter", "&", "collectOutput", 1);
%! assert (c, {[12 7]});

## Check number of lines read, not number of passes through format string
%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid, "1\n2\n3\n4\n5\n6");
%! fseek (fid, 0, "bof");
%! c = textscan (fid, "%f %f", 2);
%! E = feof (fid);
%! fclose (fid);
%! unlink (f);
%! assert (c, {1, 2});
%! assert (! E);

## Check number of lines read, not number of passes through format string
%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid, "1\r\n2\r3\n4\r\n5\n6");
%! fseek (fid, 0, "bof");
%! c = textscan (fid, "%f %f", 4);
%! fclose (fid);
%! unlink (f);
%! assert (c, {[1;3], [2;4]});

## Check number of lines read, with multiple delimiters
%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid, "1-\r\n-2\r3-\n-4\r\n5\n6");
%! fseek (fid, 0, "bof");
%! c = textscan (fid, "%f %f", 4, "delimiter", "-", "multipleDelimsAsOne", 1);
%! fclose (fid);
%! unlink (f);
%! assert (c, {[1;3], [2;4]});

## Check ReturnOnError
%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! str = "1 2 3\n4 s 6";
%! fprintf (fid, str);
%! fseek (fid, 0, "bof");
%! c = textscan (fid, "%f %f %f", "ReturnOnError", 1);
%! fseek (fid, 0, "bof");
%! fclose (fid);
%! unlink (f);
%! u = textscan (str, "%f %f %f", "ReturnOnError", 1);
%! assert (c, {[1;4], [2], [3]});
%! assert (u, {[1;4], [2], [3]});

%! ## Check ReturnOnError (2)
%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! str = "1 2 3\n4 s 6\n";
%! fprintf (fid, str);
%! fseek (fid, 0, "bof");
%! c = textscan (fid, "%f %f %f", "ReturnOnError", 1);
%! fseek (fid, 0, "bof");
%! fclose (fid);
%! unlink (f);
%! u = textscan (str, "%f %f %f", "ReturnOnError", 1);
%! assert (c, {[1;4], 2, 3});
%! assert (u, {[1;4], 2, 3});

%!error <Read error in field 2 of row 2>
%! textscan ("1 2 3\n4 s 6", "%f %f %f", "ReturnOnError", 0);

## Check ReturnOnError (3)
%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid, "1 s 3\n4 5 6");
%! fseek (fid, 0, "bof");
%! c = textscan (fid, "", "ReturnOnError", 1);
%! fseek (fid, 0, "bof");
%! fclose (fid);
%! unlink (f);
%! assert (c, {1});

## Check ReturnOnError with empty fields
%!test
%! c = textscan ("1,,3\n4,5,6", "", "Delimiter", ",", "ReturnOnError", 1);
%! assert (c, {[1;4], [NaN;5], [3;6]});

## Check ReturnOnError with empty fields (2)
%!test
%! c = textscan ("1,,3\n4,5,6", "%f %f %f", "Delimiter", ",",
%!               "ReturnOnError", 1);
%! assert (c, {[1;4], [NaN;5], [3;6]});

## Check ReturnOnError in first column
%!test
%! c = textscan ("1 2 3\ns 5 6", "", "ReturnOnError", 1);
%! assert (c, {1, 2, 3});

## FIXME: This test fails (4/14/16)
## Test incomplete first data line
%!#test
%! R = textscan (['Empty1' char(10)], 'Empty%d %f');
%! assert (R{1}, int32 (1));
%! assert (isempty (R{2}), true);

%!test <*37023>
%! data = textscan ("   1. 1 \n 2 3\n", '%f %f');
%! assert (data{1}, [1; 2], 1e-15);
%! assert (data{2}, [1; 3], 1e-15);

## Whitespace test using delimiter ";"
%!test <*37333>
%! tc{1, 1} = "C:/code;";
%! tc{1, end+1} = "C:/code/meas;";
%! tc{1, end+1} = " C:/code/sim;";
%! tc{1, end+1} = "C:/code/utils;";
%! string = [tc{:}];
%! c = textscan (string, "%s", "delimiter", ";");
%! for k = 1:max (numel (c{1}), numel (tc))
%!   lh = c{1}{k};
%!   rh = tc{k};
%!   rh(rh == ";") = "";
%!   rh = strtrim (rh);
%!   assert (strcmp (lh, rh));
%! endfor

## Whitespace test, adding multipleDelimsAsOne true arg
%!test <*37333>
%! tc{1, 1} = "C:/code;";
%! tc{1, end+1} = " C:/code/meas;";
%! tc{1, end+1} = "C:/code/sim;;";
%! tc{1, end+1} = "C:/code/utils;";
%! string = [tc{:}];
%! c = textscan (string, "%s", "delimiter", ";", "multipleDelimsAsOne", 1);
%! for k = 1:max (numel (c{1}), numel (tc))
%!   lh = c{1}{k};
%!   rh = tc{k};
%!   rh(rh == ";") = "";
%!   rh = strtrim (rh);
%!   assert (strcmp (lh, rh));
%! endfor

## Whitespace test (bug #37333), adding multipleDelimsAsOne false arg
%!test <*37333>
%! tc{1, 1} = "C:/code;";
%! tc{1, end+1} = " C:/code/meas;";
%! tc{1, end+1} = "C:/code/sim;;";
%! tc{1, end+1} = "";
%! tc{1, end+1} = "C:/code/utils;";
%! string = [tc{:}];
%! c = textscan (string, "%s", "delimiter", ";", "multipleDelimsAsOne", 0);
%! for k = 1:max (numel (c{1}), numel (tc))
%!   lh = c{1}{k};
%!   rh = tc{k};
%!   rh(rh == ";") = "";
%!   rh = strtrim (rh);
%!   assert (strcmp (lh, rh));
%! endfor

## Whitespace test (bug #37333) whitespace "" arg
%!test <*37333>
%! tc{1, 1} = "C:/code;";
%! tc{1, end+1} = " C:/code/meas;";
%! tc{1, end+1} = "C:/code/sim;";
%! tc{1, end+1} = "C:/code/utils;";
%! string = [tc{:}];
%! c = textscan (string, "%s", "delimiter", ";", "whitespace", "");
%! for k = 1:max (numel (c{1}), numel (tc))
%!   lh = c{1}{k};
%!   rh = tc{k};
%!   rh(rh == ";") = "";
%!   assert (strcmp (lh, rh));
%! endfor

## Whitespace test (bug #37333), whitespace " " arg
%!test <*37333>
%! tc{1, 1} = "C:/code;";
%! tc{1, end+1} = " C:/code/meas;";
%! tc{1, end+1} = "C:/code/sim;";
%! tc{1, end+1} = "C:/code/utils;";
%! string = [tc{:}];
%! c = textscan (string, "%s", "delimiter", ";", "whitespace", " ");
%! for k = 1:max (numel (c{1}), numel (tc))
%!   lh = c{1}{k};
%!   rh = tc{k};
%!   rh(rh == ";") = "";
%!   rh = strtrim (rh);
%!   assert (strcmp (lh, rh));
%! endfor

## Tests reading with empty format, should return proper nr of columns
%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid, " 1 2 3 4\n5 6 7 8");
%! fseek (fid, 0, "bof");
%! C = textscan (fid, "");
%! E = feof (fid);
%! fclose (fid);
%! unlink (f);
%! assert (C{1}, [1 ; 5], 1e-6);
%! assert (C{2}, [2 ; 6], 1e-6);
%! assert (C{3}, [3 ; 7], 1e-6);
%! assert (C{4}, [4 ; 8], 1e-6);
%! assert (E);

## Test leaving the file at the correct position on exit
%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid, "1,2\n3,4\n");
%! fseek (fid, 0, "bof");
%! C = textscan (fid, "%s %f", 2, "Delimiter", ",");
%! E = ftell (fid);
%! fclose (fid);
%! unlink (f);
%! assert (E, 8);

## Tests reading with empty format; empty fields & incomplete lower row
%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid, " ,2,,4\n5,6");
%! fseek (fid, 0, "bof");
%! C = textscan (fid, "", "delimiter", ",", "EmptyValue", 999,
%!                    "CollectOutput" , 1);
%! fclose (fid);
%! unlink (f);
%! assert (C{1}, [999, 2, 999, 4; 5, 6, 999, 999], 1e-6);

## Error message tests

%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! msg1 = "textscan: 1 parameters given, but only 0 values";
%! try
%!   C = textscan (fid, "", "headerlines");
%! end_try_catch
%! assert (! feof (fid));
%! fclose (fid);
%! unlink (f);
%! assert (msg1, lasterr);

%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! msg1 = "textscan: HeaderLines must be numeric";
%! try
%!   C = textscan (fid, "", "headerlines", "hh");
%! end_try_catch
%! fclose (fid);
%! unlink (f);
%! assert (msg1, lasterr);

## Skip headerlines
%!test
%! C = textscan ("field 1  field2\n 1 2\n3 4", "", "headerlines", 1,
%!               "collectOutput", 1);
%! assert (C, {[1 2; 3 4]});

## Skip headerlines with non-default EOL
%!test
%! C = textscan ("field 1  field2\r 1 2\r3 4", "", "headerlines", 2,
%!               "collectOutput", 1, "EndOfLine", '\r');
%! assert (C, {[3 4]});

%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid,"some_string");
%! fseek (fid, 0, "bof");
%! msg1 = "textscan: EndOfLine must be at most one character or '\\r\\n'";
%! try
%!   C = textscan (fid, "%f", "EndOfLine", "\n\r");
%! end_try_catch
%! fclose (fid);
%! unlink (f);
%! assert (msg1, lasterr);

%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid,"some_string");
%! fseek (fid, 0, "bof");
%! msg1 = "textscan: EndOfLine must be at most one character or '\\r\\n'";
%! try
%!   C = textscan (fid, "%f", "EndOfLine", 33);
%! end_try_catch
%! fclose (fid);
%! unlink (f);
%! assert (msg1, lasterr);

%!assert <*41824> (textscan ("123", "", "whitespace", " "){:}, 123);

## just test supplied emptyvalue
%!assert <*42343> (textscan (",NaN", "", "delimiter", "," ,"emptyValue" ,Inf),
%!                {Inf, NaN})

## test padding with supplied emptyvalue
%!test <*42343>
%! c = textscan (",1,,4\nInf,  ,NaN\n", "", "delimiter", ",",
%!               "emptyvalue", -10);
%! assert (cell2mat (c), [-10, 1, -10, 4; Inf, -10, NaN, -10]);

%!test <*42528>
%! assert (textscan ("1i", ""){1},  0+1i);
%! C = textscan ("3, 2-4i, NaN\n -i, 1, 23.4+2.2i\n 1+1 1+1j", "",
%!               "delimiter", ",");
%! assert (cell2mat (C), [3+0i, 2-4i, NaN+0i; 0-i,  1+0i, 23.4+2.2i; 1 1 1+1i]);

%!test
%! ## TreatAsEmpty
%! C = textscan ("1,2,3,NN,5,6\n", "%d%d%d%f", "delimiter", ",",
%!               "TreatAsEmpty", "NN");
%! assert (C{3}(1), int32 (3));
%! assert (C{4}(1), NaN);

## MultipleDelimsAsOne
%!test
%! str = "11, 12, 13,, 15\n21,, 23, 24, 25\n,, 33, 34, 35\n";
%! C = textscan (str, "%f %f %f %f", "delimiter", ",",
%!                    "multipledelimsasone", 1, "endofline", "\n");
%! assert (C{1}', [11, 21, 33]);
%! assert (C{2}', [12, 23, 34]);
%! assert (C{3}', [13, 24, 35]);
%! assert (C{4}', [15, 25, NaN]);

## Single-quoted escape sequences
%!test
%! str = "11\t12\t13\r21\t22\t23";
%! c = textscan (str, "", "delimiter", '\t', "EndOfLine", '\r');
%! assert (c{1}', [11, 21]);
%! assert (c{2}', [12, 22]);
%! assert (c{3}', [13, 23]);

%!test <*44750>
%! c = textscan ("/home/foo/", "%s", "delimiter", "/",
%!               "MultipleDelimsAsOne", 1);
%! assert (c{1}, {"home"; "foo"});

## FIXME: Test still fails (4/13/2016).
## Allow cuddling %sliteral, but warn it is ambiguous
%!#test
%! C = textscan ("abcxyz51\nxyz83\n##xyz101", "%s xyz %d");
%! assert (C{1}([1 3]), {"abc"; "##"});
%! assert (isempty (C{1}{2}), true);
%! assert (C{2}, int32 ([51; 83; 101]));

## Literals are not delimiters.

## Test for false positives in check for non-supported format specifiers
%!test
%! c = textscan ("Total: 32.5 % (of cm values)",
%!               "Total: %f %% (of cm values)");
%! assert (c{1}, 32.5, 1e-5);

## Test various forms of string format specifiers
%!test <*45712>
%! str = "14 :1 z:2 z:3 z:5 z:11";
%! C = textscan (str, "%f %s %*s %3s %*3s %f", "delimiter", ":");
%! assert (C, {14, {"1 z"}, {"3 z"}, 11});

## Bit width, fixed width conversion specifiers
%!test
%! str2 = "123456789012345 ";
%! str2 = [str2 str2 str2 str2 str2 str2 str2 str2];
%! str2 = [str2 "123456789.01234 1234567890.1234 12345.678901234 12345.678901234"];
%! pttrn = "%3u8%*s %5u16%*s %10u32%*s %15u64 %3d8%*s %5d16%*s %10d32%*s %15d64 %9f32%*s %14f64%*s %10.2f32%*s %12.2f64%*s";
%! C = textscan (str2, pttrn, "delimiter", " ");
%! assert (C{1}, uint8 (123));
%! assert (C{2}, uint16 (12345));
%! assert (C{3}, uint32 (1234567890));
%! assert (C{4}, uint64 (123456789012345));
%! assert (C{5}, int8 (123));
%! assert (C{6}, int16 (12345));
%! assert (C{7}, int32 (1234567890));
%! assert (C{8}, int64 (123456789012345));
%! assert (C{9}, single (123456789), 1e-12);
%! assert (C{10}, double (1234567890.123), 1e-15);
%! assert (C{11}, single (12345.68), 1e-5);
%! assert (C{12}, double (12345.68), 1e-11);

## Bit width, fixed width conv. specifiers -- check the right amount is left
%!test
%! str2 = "123456789012345 ";
%! str2 = [str2 str2 "123456789.01234"];
%! pttrn = "%3u8 %5u16 %10u32 %3d8 %5d16 %10d32 %9f32 %9f";
%! C = textscan (str2, pttrn, "delimiter", " ");
%! assert (C{1}, uint8 (123));
%! assert (C{2}, uint16 (45678));
%! assert (C{3}, uint32 (9012345));
%! assert (C{4}, int8 (123));
%! assert (C{5}, int16 (45678));
%! assert (C{6}, int32 (9012345));
%! assert (C{7}, single (123456789), 1e-12);
%! assert (C{8}, double (0.01234), 1e-12);

%!test
%! C = textscan ("123.123", "%2f %3f %3f");
%! assert (C{1}, 12);
%! assert (C{2}, 3.1, 1e-11);
%! assert (C{3}, 23);

%!test
%! C = textscan ("123.123", "%3f %3f %3f");
%! assert (C{1}, 123);
%! assert (C{2}, 0.12, 1e-11);
%! assert (C{3}, 3);

%!test
%! C = textscan ("123.123", "%4f %3f");
%! assert (C{1}, 123);
%! assert (C{2}, 123);

## field width interrupts exponent.  (Matlab incorrectly gives [12, 2e12])
%!test
%! assert (textscan ("12e12",  "%4f"), {[120;  2]});
%! assert (textscan ("12e+12", "%5f"), {[120;  2]});
%! assert (textscan ("125e-12","%6f"), {[12.5; 2]});

## %[] tests
## Plain [..] and *[..]
%!test
%! ar = "abcdefguvwxAny\nacegxyzTrailing\nJunk";
%! C = textscan (ar, "%[abcdefg] %*[uvwxyz] %s");
%! assert (C{1}, {"abcdefg"; "aceg"; ""});
%! assert (C{2}, {"Any"; "Trailing"; "Junk"});

%!test
%! assert (textscan ("A2 B2 C3", "%*[ABC]%d", 3), {int32([2; 2; 3])});

## [^..] and *[^..]
%!test
%! br = "abcdefguvwx1Any\nacegxyz2Trailing\n3Junk";
%! C = textscan (br, "%[abcdefg] %*[^0123456789] %s");
%! assert (C{1}, {"abcdefg"; "aceg"; ""});
%! assert (C{2}, {"1Any"; "2Trailing"; "3Junk"});

## [..] and [^..] containing delimiters
%!test
%! cr = "ab cd efguv wx1Any\na ce gx yz2Trailing\n   3Junk";
%! C = textscan (cr, "%[ abcdefg] %*[^0123456789] %s", "delimiter", " \n",
%!                   "whitespace", "");
%! assert (C{1}, {"ab cd efg"; "a ce g"; "   "});
%! assert (C{2}, {"1Any"; "2Trailing"; "3Junk"});

%!assert <*36464> (textscan ("1 2 3 4 5 6", "%*n%n%*[^\n]"){1}, 2);

## test %[]] and %[^]]
%!test
%! assert (textscan ("345]", "%*[123456]%[]]"){1}{1}, "]");
%! assert (textscan ("345]", "%*[^]]%s"){1}{1}, "]");

## Test that "-i" checks the next two characters
%!test
%! C = textscan ("-i -in -inf -infinity", "%f %f%s %f %f %s");
%! assert (C, {-i, -i, {"n"}, -Inf, -Inf, {"inity"}});

## Again for "+i", this time with custom parser
%!test
%! C = textscan ("+i +in +inf +infinity", "%f %f%s %f %f %s", "ExpChars", "eE");
%! assert (C, {i, i, {"n"}, Inf, Inf, {"inity"}});

## Check single quoted format interprets control sequences
%!test
%! C = textscan ("1 2\t3 4", '%f %[^\t] %f %f');
%! assert (C, {1, {"2"}, 3, 4});

## Check a non-empty line with no valid conversion registers empytValue
%!test
%! C = textscan ("Empty\n", "Empty%f %f");
%! assert (C, { NaN, NaN });

## Check overflow and underflow of integer types
%!test
%! a = "-1e90 ";
%! b = "1e90 ";
%! fmt = "%d8 %d16 %d32 %d64 %u8 %u16 %u32 %u64 ";
%! C = textscan ([a a a a a a a a b b b b b b b b], fmt);
%! assert (C{1}, int8 ([-128; 127]));
%! assert (C{2}, int16 ([-32768; 32767]));
%! assert (C{3}, int32 ([-2147483648; 2147483647]));
%! assert (C{4}, int64 ([-9223372036854775808; 9223372036854775807]));
%! assert (C{5}, uint8 ([0; 255]));
%! assert (C{6}, uint16 ([0; 65535]));
%! assert (C{7}, uint32 ([0; 4294967295]));
%! assert (C{8}, uint64 ([0; 18446744073709551615]));

## Tests from Matlab (does The MathWorks have any copyright over the input?)
%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid,"09/12/2005 Level1 12.34 45 1.23e10 inf Nan Yes 5.1+3i\n");
%! fprintf (fid,"10/12/2005 Level2 23.54 60 9e19 -inf  0.001 No 2.2-.5i\n");
%! fprintf (fid,"11/12/2005 Level3 34.90 12 2e5   10  100   No 3.1+.1i\n");
%! fseek (fid, 0, "bof");
%! C = textscan (fid,"%s %s %f32 %d8 %u %f %f %s %f");
%! %assert (C{1}, {"09/12/2005";"10/12/2005";"11/12/2005"});
%! assert (C{2}, {"Level1";"Level2";"Level3"});
%! assert (C{3}, [single(12.34);single(23.54);single(34.90)]);
%! assert (C{4}, [int8(45);int8(60);int8(12)]);
%! assert (C{5}, [uint32(4294967295);uint32(4294967295);uint32(200000)]);
%! assert (C{6}, [inf;-inf;10]);
%! assert (C{7}, [NaN;0.001;100], eps);
%! assert (C{8}, {"Yes";"No";"No"});
%! assert (C{9}, [5.1+3i;2.2-0.5i;3.1+0.1i]);
%! fseek (fid, 0, "bof");
%! C = textscan (fid,"%s Level%d %f32 %d8 %u %f %f %s %f");
%! assert (C{2}, [int32(1);int32(2);int32(3)]);
%! assert (C{3}, [single(12.34);single(23.54);single(34.90)]);
%! fseek (fid, 0, "bof");
%! C = textscan (fid, '%s %*[^\n]');
%! fclose (fid);
%! unlink (f);
%! assert (C, {{"09/12/2005";"10/12/2005";"11/12/2005"}});

%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid,"1,  2,  3,  4,   ,  6\n");
%! fprintf (fid,"7,  8,  9,   , 11, 12\n");
%! fseek (fid, 0, "bof");
%! C = textscan (fid,"%f %f %f %f %u8 %f", "Delimiter",",","EmptyValue",-Inf);
%! fclose (fid);
%! unlink (f);
%! assert (C{4}, [4; -Inf]);
%! assert (C{5}, uint8 ([0; 11]));

%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! fprintf (fid,"abc, 2, NA, 3, 4\n");
%! fprintf (fid,"// Comment Here\n");
%! fprintf (fid,"def, na, 5, 6, 7\n");
%! fseek (fid, 0, "bof");
%! C = textscan (fid, "%s %n %n %n %n", "Delimiter", ",",
%!                    "TreatAsEmpty", {"NA","na"}, "CommentStyle", "//");
%! fclose (fid);
%! unlink (f);
%! assert (C{1}, {"abc";"def"});
%! assert (C{2}, [2; NaN]);
%! assert (C{3}, [NaN; 5]);
%! assert (C{4}, [3; 6]);
%! assert (C{5}, [4; 7]);

## FIXME: Almost passes.  Second return value is {"/"}.  Tested 4/14/16.
## Test start of comment as string
%!#test
%! c = textscan ("1 / 2 // 3", "%n %s %u8", "CommentStyle", {"//"});
%! assert (c(1), {1, "/", 2});

%!assert (textscan (["1 2 3 4"; "5 6 7 8"], "%f"), {[15; 26; 37; 48]})

## Check for delimiter after exponent
%!assert (textscan ("1e-3|42", "%f", "delimiter", "|"), {[1e-3; 42]})

%!test <*52479>
%! str = "\t\ta\tb\tc\n";
%! ret = textscan (str, "%s", "delimiter", "\t");
%! assert (ret, { {''; ''; 'a'; 'b'; 'c'} });

%!test <*52479>
%! str = "\t\ta\tb\tc\n";
%! ret = textscan (str, "%s", "delimiter", {"\t"});
%! assert (ret, { {''; ''; 'a'; 'b'; 'c'} });

%!test <*52550>
%! str = ",,1,2,3\n";
%! obs = textscan (str, "%d", "delimiter", ",");
%! assert (obs, { [0; 0; 1; 2; 3] });
%! obs = textscan (str, "%d", "delimiter", {","});
%! assert (obs, { [0; 0; 1; 2; 3] });

%!test <*52550>
%! str = " , ,1,2,3\n";
%! obs = textscan (str, "%d", "delimiter", ",");
%! assert (obs, { [0; 0; 1; 2; 3] });
%! textscan (str, "%d", "delimiter", {","});
%! assert (obs, { [0; 0; 1; 2; 3] });

%!test <*52550>
%! str = " 0 , 5+6j , -INF+INFj ,NaN,3\n";
%! obs = textscan (str, "%f", "delimiter", ",");
%! assert (obs, { [0; 5+6i; complex(-Inf,Inf); NaN; 3] });
%! obs = textscan (str, "%f", "delimiter", {","});
%! assert (obs, { [0; 5+6i; complex(-Inf,Inf); NaN; 3] });

%!test <*52550>
%! str = " 0;,;,1;,2;,3\n";
%! assert (textscan (str, "%f", "delimiter", {";,"}), { [0; NaN; 1; 2; 3] });

%!test <*52550>
%! str = " 0 ;1 , $ 2 ;3\n";
%! obs = textscan (str, "%f", "delimiter", ",;$");
%! assert (obs, { [0; 1; NaN; 2; 3] });
%! obs = textscan (str, "%f", "delimiter", {",",";","$"});
%! assert (obs, { [0; 1; NaN; 2; 3] });

## file stream with encoding
## FIXME: should be conditional on OCTAVE_HAVE_STRICT_ENCODING_FACET
%!testif ; ! __have_feature__ ("LLVM_LIBCXX")
%! f = tempname ();
%! fid = fopen (f, "wt+", "n", "iso-8859-1");
%! unwind_protect
%!   fprintf (fid, "abc,äöü\n");
%!   fflush (fid);
%!   fseek (fid, 0, "bof");
%!   obs = textscan (fid, "%s", "delimiter", ",");
%!   fclose (fid);
%!   assert (obs, { {"abc"; "äöü"} });
%! unwind_protect_cleanup
%!   unlink (f);
%! end_unwind_protect

%!test <*56917>
%! str = '"a,b","c"';
%! obs = textscan (str, "%q", "delimiter", ",");
%! assert (obs, { { "a,b"; "c" } });

%!test <*58008>
%! txt = sprintf ('literal_other_1_1;literal_other_1_2\nliteral_other_2_1;literal_other_2_2\nliteral_other_3_1;literal_other_3_2');
%! nm1 = textscan (txt, 'literal%s literal%s', 'Delimiter', ';');
%! assert (nm1{1}, {"_other_1_1" ; "_other_2_1" ; "_other_3_1"});
%! assert (nm1{2}, {"_other_1_2" ; "_other_2_2" ; "_other_3_2"});
%! nm2 = textscan (txt, 'literal%s;literal%s', 'Delimiter', ';');
%! assert (nm1, nm2);

%!test <*57612>
%! str = sprintf (['101,' '\n' '201,']);
%! C = textscan (str, '%s%q', 'Delimiter', ',');
%! assert (size (C), [1, 2]);
%! assert (C{1}, { "101"; "201" });
%! assert (C{2}, { ""; "" });

%!test <*57612>
%! str = sprintf (['101,' '\n' '201,']);
%! C = textscan (str, '%s%f', 'Delimiter', ',');
%! assert (size (C), [1, 2]);
%! assert (C{1}, { "101"; "201" });
%! assert (C{2}, [ NaN; NaN ]);

%!test <*57612>
%! str = sprintf (['101,' '\n' '201,']);
%! C = textscan (str, '%s%d', 'Delimiter', ',');
%! assert (size (C), [1, 2]);
%! assert (C{1}, { "101"; "201" });
%! assert (C{2}, int32 ([ 0; 0 ]));

%!test <*51093>
%! str = sprintf ('a\t\tb\tc');
%! C = textscan (str, '%s', 'Delimiter', '\t', 'MultipleDelimsAsOne', false);
%! assert (C{1}, {'a'; ''; 'b'; 'c'});

%!test <50743>
%! C = textscan ('5973459727478852968', '%u64');
%! assert (C{1}, uint64 (5973459727478852968));

%!assert <*60711> (textscan('1,.,2', '%f', 'Delimiter', ','), {1});

*/

// These tests have end-comment sequences, so can't just be in a comment
#if 0
## Test unfinished comment
%!test
%! c = textscan ("1 2 /* half comment", "%n %u8", "CommentStyle", {"/*", "*/"});
%! assert (c, {1, 2});

## Test reading from a real file
%!test
%! f = tempname ();
%! fid = fopen (f, "w+");
%! d = rand (1, 4);
%! fprintf (fid, "  %f %f /* comment */  %f  %f ", d);
%! fseek (fid, 0, "bof");
%! A = textscan (fid, "%f %f", "CommentStyle", {"/*", "*/"});
%! E = feof (fid);
%! fclose (fid);
%! unlink (f);
%! assert (A{1}, [d(1); d(3)], 1e-6);
%! assert (A{2}, [d(2); d(4)], 1e-6);
%! assert (E);
#endif

/*
## Test input validation
%!error textscan ()
%!error <file id must be> textscan (single (4))
%!error <file id must be> textscan ({4})
%!error <must be a string> textscan ("Hello World", 2)
%!error <at most one character or>
%! textscan ("Hello World", "%s", "EndOfLine", 3);
%!error <'%z' is not a valid format specifier> textscan ("1.0", "%z")
%!error <no valid format conversion specifiers> textscan ("1.0", "foo")
*/

static octave_value
do_fread (stream& os, const octave_value& size_arg,
          const octave_value& prec_arg, const octave_value& skip_arg,
          const octave_value& arch_arg, octave_idx_type& count)
{
  count = -1;

  Array<double> size = size_arg.xvector_value ("fread: invalid SIZE specified");

  std::string prec = prec_arg.xstring_value ("fread: PRECISION must be a string");

  int block_size = 1;
  oct_data_conv::data_type input_type;
  oct_data_conv::data_type output_type;

  try
    {
      oct_data_conv::string_to_data_type (prec, block_size,
                                          input_type, output_type);
    }
  catch (execution_exception& ee)
    {
      error (ee, "fread: invalid PRECISION specified");
    }

  int skip = 0;

  try
    {
      skip = skip_arg.int_value (true);
    }
  catch (execution_exception& ee)
    {
      error (ee, "fread: SKIP must be an integer");
    }

  std::string arch = arch_arg.xstring_value ("fread: ARCH architecture type must be a string");

  mach_info::float_format flt_fmt
    = mach_info::string_to_float_format (arch);

  return os.read (size, block_size, input_type, output_type, skip,
                  flt_fmt, count);
}

DEFMETHOD (fread, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} fread (@var{fid})
@deftypefnx {} {@var{val} =} fread (@var{fid}, @var{size})
@deftypefnx {} {@var{val} =} fread (@var{fid}, @var{size}, @var{precision})
@deftypefnx {} {@var{val} =} fread (@var{fid}, @var{size}, @var{precision}, @var{skip})
@deftypefnx {} {@var{val} =} fread (@var{fid}, @var{size}, @var{precision}, @var{skip}, @var{arch})
@deftypefnx {} {[@var{val}, @var{count}] =} fread (@dots{})
Read binary data from the file specified by the file descriptor @var{fid}.

The optional argument @var{size} specifies the amount of data to read
and may be one of

@table @code
@item Inf
Read as much as possible, returning a column vector.

@item @var{nr}
Read up to @var{nr} elements, returning a column vector.

@item [@var{nr}, Inf]
Read as much as possible, returning a matrix with @var{nr} rows.  If the
number of elements read is not an exact multiple of @var{nr}, the last
column is padded with zeros.

@item [@var{nr}, @var{nc}]
Read up to @code{@var{nr} * @var{nc}} elements, returning a matrix with
@var{nr} rows.  If the number of elements read is not an exact multiple
of @var{nr}, the last column is padded with zeros.
@end table

@noindent
If @var{size} is omitted, a value of @code{Inf} is assumed.

The optional argument @var{precision} is a string specifying the type of
data to read and may be one of

@table @asis
@item @qcode{"uint8"} (default)
8-bit unsigned integer.

@item  @qcode{"int8"}
@itemx @qcode{"integer*1"}
8-bit signed integer.

@item  @qcode{"uint16"}
@itemx @qcode{"ushort"}
@itemx @qcode{"unsigned short"}
16-bit unsigned integer.

@item  @qcode{"int16"}
@itemx @qcode{"integer*2"}
@itemx @qcode{"short"}
16-bit signed integer.

@item  @qcode{"uint"}
@itemx @qcode{"uint32"}
@itemx @qcode{"unsigned int"}
@itemx @qcode{"ulong"}
@itemx @qcode{"unsigned long"}
32-bit unsigned integer.

@item  @qcode{"int"}
@itemx @qcode{"int32"}
@itemx @qcode{"integer*4"}
@itemx @qcode{"long"}
32-bit signed integer.

@item @qcode{"uint64"}
64-bit unsigned integer.

@item  @qcode{"int64"}
@itemx @qcode{"integer*8"}
64-bit signed integer.

@item  @qcode{"single"}
@itemx @qcode{"float"}
@itemx @qcode{"float32"}
@itemx @qcode{"real*4"}
32-bit floating point number.

@item  @qcode{"double"}
@itemx @qcode{"float64"}
@itemx @qcode{"real*8"}
64-bit floating point number.

@item  @qcode{"char"}
@itemx @qcode{"char*1"}
8-bit single character.

@item  @qcode{"uchar"}
@itemx @qcode{"unsigned char"}
8-bit unsigned character.

@item  @qcode{"schar"}
@itemx @qcode{"signed char"}
8-bit signed character.

@end table

@noindent
The default precision is @qcode{"uint8"}.

The @var{precision} argument may also specify an optional repeat
count.  For example, @samp{32*single} causes @code{fread} to read
a block of 32 single precision floating point numbers.  Reading in
blocks is useful in combination with the @var{skip} argument.

The @var{precision} argument may also specify a type conversion.
For example, @samp{int16=>int32} causes @code{fread} to read 16-bit
integer values and return an array of 32-bit integer values.  By
default, @code{fread} returns a double precision array.  The special
form @samp{*TYPE} is shorthand for @samp{TYPE=>TYPE}.

The conversion and repeat counts may be combined.  For example, the
specification @samp{32*single=>single} causes @code{fread} to read
blocks of single precision floating point values and return an array
of single precision values instead of the default array of double
precision values.

The optional argument @var{skip} specifies the number of bytes to skip
after each element (or block of elements) is read.  If it is not
specified, a value of 0 is assumed.  If the final block read is not
complete, the final skip is omitted.  For example,

@example
fread (f, 10, "3*single=>single", 8)
@end example

@noindent
will omit the final 8-byte skip because the last read will not be
a complete block of 3 values.

The optional argument @var{arch} is a string specifying the data format
for the file.  Valid values are

@table @asis
@item @qcode{"native"} or @qcode{"n"}
The format of the current machine.

@item @qcode{"ieee-be"} or @qcode{"b"}
IEEE big endian.

@item @qcode{"ieee-le"} or @qcode{"l"}
IEEE little endian.
@end table

If no @var{arch} is given the value used in the call to @code{fopen} which
created the file descriptor is used.  Otherwise, the value specified with
@code{fread} overrides that of @code{fopen} and determines the data format.

The output argument @var{val} contains the data read from the file.

The optional return value @var{count} contains the number of elements read.
@seealso{fwrite, fgets, fgetl, fscanf, fopen}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 5)
    print_usage ();

  stream_list& streams = interp.get_stream_list ();

  stream os = streams.lookup (args(0), "fread");

  octave_value size = lo_ieee_inf_value ();
  octave_value prec = "uint8";
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

  return ovl (tmp, count);
}

static int
do_fwrite (stream& os, const octave_value& data,
           const octave_value& prec_arg, const octave_value& skip_arg,
           const octave_value& arch_arg)
{
  std::string prec = prec_arg.xstring_value ("fwrite: PRECISION must be a string");

  int block_size = 1;
  oct_data_conv::data_type output_type;

  try
    {
      oct_data_conv::string_to_data_type (prec, block_size, output_type);
    }
  catch (execution_exception& ee)
    {
      error (ee, "fwrite: invalid PRECISION specified");
    }

  int skip = 0;

  try
    {
      skip = skip_arg.int_value (true);
    }
  catch (execution_exception& ee)
    {
      error (ee, "fwrite: SKIP must be an integer");
    }

  std::string arch = arch_arg.xstring_value ("fwrite: ARCH architecture type must be a string");

  mach_info::float_format flt_fmt
    = mach_info::string_to_float_format (arch);

  return os.write (data, block_size, output_type, skip, flt_fmt);
}

DEFMETHOD (fwrite, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{count} =} fwrite (@var{fid}, @var{data})
@deftypefnx {} {@var{count} =} fwrite (@var{fid}, @var{data}, @var{precision})
@deftypefnx {} {@var{count} =} fwrite (@var{fid}, @var{data}, @var{precision}, @var{skip})
@deftypefnx {} {@var{count} =} fwrite (@var{fid}, @var{data}, @var{precision}, @var{skip}, @var{arch})
Write data in binary form to the file specified by the file descriptor
@var{fid}.

The argument @var{data} is a matrix of values that are to be written to the
file.  The values are extracted in column-major order.

The remaining arguments @var{precision}, @var{skip}, and @var{arch} are
optional, and are interpreted as described for @code{fread}.

The output @var{count} is the number of data items successfully written.

Programming Note: The behavior of @code{fwrite} is undefined if the values in
@var{data} are too large to fit in the specified precision.
@seealso{fread, fputs, fprintf, fopen}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2 || nargin > 5)
    print_usage ();

  stream_list& streams = interp.get_stream_list ();

  stream os = streams.lookup (args(0), "fwrite");

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

  return ovl (do_fwrite (os, data, prec, skip, arch));
}

DEFMETHODX ("feof", Ffeof, interp, args, ,
            doc: /* -*- texinfo -*-
@deftypefn {} {@var{status} =} feof (@var{fid})
Return 1 if an end-of-file condition has been encountered for the file
specified by file descriptor @var{fid} and 0 otherwise.

Note that @code{feof} will only return 1 if the end of the file has already
been encountered, not if the next read operation will result in an
end-of-file condition.
@seealso{fread, frewind, fseek, fclear, fopen}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  stream_list& streams = interp.get_stream_list ();

  stream os = streams.lookup (args(0), "feof");

  return ovl (os.eof () ? 1.0 : 0.0);
}

DEFMETHODX ("ferror", Fferror, interp, args, ,
            doc: /* -*- texinfo -*-
@deftypefn  {} {@var{msg} =} ferror (@var{fid})
@deftypefnx {} {[@var{msg}, @var{err}] =} ferror (@var{fid})
@deftypefnx {} {[@dots{}] =} ferror (@var{fid}, "clear")
Query the error status of the stream specified by file descriptor @var{fid}.

If an error condition exists then return a string @var{msg} describing the
error.  Otherwise, return an empty string @qcode{""}.

The second input @qcode{"clear"} is optional.  If supplied, the error
state on the stream will be cleared.

The optional second output is a numeric indication of the error status.
@var{err} is 1 if an error condition has been encountered and 0 otherwise.

Note that @code{ferror} indicates if an error has already occurred, not
whether the next operation will result in an error condition.
@seealso{fclear, fopen}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  stream_list& streams = interp.get_stream_list ();

  stream os = streams.lookup (args(0), "ferror");

  bool clear = false;

  if (nargin == 2)
    {
      std::string opt = args(1).string_value ();

      clear = (opt == "clear");
    }

  int error_number = 0;

  std::string error_message = os.error (clear, error_number);

  return ovl (error_message, error_number);
}

DEFMETHODX ("popen", Fpopen, interp, args, ,
            doc: /* -*- texinfo -*-
@deftypefn {} {@var{fid} =} popen (@var{command}, @var{mode})
Start a process and create a pipe.

The name of the command to run is given by @var{command}.  The argument
@var{mode} may be

@table @asis
@item @qcode{"r"}
The pipe will be connected to the standard output of the process, and
open for reading.

@item @qcode{"w"}
The pipe will be connected to the standard input of the process, and
open for writing.
@end table

The file identifier corresponding to the input or output stream of the
process is returned in @var{fid}.

For example:

@example
@group
fid = popen ("ls -ltr / | tail -3", "r");
while (ischar (s = fgets (fid)))
  fputs (stdout, s);
endwhile

   @print{} drwxr-xr-x  33 root  root  3072 Feb 15 13:28 etc
   @print{} drwxr-xr-x   3 root  root  1024 Feb 15 13:28 lib
   @print{} drwxrwxrwt  15 root  root  2048 Feb 17 14:53 tmp
@end group
@end example
@seealso{popen2}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  std::string name = args(0).xstring_value ("popen: COMMAND must be a string");
  std::string mode = args(1).xstring_value ("popen: MODE must be a string");

  octave_value retval;

  stream_list& streams = interp.get_stream_list ();

  if (mode == "r")
    {
      stream ips = octave_iprocstream::create (name);

      retval = streams.insert (ips);
    }
  else if (mode == "w")
    {
      stream ops = octave_oprocstream::create (name);

      retval = streams.insert (ops);
    }
  else
    error ("popen: invalid MODE specified");

  return retval;
}

DEFMETHODX ("pclose", Fpclose, interp, args, ,
            doc: /* -*- texinfo -*-
@deftypefn {} {@var{status} =} pclose (@var{fid})
Close a file identifier @var{fid} that was opened by @code{popen}.

If successful, @code{fclose} returns 0, otherwise, it returns -1.

Programming Note: The function @code{fclose} may also be used for the same
purpose.
@seealso{fclose, popen}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  stream_list& streams = interp.get_stream_list ();

  return ovl (streams.remove (args(0), "pclose"));
}

DEFUN (tempdir, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{dir} =} tempdir ()
Return the name of the host system's directory for temporary files.

The directory name is taken first from the environment variable @env{TMPDIR}.
If that does not exist, the environment variable @env{TMP} (and on Windows
platforms also with higher priority the environment variable @env{TEMP}) is
checked.  If none of those are set, the system default returned by
@code{P_tmpdir} is used.
@seealso{P_tmpdir, tempname, mkstemp, tmpfile}
@end deftypefn */)
{
  if (args.length () > 0)
    print_usage ();

  std::string tmpdir = sys::env::get_temp_directory ();

  if (! sys::file_ops::is_dir_sep (tmpdir.back ()))
    tmpdir += sys::file_ops::dir_sep_str ();

  return ovl (tmpdir);
}

/*
%!assert (ischar (tempdir ()))

%!test
%! old_wstate = warning ("off");
%! old_tmpdir = getenv ("TMPDIR");
%! unwind_protect
%!   setenv ("TMPDIR", "__MY_TMP_DIR__");
%!   assert (tempdir (), ["__MY_TMP_DIR__" filesep()]);
%! unwind_protect_cleanup
%!   if (! isempty (old_tmpdir))
%!     setenv ("TMPDIR", old_tmpdir);
%!   else
%!     unsetenv ("TMPDIR");
%!   endif
%!   warning (old_wstate);
%! end_unwind_protect
*/

DEFUN (tempname, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{fname} =} tempname ()
@deftypefnx {} {@var{fname} =} tempname (@var{dir})
@deftypefnx {} {@var{fname} =} tempname (@var{dir}, @var{prefix})
Return a unique temporary filename as a string.

If @var{prefix} is omitted, a value of @qcode{"oct-"} is used.

If @var{dir} is also omitted, the default directory for temporary files
(@code{P_tmpdir}) is used.  If @var{dir} is provided, it must exist,
otherwise the default directory for temporary files is used.

Programming Note: Because the named file is not opened by @code{tempname},
it is possible, though relatively unlikely, that it will not be available
by the time your program attempts to open it.  If this is a concern,
@pxref{XREFtmpfile,,@code{tmpfile}}.
@seealso{mkstemp, tempdir, P_tmpdir, tmpfile}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 2)
    print_usage ();

  std::string dir;

  if (nargin > 0)
    dir = args(0).xstring_value ("tempname: DIR must be a string");

  std::string pfx ("oct-");

  if (nargin > 1)
    pfx = args(1).xstring_value ("tempname: PREFIX must be a string");

  return ovl (sys::tempnam (dir, pfx));
}

/*
%!test
%! envvar = {"TMPDIR", "TMP"};
%! envdir = cellfun (@(x) getenv (x), envvar, "uniformoutput", false);
%! unwind_protect
%!   cellfun (@(x) unsetenv (x), envvar);
%!   envname = "TMPDIR";
%!   def_tmpdir = P_tmpdir;
%!   ## Strip trailing file separators from P_tmpdir
%!   while (length (def_tmpdir) > 2 && any (def_tmpdir(end) == filesep ("all")))
%!     def_tmpdir(end) = [];
%!   endwhile
%!
%!   ## Test 0-argument form
%!   fname = tempname ();
%!   [tmpdir, tmpfname] = fileparts (fname);
%!   assert (tmpdir, def_tmpdir);
%!   assert (tmpfname (1:4), "oct-");
%!   ## Test 1-argument form
%!   tmp_tmpdir = [def_tmpdir filesep() substr(tmpfname, -5)];
%!   mkdir (tmp_tmpdir) || error ("Unable to create tmp dir");
%!   setenv (envname, def_tmpdir);
%!   fname = tempname (tmp_tmpdir);
%!   [tmpdir, tmpfname] = fileparts (fname);
%!   assert (tmpdir, tmp_tmpdir);
%!   assert (tmpfname (1:4), "oct-");
%!   ## Test 1-argument form w/null tmpdir
%!   fname = tempname ("");
%!   [tmpdir, tmpfname] = fileparts (fname);
%!   assert (tmpdir, def_tmpdir);
%!   assert (tmpfname (1:4), "oct-");
%!   ## Test 2-argument form
%!   fname = tempname (tmp_tmpdir, "pfx-");
%!   [tmpdir, tmpfname] = fileparts (fname);
%!   assert (tmpdir, tmp_tmpdir);
%!   assert (tmpfname (1:4), "pfx-");
%!   ## Test 2-argument form w/null prefix
%!   fname = tempname (tmp_tmpdir, "");
%!   [tmpdir, tmpfname] = fileparts (fname);
%!   assert (tmpdir, tmp_tmpdir);
%!   assert (tmpfname (1:4), "file");
%! unwind_protect_cleanup
%!   sts = rmdir (tmp_tmpdir);
%!   for i = 1:numel (envvar)
%!     if (isempty (envdir{i}))
%!       unsetenv (envvar{i});
%!     else
%!       setenv (envvar{i}, envdir{i});
%!     endif
%!   endfor
%! end_unwind_protect
*/

DEFMETHOD (tmpfile, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {[@var{fid}, @var{msg}] =} tmpfile ()
Return the file ID corresponding to a new temporary file with a unique
name.

The file is opened in binary read/write (@qcode{"w+b"}) mode and will be
deleted automatically when it is closed or when Octave exits.

If successful, @var{fid} is a valid file ID and @var{msg} is an empty
string.  Otherwise, @var{fid} is -1 and @var{msg} contains a
system-dependent error message.
@seealso{tempname, mkstemp, tempdir, P_tmpdir}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  octave_value_list retval;

  std::string tmpfile (sys::tempnam (sys::env::get_temp_directory (), "oct-"));

  FILE *fid = sys::fopen_tmp (tmpfile, "w+b");

  if (fid)
    {
      std::ios::openmode md = fopen_mode_to_ios_mode ("w+b");

      stream s = stdiostream::create (tmpfile, fid, md);

      if (! s)
        {
          fclose (fid);

          error ("tmpfile: failed to create stdiostream object");
        }

      stream_list& streams = interp.get_stream_list ();

      retval = ovl (streams.insert (s), "");
    }
  else
    retval = ovl (-1, std::strerror (errno));

  return retval;
}

DEFMETHOD (mkstemp, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {[@var{fid}, @var{name}, @var{msg}] =} mkstemp ("@var{template}")
@deftypefnx {} {[@var{fid}, @var{name}, @var{msg}] =} mkstemp ("@var{template}", @var{delete})
Return the file descriptor @var{fid} corresponding to a new temporary file
with a unique name created from @var{template}.

The last six characters of @var{template} must be @qcode{"XXXXXX"} and
these are replaced with a string that makes the filename unique.  The file
is then created with mode read/write and permissions that are system
dependent (on GNU/Linux systems, the permissions will be 0600 for versions
of glibc 2.0.7 and later).  The file is opened in binary mode and with the
@w{@code{O_EXCL}} flag.

If the optional argument @var{delete} is supplied and is true, the file will
be deleted automatically when Octave exits.

If successful, @var{fid} is a valid file ID, @var{name} is the name of the
file, and @var{msg} is an empty string.  Otherwise, @var{fid} is -1,
@var{name} is empty, and @var{msg} contains a system-dependent error
message.
@seealso{tempname, tempdir, P_tmpdir, tmpfile, fopen}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  std::string tmpl8 = args(0).xstring_value ("mkstemp: TEMPLATE argument must be a string");

  octave_value_list retval = ovl (-1, "", "");

  OCTAVE_LOCAL_BUFFER (char, tmp, tmpl8.size () + 1);
  strcpy (tmp, tmpl8.c_str ());

  int fd = octave_mkostemp_wrapper (tmp);

  if (fd < 0)
    {
      retval(0) = fd;
      retval(2) = std::strerror (errno);
    }
  else
    {
      const char *fopen_mode = "w+b";

      FILE *fid = fdopen (fd, fopen_mode);

      if (! fid)
        {
          retval(0) = -1;
          retval(2) = std::strerror (errno);
        }
      else
        {
          std::string nm = tmp;

          std::ios::openmode md = fopen_mode_to_ios_mode (fopen_mode);

          stream s = stdiostream::create (nm, fid, md);

          if (! s)
            error ("mkstemp: failed to create stdiostream object");

          stream_list& streams = interp.get_stream_list ();

          retval(0) = streams.insert (s);
          retval(1) = nm;

          if (nargin == 2 && args(1).is_true ())
            interp.mark_for_deletion (nm);
        }
    }

  return retval;
}

// FIXME: This routine also exists verbatim in syscalls.cc.
//        Maybe change to be a general utility routine.
static int
convert (int x, int ibase, int obase)
{
  int retval = 0;

  int tmp = x % obase;

  if (tmp > ibase - 1)
    error ("umask: invalid digit");

  retval = tmp;
  int mult = ibase;
  while ((x = (x - tmp) / obase))
    {
      tmp = x % obase;

      if (tmp > ibase - 1)
        error ("umask: invalid digit");

      retval += mult * tmp;
      mult *= ibase;
    }

  return retval;
}

DEFUNX ("umask", Fumask, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{oldmask} =} umask (@var{mask})
Set the permission mask for file creation.

The parameter @var{mask} is an integer, interpreted as an octal number.

If successful, returns the previous value of the mask (as an integer to be
interpreted as an octal number); otherwise an error message is printed.

The permission mask is a UNIX concept used when creating new objects on a
file system such as files, directories, or named FIFOs.  The object to be
created has base permissions in an octal number @var{mode} which are
modified according to the octal value of @var{mask}.  The final permissions
for the new object are @code{@var{mode} - @var{mask}}.
@seealso{fopen, mkdir, mkfifo}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  int mask = args(0).xint_value ("umask: MASK must be an integer");

  if (mask < 0)
    error ("umask: MASK must be a positive integer value");

  int oct_mask = convert (mask, 8, 10);

  int status = convert (sys::umask (oct_mask), 10, 8);

  if (status >= 0)
    return ovl (status);
  else
    return ovl ();
}

static octave_value
const_value (const char *, const octave_value_list& args, int val)
{
  if (args.length () != 0)
    print_usage ();

  return octave_value (val);
}

DEFUNX ("P_tmpdir", FP_tmpdir, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{sys_tmpdir} =} P_tmpdir ()
Return the name of the host system's @strong{default} directory for
temporary files.

Programming Note: The value returned by @code{P_tmpdir} is always the
default location.  This value may not agree with that returned from
@code{tempdir} if the user has overridden the default with the @env{TMPDIR}
environment variable.
@seealso{tempdir, tempname, mkstemp, tmpfile}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (get_P_tmpdir ());
}

// NOTE: the values of SEEK_SET, SEEK_CUR, and SEEK_END have to be
//       this way for Matlab compatibility.

DEFUNX ("SEEK_SET", FSEEK_SET, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{fseek_origin} =} SEEK_SET ()
Return the numerical value to pass to @code{fseek} to position the file pointer
relative to the beginning of the file.
@seealso{SEEK_CUR, SEEK_END, fseek}
@end deftypefn */)
{
  return const_value ("SEEK_SET", args, -1);
}

DEFUNX ("SEEK_CUR", FSEEK_CUR, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{fseek_origin} =} SEEK_CUR ()
Return the numerical value to pass to @code{fseek} to position the file pointer
relative to the current position.
@seealso{SEEK_SET, SEEK_END, fseek}
@end deftypefn */)
{
  return const_value ("SEEK_CUR", args, 0);
}

DEFUNX ("SEEK_END", FSEEK_END, args, ,
        doc: /* -*- texinfo -*-
@deftypefn {} {@var{fseek_origin} =} SEEK_END ()
Return the numerical value to pass to @code{fseek} to position the file pointer
relative to the end of the file.
@seealso{SEEK_SET, SEEK_CUR, fseek}
@end deftypefn */)
{
  return const_value ("SEEK_END", args, 1);
}

static octave_value
const_value (const char *, const octave_value_list& args,
             const octave_value& val)
{
  if (args.length () != 0)
    print_usage ();

  return octave_value (val);
}

DEFMETHODX ("stdin", Fstdin, interp, args, ,
            doc: /* -*- texinfo -*-
@deftypefn {} {@var{fid} =} stdin ()
Return the numeric value corresponding to the standard input stream.

When Octave is used interactively, stdin is filtered through the command
line editing functions.
@seealso{stdout, stderr}
@end deftypefn */)
{
  stream_list& streams = interp.get_stream_list ();

  return const_value ("stdin", args, streams.stdin_file ());
}

DEFMETHODX ("stdout", Fstdout, interp, args, ,
            doc: /* -*- texinfo -*-
@deftypefn {} {@var{fid} =} stdout ()
Return the numeric value corresponding to the standard output stream.

Data written to the standard output may be filtered through the pager.
@seealso{stdin, stderr, page_screen_output}
@end deftypefn */)
{
  stream_list& streams = interp.get_stream_list ();

  return const_value ("stdout", args, streams.stdout_file ());
}

DEFMETHODX ("stderr", Fstderr, interp, args, ,
            doc: /* -*- texinfo -*-
@deftypefn {} {@var{fid} =} stderr ()
Return the numeric value corresponding to the standard error stream.

Even if paging is turned on, the standard error is not sent to the pager.
It is useful for error messages and prompts.
@seealso{stdin, stdout}
@end deftypefn */)
{
  stream_list& streams = interp.get_stream_list ();

  return const_value ("stderr", args, streams.stderr_file ());
}

OCTAVE_END_NAMESPACE(octave)
