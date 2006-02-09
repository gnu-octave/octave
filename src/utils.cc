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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cerrno>
#include <climits>
#include <cstring>

#include <fstream>
#include <iostream>
#include <string>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "quit.h"

#include "dir-ops.h"
#include "file-ops.h"
#include "file-stat.h"
#include "lo-mappers.h"
#include "lo-sstream.h"
#include "oct-cmplx.h"
#include "oct-env.h"
#include "pathsearch.h"
#include "str-vec.h"

#include "Cell.h"
#include <defaults.h>
#include "defun.h"
#include "dirfns.h"
#include "error.h"
#include "gripes.h"
#include "input.h"
#include "oct-errno.h"
#include "oct-hist.h"
#include "oct-obj.h"
#include "pager.h"
#include "sysdep.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// If TRUE, print a warning for expressions like
//
//   ones (-1, 5)
//
static int Vwarn_neg_dim_as_zero;

// Return TRUE if S is a valid identifier.

bool
valid_identifier (const char *s)
{
  if (! s || ! (isalpha (*s) || *s == '_' || *s == '$'))
     return false;

  while (*++s != '\0')
    if (! (isalnum (*s) || *s == '_' || *s == '$'))
      return false;

  return true;
}

bool
valid_identifier (const std::string& s)
{
  return valid_identifier (s.c_str ());
}

DEFCMD (isvarname, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isvarname (@var{name})\n\
Return true if @var{name} is a valid variable name\n\
@end deftypefn")
{
  octave_value retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("isvarname");

  if (error_state)
    return retval;

  if (argc == 2)
    retval = valid_identifier (argv[1]);
  else
    print_usage ("isvarname");

  return retval;
}

int
almost_match (const std::string& std, const std::string& s, int min_match_len,
	      int case_sens)
{
  int stdlen = std.length ();
  int slen = s.length ();

  return (slen <= stdlen
	  && slen >= min_match_len
	  && (case_sens
	      ? (strncmp (std.c_str (), s.c_str (), slen) == 0)
	      : (octave_strncasecmp (std.c_str (), s.c_str (), slen) == 0)));
}

// Ugh.

int
keyword_almost_match (const char * const *std, int *min_len, const std::string& s,
		      int min_toks_to_match, int max_toks)
{
  int status = 0;
  int tok_count = 0;
  int toks_matched = 0;

  if (s.empty () || max_toks < 1)
    return status;

  char *kw = strsave (s.c_str ());

  char *t = kw;
  while (*t != '\0')
    {
      if (*t == '\t')
	*t = ' ';
      t++;
    }

  char *beg = kw;
  while (*beg == ' ')
    beg++;

  if (*beg == '\0')
    return status;


  const char **to_match = new const char * [max_toks + 1];
  const char * const *s1 = std;
  const char **s2 = to_match;

  if (! s1 || ! s2)
    goto done;

  s2[tok_count] = beg;
  char *end;
  while ((end = strchr (beg, ' ')) != 0)
    {
      *end = '\0';
      beg = end + 1;

      while (*beg == ' ')
	beg++;

      if (*beg == '\0')
	break;

      tok_count++;
      if (tok_count >= max_toks)
	goto done;

      s2[tok_count] = beg;
    }
  s2[tok_count+1] = 0;

  s2 = to_match;

  for (;;)
    {
      if (! almost_match (*s1, *s2, min_len[toks_matched], 0))
	goto done;

      toks_matched++;

      s1++;
      s2++;

      if (! *s2)
	{
	  status = (toks_matched >= min_toks_to_match);
	  goto done;
	}

      if (! *s1)
	goto done;
    }

 done:

  delete [] kw;
  delete [] to_match;

  return status;
}

// Return non-zero if either NR or NC is zero.  Return -1 if this
// should be considered fatal; return 1 if this is ok.

int
empty_arg (const char * /* name */, octave_idx_type nr, octave_idx_type nc)
{
  return (nr == 0 || nc == 0);
}

// See if the given file is in the path.

std::string
search_path_for_file (const std::string& path, const string_vector& names)
{
  dir_path p (path);

  return octave_env::make_absolute (p.find_first_of (names),
				    octave_env::getcwd ());
}

// Find all locations of the given file in the path.

string_vector
search_path_for_all_files (const std::string& path, const string_vector& names)
{
  dir_path p (path);

  string_vector sv = p.find_all_first_of (names);

  int len = sv.length ();

  for (int i = 0; i < len; i++)
    sv[i] = octave_env::make_absolute (sv[i], octave_env::getcwd ());

  return sv;
}

static string_vector
make_absolute (const string_vector& sv)
{
  int len = sv.length ();

  for (int i = 0; i < len; i++)
    sv[i] = octave_env::make_absolute (sv[i], octave_env::getcwd ());

  return sv;
}

DEFUN (file_in_loadpath, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} file_in_loadpath (@var{file})\n\
@deftypefnx {Built-in Function} {} file_in_loadpath (@var{file}, \"all\")\n\
\n\
Return the absolute name of @var{file} if it can be found in\n\
the list of directories specified by @code{LOADPATH}.\n\
If no file is found, return an empty matrix.\n\
\n\
If the first argument is a cell array of strings, search each\n\
directory of the loadpath for element of the cell array and return\n\
the first that matches.\n\
\n\
If the second optional argument @code{\"all\"} is supplied, return\n\
a cell array containing the list of all files that have the same\n\
name in the path.  If no files are found, return an empty cell array.\n\
@end deftypefn\n\
@seealso{file_in_path, LOADPATH}")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      string_vector names = args(0).all_strings ();

      if (! error_state && names.length () > 0)
	{
	  if (nargin == 1)
	    {
	      std::string fname = octave_env::make_absolute
		(Vload_path_dir_path.find_first_of (names),
		 octave_env::getcwd ());

	      if (fname.empty ())
		retval = Matrix ();
	      else
		retval = fname;
	    }
	  else if (nargin == 2)
	    {
	      std::string opt = args(1).string_value ();

	      if (! error_state && opt == "all")
		retval = Cell (make_absolute (Vload_path_dir_path.find_all_first_of (names)));
	      else
		error ("file_in_loadpath: invalid option");
	    }
	}
      else
	error ("file_in_loadpath: expecting string as first argument");
    }
  else
    print_usage ("file_in_loadpath");

  return retval;
}

DEFUN (file_in_path, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} file_in_path (@var{path}, @var{file})\n\
@deftypefnx {Built-in Function} {} file_in_path (@var{path}, @var{file}, \"all\")\n\
Return the absolute name of @var{file} if it can be found in\n\
@var{path}.  The value of @var{path} should be a colon-separated list of\n\
directories in the format described for the built-in variable\n\
@code{LOADPATH}.  If no file is found, return an empty matrix.\n\
For example,\n\
\n\
@example\n\
file_in_path (EXEC_PATH, \"sh\")\n\
     @result{} \"/bin/sh\"\n\
@end example\n\
\n\
If the second argument is a cell array of strings, search each\n\
directory of the path for element of the cell array and return\n\
the first that matches.\n\
\n\
If the third optional argument @code{\"all\"} is supplied, return\n\
a cell array containing the list of all files that have the same\n\
name in the path.  If no files are found, return an empty cell array.\n\
\n\
Note that @code{file_in_path} does not expand leading, trailing,\n\
or doubled colons the way that @code{file_in_loadpath} does.  See\n\
also @xref{Function Files}, for an explanation of\n\
how colon expansion is applied to @code{LOADPATH}.\n\
@seealso{file_in_loadpath}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 2 || nargin == 3)
    {
      std::string path = args(0).string_value ();

      if (! error_state)
	{
	  string_vector names = args(1).all_strings ();

	  if (! error_state && names.length () > 0)
	    {
	      if (nargin == 2)
		{
		  std::string fname = search_path_for_file (path, names);

		  if (fname.empty ())
		    retval = Matrix ();
		  else
		    retval = fname;
		}
	      else if (nargin == 3)
		{
		  std::string opt = args(2).string_value ();

		  if (! error_state && opt == "all")
		    retval = Cell (make_absolute (search_path_for_all_files (path, names)));
		  else
		    error ("file_in_path: invalid option");
		}
	    }
	  else
	    error ("file_in_path: expecting string as second argument");
	}
      else
	error ("file_in_path: expecting string as first argument");
    }
  else
    print_usage ("file_in_path");

  return retval;
}

std::string
file_in_path (const std::string& name, const std::string& suffix)
{
  std::string nm = name;

  if (! suffix.empty ())
    nm.append (suffix);

  return octave_env::make_absolute (Vload_path_dir_path.find (nm),
				    octave_env::getcwd ());
}

// See if there is an function file in the path.  If so, return the
// full path to the file.

std::string
fcn_file_in_path (const std::string& name)
{
  std::string retval;

  int len = name.length ();
  
  if (len > 0)
    {
      if (len > 2 && name [len - 2] == '.' && name [len - 1] == 'm')
	retval = file_in_path (name, "");
      else
	retval = file_in_path (name, ".m");
    }

  return retval;
}

// See if there is an octave file in the path.  If so, return the
// full path to the file.

std::string
oct_file_in_path (const std::string& name)
{
  std::string retval;

  int len = name.length ();
  
  if (len > 0)
    {
      if (len > 4 && name [len - 4] == '.' && name [len - 3] == 'o'
	  && name [len - 2] == 'c' && name [len - 1] == 't')
	retval = file_in_path (name, "");
      else
	retval = file_in_path (name, ".oct");
    }

  return retval;
}

// Replace backslash escapes in a string with the real values.

std::string
do_string_escapes (const std::string& s)
{
  std::string retval;

  size_t i = 0;
  size_t j = 0;
  size_t len = s.length ();

  retval.resize (len);

  while (j < len)
    {
      if (s[j] == '\\' && j+1 < len)
	{
	  switch (s[++j])
	    {
	    case '0':
	      retval[i] = '\0';
	      break;

	    case 'a':
	      retval[i] = '\a';
	      break;

	    case 'b': // backspace
	      retval[i] = '\b';
	      break;

	    case 'f': // formfeed
	      retval[i] = '\f';
	      break;

	    case 'n': // newline
	      retval[i] = '\n';
	      break;

	    case 'r': // carriage return
	      retval[i] = '\r';
	      break;

	    case 't': // horizontal tab
	      retval[i] = '\t';
	      break;

	    case 'v': // vertical tab
	      retval[i] = '\v';
	      break;

	    case '\\': // backslash
	      retval[i] = '\\';
	      break;

	    case '\'': // quote
	      retval[i] = '\'';
	      break;

	    case '"': // double quote
	      retval[i] = '"';
	      break;

	    default:
	      warning ("unrecognized escape sequence `\\%c' --\
 converting to `%c'", s[j], s[j]);
	      retval[i] = s[j];
	      break;
	    }
	}
      else
	{
	  retval[i] = s[j];
	}

      i++;
      j++;
    }

  retval.resize (i);

  return retval;
}

DEFUN (do_string_escapes, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} do_string_escapes (@var{string})\n\
Convert special characters in @var{string} to their escaped forms.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      if (args(0).is_string ())
	retval = do_string_escapes (args(0).string_value ());
      else
	error ("do_string_escapes: argument must be a string");
    }
  else
    print_usage ("do_string_escapes");

  return retval;
}

const char *
undo_string_escape (char c)
{
  if (! c)
    return "";

  switch (c)
    {
    case '\0':
      return "\\0";

    case '\a':
      return "\\a";

    case '\b': // backspace
      return "\\b";

    case '\f': // formfeed
      return "\\f";

    case '\n': // newline
      return "\\n";

    case '\r': // carriage return
      return "\\r";

    case '\t': // horizontal tab
      return "\\t";

    case '\v': // vertical tab
      return "\\v";

    case '\\': // backslash
      return "\\\\";

    case '"': // double quote
      return "\\\"";

    default:
      {
	static char retval[2];
	retval[0] = c;
	retval[1] = '\0';
	return retval;
      }
    }
}

std::string
undo_string_escapes (const std::string& s)
{
  std::string retval;

  for (size_t i = 0; i < s.length (); i++)
    retval.append (undo_string_escape (s[i]));

  return retval;
}

DEFUN (undo_string_escapes, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} undo_string_escapes (@var{s})\n\
Converts special characters in strings back to their escaped forms.  For\n\
example, the expression\n\
\n\
@example\n\
bell = \"\\a\";\n\
@end example\n\
\n\
@noindent\n\
assigns the value of the alert character (control-g, ASCII code 7) to\n\
the string variable @code{bell}.  If this string is printed, the\n\
system will ring the terminal bell (if it is possible).  This is\n\
normally the desired outcome.  However, sometimes it is useful to be\n\
able to print the original representation of the string, with the\n\
special characters replaced by their escape sequences.  For example,\n\
\n\
@example\n\
octave:13> undo_string_escapes (bell)\n\
ans = \\a\n\
@end example\n\
\n\
@noindent\n\
replaces the unprintable alert character with its printable\n\
representation.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      if (args(0).is_string ())
	retval = undo_string_escapes (args(0).string_value ());
      else
	error ("undo_string_escapes: argument must be a string");
    }
  else
    print_usage ("undo_string_escapes");

  return retval;
}

DEFUN (find_first_of_in_loadpath, args, , "")
{
  octave_value retval;

  if (args.length () == 1)
    {
      string_vector names = args(0).all_strings ();

      if (! error_state)
	retval = Vload_path_dir_path.find_all_first_of (names);
    }
  else
    print_usage ("find_first_of_in_loadpath");

  return retval;
}

DEFUNX ("errno", Ferrno, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{err} =} errno ()\n\
@deftypefnx {Built-in Function} {@var{err} =} errno (@var{val})\n\
@deftypefnx {Built-in Function} {@var{err} =} errno (@var{name})\n\
Return the current value of the system-dependent variable errno,\n\
set its value to @var{val} and return the previous value, or return\n\
the named error code given @var{name} as a character string, or -1\n\
if @var{name} is not found.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      if (args(0).is_string ())
	{
	  std::string nm = args(0).string_value ();

	  if (! error_state)
	    retval = octave_errno::lookup (nm);
	  else
	    error ("errno: expecting character string argument");
	}
      else
	{
	  int val = args(0).int_value ();

	  if (! error_state)
	    retval = octave_errno::set (val);
	  else
	    error ("errno: expecting integer argument");
	}
    }
  else if (nargin == 0)
    retval = octave_errno::get ();
  else
    print_usage ("errno");

  return retval;
}

DEFUN (errno_list, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} errno_list ()\n\
Return a structure containing the system-dependent errno values.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 0)
    retval = octave_errno::list ();
  else
    print_usage ("errno_list");

  return retval;
}

static void
warn_old_style_preference (bool val, const std::string& sval)
{
  warning
    ("preference of \"%s\" is obsolete -- use numeric value of %d instead",
     sval.c_str (), (val ? 1 : 0));
}

// Check the value of a string variable to see if it it's ok to do
// something.
//
//   return of  1 => always ok.
//   return of  0 => never ok.
//   return of -1 => ok, but give me warning (default).

int
check_preference (const std::string& var)
{
  int pref = -1;

  std::string val = builtin_string_variable (var);

  if (val.empty ())
    {
      double dval = 0;
      if (builtin_real_scalar_variable (var, dval))
	pref = NINT (dval);
    }
  else
    {
      if (val == "yes" || val == "true")
	{
	  warn_old_style_preference (true, val);
	  pref = 1;
	}
      else if (val == "never" || val == "no" || val == "false")
	{
	  warn_old_style_preference (false, val);
	  pref = 0;
	}
    }

  return pref;
}

static void
check_dimensions (octave_idx_type& nr, octave_idx_type& nc, const char *warnfor)
{
  if (nr < 0 || nc < 0)
    {
      if (Vwarn_neg_dim_as_zero)
	warning ("%s: converting negative dimension to zero", warnfor);

      nr = (nr < 0) ? 0 : nr;
      nc = (nc < 0) ? 0 : nc;
    }
}

void
check_dimensions (dim_vector& dim, const char *warnfor)
{
  bool neg = false;

  for (int i = 0; i < dim.length (); i++)
    {
      if (dim(i) < 0)
        {
          dim(i) = 0;
          neg = true;
        }
    }

  if (neg && Vwarn_neg_dim_as_zero)
    warning ("%s: converting negative dimension to zero", warnfor);
}


void
get_dimensions (const octave_value& a, const char *warn_for,
                dim_vector& dim)
{
  if (a.is_scalar_type ())
    {
      dim.resize (2);
      dim(0) = a.int_value ();
      dim(1) = dim(0);
    }
  else
    {
      octave_idx_type nr = a.rows ();
      octave_idx_type nc = a.columns ();

      if (nr == 1 || nc == 1)
        {
          Array<double> v = a.vector_value ();

          if (error_state)
            return;

          octave_idx_type n = v.length ();
          dim.resize (n);
          for (octave_idx_type i = 0; i < n; i++)
            dim(i) = static_cast<int> (fix (v(i)));
        }
      else
        error ("%s (A): use %s (size (A)) instead", warn_for, warn_for);
    }

  if (! error_state)
    check_dimensions (dim, warn_for); // May set error_state.
}


void
get_dimensions (const octave_value& a, const char *warn_for,
		octave_idx_type& nr, octave_idx_type& nc)
{
  if (a.is_scalar_type ())
    {
      nr = nc = a.int_value ();
    }
  else
    {
      nr = a.rows ();
      nc = a.columns ();

      if ((nr == 1 && nc == 2) || (nr == 2 && nc == 1))
	{
	  Array<double> v = a.vector_value ();

	  if (error_state)
	    return;

	  nr = static_cast<octave_idx_type> (fix (v (0)));
	  nc = static_cast<octave_idx_type> (fix (v (1)));
	}
      else
	error ("%s (A): use %s (size (A)) instead", warn_for, warn_for);
    }

  if (! error_state)
    check_dimensions (nr, nc, warn_for); // May set error_state.
}

void
get_dimensions (const octave_value& a, const octave_value& b,
		const char *warn_for, octave_idx_type& nr, octave_idx_type& nc)
{
  nr = a.is_empty () ? 0 : a.int_value ();
  nc = b.is_empty () ? 0 : b.int_value ();

  if (error_state)
    error ("%s: expecting two scalar arguments", warn_for);
  else
    check_dimensions (nr, nc, warn_for); // May set error_state.
}

Matrix
identity_matrix (octave_idx_type nr, octave_idx_type nc)
{
  Matrix m (nr, nc, 0.0);

  if (nr > 0 && nc > 0)
    {
      octave_idx_type n = std::min (nr, nc);

      for (octave_idx_type i = 0; i < n; i++)
	m (i, i) = 1.0;
    }

  return m;
}

extern int
octave_format (std::ostream& os, const char *fmt, ...)
{
  int retval = -1;

  va_list args;
  va_start (args, fmt);

  retval = octave_vformat (os, fmt, args);

  va_end (args);

  return retval;
}

extern int
octave_vformat (std::ostream& os, const char *fmt, va_list args)
{
  int retval = -1;

#if defined (__GNUG__) && !CXX_ISO_COMPLIANT_LIBRARY

  std::streambuf *sb = os.rdbuf ();

  if (sb)
    {
      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      retval = sb->vform (fmt, args);

      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
    }

#else

  char *s = octave_vsnprintf (fmt, args);

  if (s)
    {
      os << s;

      retval = strlen (s);
    }

#endif

  return retval;
}

/* XXX FIXME XXX -- we really need a configure test for this.  */

#if defined __GNUC__ && __GNUC__ >= 3
#define HAVE_C99_VSNPRINTF 1
#endif

// We manage storage.  User should not free it, and its contents are
// only valid until next call to vsnprintf.

// Interrupts might happen if someone makes a call with something that
// will require a very large buffer.  If we are interrupted in that
// case, we should make the buffer size smaller for the next call.

#define BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE_FOR_VSNPRINTF \
  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE_1; \
  delete [] buf; \
  buf = 0; \
  size = initial_size; \
  octave_throw_interrupt_exception (); \
  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE_2

#if defined __GNUC__ && defined __va_copy
#define SAVE_ARGS(saved_args, args) __va_copy (saved_args, args)
#elif defined va_copy
#define SAVE_ARGS(saved_args, args) va_copy (saved_args, args)
#else
#define SAVE_ARGS(saved_args, args) saved_args = args
#endif

char *
octave_vsnprintf (const char *fmt, va_list args)
{
  static const size_t initial_size = 100;

  static size_t size = initial_size;

  static char *buf = 0;

#if defined (HAVE_C99_VSNPRINTF)
  size_t nchars;
#else
  int nchars;
#endif

  if (! buf)
    buf = new char [size];

  if (! buf)
    return 0;

#if defined (HAVE_C99_VSNPRINTF)

  // Note that the caller is responsible for calling va_end on args.
  // We will do it for saved_args.

  va_list saved_args;

  SAVE_ARGS (saved_args, args);

  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE_FOR_VSNPRINTF;

  nchars = octave_raw_vsnprintf (buf, size, fmt, args);

  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

  if (nchars >= size)
    {
      size = nchars + 1;

      delete [] buf;

      buf = new char [size];

      if (buf)
	{
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE_FOR_VSNPRINTF;

	  octave_raw_vsnprintf (buf, size, fmt, saved_args);

	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	}
    }

  va_end (saved_args);

#else

  while (1)
    {
      va_list saved_args;

      SAVE_ARGS (saved_args, args);

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE_FOR_VSNPRINTF;

      nchars = octave_raw_vsnprintf (buf, size, fmt, saved_args);

      va_end (saved_args);

      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      if (nchars > -1 && nchars < size-1)
       return buf;
      else
       {
	 delete [] buf;

         size *= 2;

	 buf = new char [size];

         if (! buf)
           return 0;
       }
    }

#endif

  return buf;
}

char *
octave_snprintf (const char *fmt, ...)
{
  char *retval = 0;

  va_list args;
  va_start (args, fmt);

  retval = octave_vsnprintf (fmt, args);

  va_end (args);

  return retval;
}

void
octave_sleep (double seconds)
{
  if (seconds > 0)
    {
      double t;

      unsigned int usec
	= static_cast<unsigned int> (modf (seconds, &t) * 1000000);

      unsigned int sec
	= (t > UINT_MAX) ? UINT_MAX : static_cast<unsigned int> (t);

      octave_sleep (sec);
      octave_usleep (usec);
    }
}

static int
warn_neg_dim_as_zero (void)
{
  Vwarn_neg_dim_as_zero = check_preference ("warn_neg_dim_as_zero");

  return 0;
}

void
symbols_of_utils (void)
{
  DEFVAR (warn_neg_dim_as_zero, false, warn_neg_dim_as_zero,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} warn_neg_dim_as_zero\n\
If the value of @code{warn_neg_dim_as_zero} is nonzero, print a warning\n\
for expressions like\n\
\n\
@example\n\
eye (-1)\n\
@end example\n\
\n\
@noindent\n\
The default value is 0.\n\
@end defvr");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
