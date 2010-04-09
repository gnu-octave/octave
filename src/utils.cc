/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
              2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009 John W. Eaton

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

#include <cerrno>
#include <climits>
#include <cstring>

#include <fstream>
#include <iostream>
#include <string>

#include <sys/types.h>
#include <unistd.h>

#include "quit.h"

#include "dir-ops.h"
#include "file-ops.h"
#include "file-stat.h"
#include "lo-mappers.h"
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
#include "lex.h"
#include "load-path.h"
#include "oct-errno.h"
#include "oct-hist.h"
#include "oct-obj.h"
#include "pager.h"
#include "sysdep.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

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

DEFUN (isvarname, args, ,
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
    retval = valid_identifier (argv[1]) && ! is_keyword (argv[1]);
  else
    print_usage ();

  return retval;
}

// Return TRUE if F and G are both names for the same file.

bool
same_file (const std::string& f, const std::string& g)
{
  return same_file_internal (f, g);
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

  return octave_env::make_absolute (p.find_first_of (names));
}

// Find all locations of the given file in the path.

string_vector
search_path_for_all_files (const std::string& path, const string_vector& names)
{
  dir_path p (path);

  string_vector sv = p.find_all_first_of (names);

  octave_idx_type len = sv.length ();

  for (octave_idx_type i = 0; i < len; i++)
    sv[i] = octave_env::make_absolute (sv[i]);

  return sv;
}

static string_vector
make_absolute (const string_vector& sv)
{
  octave_idx_type len = sv.length ();

  string_vector retval (len);

  for (octave_idx_type i = 0; i < len; i++)
    retval[i] = octave_env::make_absolute (sv[i]);
 
  return retval;
}

DEFUN (file_in_loadpath, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} file_in_loadpath (@var{file})\n\
@deftypefnx {Built-in Function} {} file_in_loadpath (@var{file}, \"all\")\n\
\n\
Return the absolute name of @var{file} if it can be found in\n\
the list of directories specified by @code{path}.\n\
If no file is found, return an empty matrix.\n\
\n\
If the first argument is a cell array of strings, search each\n\
directory of the loadpath for element of the cell array and return\n\
the first that matches.\n\
\n\
If the second optional argument @code{\"all\"} is supplied, return\n\
a cell array containing the list of all files that have the same\n\
name in the path.  If no files are found, return an empty cell array.\n\
@seealso{file_in_path, path}\n\
@end deftypefn")
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
              std::string fname
                = octave_env::make_absolute (load_path::find_first_of (names));

              if (fname.empty ())
                retval = Matrix ();
              else
                retval = fname;
            }
          else if (nargin == 2)
            {
              std::string opt = args(1).string_value ();

              if (! error_state && opt == "all")
                retval = Cell (make_absolute
                               (load_path::find_all_first_of (names)));
              else
                error ("file_in_loadpath: invalid option");
            }
        }
      else
        error ("file_in_loadpath: expecting string as first argument");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (file_in_path, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} file_in_path (@var{path}, @var{file})\n\
@deftypefnx {Built-in Function} {} file_in_path (@var{path}, @var{file}, \"all\")\n\
Return the absolute name of @var{file} if it can be found in\n\
@var{path}.  The value of @var{path} should be a colon-separated list of\n\
directories in the format described for @code{path}.  If no file\n\
is found, return an empty matrix.  For example,\n\
\n\
@example\n\
@group\n\
file_in_path (EXEC_PATH, \"sh\")\n\
     @result{} \"/bin/sh\"\n\
@end group\n\
@end example\n\
\n\
If the second argument is a cell array of strings, search each\n\
directory of the path for element of the cell array and return\n\
the first that matches.\n\
\n\
If the third optional argument @code{\"all\"} is supplied, return\n\
a cell array containing the list of all files that have the same\n\
name in the path.  If no files are found, return an empty cell array.\n\
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
                    retval = Cell (make_absolute
                                   (search_path_for_all_files (path, names)));
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
    print_usage ();

  return retval;
}

std::string
file_in_path (const std::string& name, const std::string& suffix)
{
  std::string nm = name;

  if (! suffix.empty ())
    nm.append (suffix);

  return octave_env::make_absolute (load_path::find_file (nm));
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
      if (octave_env::absolute_pathname (name))
        {
          file_stat fs (name);

          if (fs.exists ())
            retval = name;
        }
      else if (len > 2 && name [len - 2] == '.' && name [len - 1] == 'm')
        retval = load_path::find_fcn_file (name.substr (0, len-2));
      else
        {
          std::string fname = name;
          size_t pos = name.find_first_of (Vfilemarker);
          if (pos != std::string::npos)
            fname = name.substr (0, pos);

          retval = load_path::find_fcn_file (fname);
        }
    }

  return retval;
}

// See if there is a directory called "name" in the path and if it
// contains a Contents.m file return the full path to this file.

std::string
contents_file_in_path (const std::string& dir)
{
  std::string retval;

  if (dir.length () > 0)
    {
      std::string tcontents = file_ops::concat (load_path::find_dir (dir), 
                                                std::string ("Contents.m"));

      file_stat fs (tcontents);

      if (fs.exists ())
        retval = octave_env::make_absolute (tcontents);
    }

  return retval;
}

// See if there is a .oct file in the path.  If so, return the
// full path to the file.

std::string
oct_file_in_path (const std::string& name)
{
  std::string retval;

  int len = name.length ();
  
  if (len > 0)
    {
      if (octave_env::absolute_pathname (name))
        {
          file_stat fs (name);

          if (fs.exists ())
            retval = name;
        }
      else if (len > 4 && name [len - 4] == '.' && name [len - 3] == 'o'
               && name [len - 2] == 'c' && name [len - 1] == 't')
        retval = load_path::find_oct_file (name.substr (0, len-4));
      else
        retval = load_path::find_oct_file (name);
    }

  return retval;
}

// See if there is a .mex file in the path.  If so, return the
// full path to the file.

std::string
mex_file_in_path (const std::string& name)
{
  std::string retval;

  int len = name.length ();
  
  if (len > 0)
    {
      if (octave_env::absolute_pathname (name))
        {
          file_stat fs (name);

          if (fs.exists ())
            retval = name;
        }
      else if (len > 4 && name [len - 4] == '.' && name [len - 3] == 'm'
               && name [len - 2] == 'e' && name [len - 1] == 'x')
        retval = load_path::find_mex_file (name.substr (0, len-4));
      else
        retval = load_path::find_mex_file (name);
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
    print_usage ();

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
@group\n\
octave:13> undo_string_escapes (bell)\n\
ans = \\a\n\
@end group\n\
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
    print_usage ();

  return retval;
}

DEFUN (is_absolute_filename, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} is_absolute_filename (@var{file})\n\
Return true if @var{file} is an absolute filename.\n\
@end deftypefn")
{
  octave_value retval = false;

  if (args.length () == 1)
    retval = (args(0).is_string ()
              && octave_env::absolute_pathname (args(0).string_value ()));
  else
    print_usage ();

  return retval;
}

DEFUN (is_rooted_relative_filename, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} is_rooted_relative_filename (@var{file})\n\
Return true if @var{file} is a rooted-relative filename.\n\
@end deftypefn")
{
  octave_value retval = false;

  if (args.length () == 1)
    retval = (args(0).is_string ()
              && octave_env::rooted_relative_pathname (args(0).string_value ()));
  else
    print_usage ();

  return retval;
}

DEFUN (make_absolute_filename, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} make_absolute_filename (@var{file})\n\
Return the full name of @var{file}, relative to the current directory.\n\
@end deftypefn")
{
  octave_value retval = std::string ();

  if (args.length () == 1)
    {
      std::string nm = args(0).string_value ();

      if (! error_state)
        retval = octave_env::make_absolute (nm);
      else
        error ("make_absolute_filename: expecting argument to be a file name");
    }      
  else
    print_usage ();

  return retval;
}

DEFUN (find_dir_in_path, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} find_dir_in_path (@var{dir}, \"all\")\n\
Return the full name of the path element matching @var{dir}.  The\n\
match is performed at the end of each path element.  For example, if\n\
@var{dir} is @code{\"foo/bar\"}, it matches the path element\n\
@code{\"/some/dir/foo/bar\"}, but not @code{\"/some/dir/foo/bar/baz\"}\n\
or @code{\"/some/dir/allfoo/bar\"}.\n\
\n\
The second argument is optional.  If it is supplied, return a cell array\n\
containing all the directory names that match.\n\
@end deftypefn")
{
  octave_value retval = std::string ();

  int nargin = args.length ();

  std::string dir;

  if (nargin == 1 || nargin == 2)
    {
      dir = args(0).string_value ();

      if (! error_state)
        {
          if (nargin == 1)
            retval = load_path::find_dir (dir);
          else if (nargin == 2)
            retval = Cell (load_path::find_matching_dirs (dir));
        }
      else
        error ("find_dir_in_path: expecting argument to be a directory name");
    }
  else
    print_usage ();

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
    print_usage ();

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
    print_usage ();

  return retval;
}

static void
check_dimensions (octave_idx_type& nr, octave_idx_type& nc, const char *warnfor)
{
  if (nr < 0 || nc < 0)
    {
      warning_with_id ("Octave:neg-dim-as-zero",
                       "%s: converting negative dimension to zero", warnfor);

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

  if (neg)
    warning_with_id ("Octave:neg-dim-as-zero",
                     "%s: converting negative dimension to zero", warnfor);
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

octave_idx_type
dims_to_numel (const dim_vector& dims, const octave_value_list& idx)
{
  octave_idx_type retval;

  octave_idx_type len = idx.length ();

  if (len == 0)
    retval = dims.numel ();
  else
    {
      const dim_vector dv = dims.redim (len);
      retval = 1;
      for (octave_idx_type i = 0; i < len; i++)
        {
          octave_value idxi = idx(i);
          if (idxi.is_magic_colon ())
            retval *= dv(i);
          else if (idxi.is_numeric_type ())
            retval *= idxi.numel ();
          else
            {
              idx_vector jdx = idxi.index_vector ();
              if (error_state)
                break;
              retval *= jdx.length (dv(i));
            }
        }
    }

  return retval;
}

void
decode_subscripts (const char* name, const octave_value& arg,
                   std::string& type_string,
                   std::list<octave_value_list>& idx)
{
  Octave_map m = arg.map_value ();

  if (! error_state
      && m.nfields () == 2 && m.contains ("type") && m.contains ("subs"))
    {
      Cell& type = m.contents ("type");
      Cell& subs = m.contents ("subs");

      type_string = std::string (type.length(), '\0');

      for (int k = 0; k < type.length (); k++)
        {
          std::string item = type(k).string_value ();

          if (! error_state)
            {
              if (item == "{}")
                type_string[k] = '{';
              else if (item == "()")
                type_string[k] = '(';
              else if (item == ".")
                type_string[k] = '.';
              else
                {
                  error("%s: invalid indexing type `%s'", name, item.c_str ());
                  return;
                }
            }
          else
            {
              error ("%s: expecting type(%d) to be a character string",
                     name, k+1);
              return;
            }

          octave_value_list idx_item;

          if (subs(k).is_string ())
            idx_item(0) = subs(k);
          else if (subs(k).is_cell ())
            {
              Cell subs_cell = subs(k).cell_value ();

              for (int n = 0; n < subs_cell.length (); n++)
                {
                  if (subs_cell(n).is_string ()
                      && subs_cell(n).string_value () == ":")
                    idx_item(n) = octave_value(octave_value::magic_colon_t);
                  else
                    idx_item(n) = subs_cell(n);
                }
            }
          else
            {
              error ("%s: expecting subs(%d) to be a character string or cell array",
                     name, k+1);
              return;
            }

          idx.push_back (idx_item);
        }
    }
  else
    error ("%s: second argument must be a structure with fields `type' and `subs'", name);
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

FloatMatrix
float_identity_matrix (octave_idx_type nr, octave_idx_type nc)
{
  FloatMatrix m (nr, nc, 0.0);

  if (nr > 0 && nc > 0)
    {
      octave_idx_type n = std::min (nr, nc);

      for (octave_idx_type i = 0; i < n; i++)
        m (i, i) = 1.0;
    }

  return m;
}

int
octave_format (std::ostream& os, const char *fmt, ...)
{
  int retval = -1;

  va_list args;
  va_start (args, fmt);

  retval = octave_vformat (os, fmt, args);

  va_end (args);

  return retval;
}

int
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
  octave_rethrow_exception (); \
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

  int nchars = 0;

  if (! buf)
    buf = new char [size];

  if (! buf)
    return 0;

  while (1)
    {
      va_list saved_args;

      SAVE_ARGS (saved_args, args);

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE_FOR_VSNPRINTF;

      nchars = octave_raw_vsnprintf (buf, size, fmt, saved_args);

      va_end (saved_args);

      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      if (nchars > -1 && nchars < size)
        break;
      else
        {
          delete [] buf;

          size = nchars + 1;;

          buf = new char [size];

          if (! buf)
            return 0;
        }
    }

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

      // Versions of these functions that accept unsigned int args are
      // defined in cutils.c.
      octave_sleep (sec);
      octave_usleep (usec);

      octave_quit ();
    }
}

DEFUN (isindex, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isindex (@var{ind}, @var{n})\n\
Returns true if @var{ind} is a valid index.  Valid indices can be\n\
either positive integers (though possibly real data), or logical arrays.\n\
If present, @var{n} specifies the extent of the dimension to be indexed.\n\
Note that, if possible, the internal conversion result is cached so that\n\
subsequent indexing will not perform the checking again.\n\
@end deftypefn")
{
  octave_value retval;
  int nargin = args.length ();
  octave_idx_type n = 0;

  if (nargin == 2)
    n = args(1).idx_type_value ();
  else if (nargin != 1)
    print_usage ();

  if (! error_state)
    {
      unwind_protect frame;
      frame.protect_var (error_state);
      frame.protect_var (discard_error_messages);
      discard_error_messages = true;

      try
        {
          idx_vector idx = args(0).index_vector ();
          if (! error_state)
            {
              if (nargin == 2)
                retval = idx.extent (n) <= n;
              else
                retval = true;
            }
          else
            retval = false;
        }
      catch (octave_execution_exception)
        {
          retval = false;
        }
    }

  return retval;
}

octave_value_list
do_simple_cellfun (octave_value_list (*fun) (const octave_value_list&, int),
                   const char *fun_name, const octave_value_list& args, 
                   int nargout)
{
  octave_value_list new_args = args, retval;
  int nargin = args.length ();
  OCTAVE_LOCAL_BUFFER (bool, iscell, nargin);
  OCTAVE_LOCAL_BUFFER (Cell, cells, nargin);
  OCTAVE_LOCAL_BUFFER (Cell, rcells, nargout);

  const Cell *ccells = cells;

  octave_idx_type numel = 1;
  dim_vector dims (1, 1);

  for (int i = 0; i < nargin; i++)
    {
      octave_value arg = new_args(i);
      iscell[i] = arg.is_cell ();
      if (iscell[i])
        {
          cells[i] = arg.cell_value ();
          octave_idx_type n = ccells[i].numel ();
          if (n == 1)
            { 
              iscell[i] = false;
              new_args(i) = ccells[i](0);
            }
          else if (numel == 1)
            {
              numel = n;
              dims = ccells[i].dims ();
            } 
          else if (dims != ccells[i].dims ())
            {
              error ("%s: cell arguments must have matching sizes", fun_name);
              break;
            }
        }
    }

  if (! error_state)
    {
      for (int i = 0; i < nargout; i++)
        rcells[i].clear (dims);

      for (octave_idx_type j = 0; j < numel; j++)
        {
          for (int i = 0; i < nargin; i++)
            if (iscell[i])
              new_args(i) = ccells[i](j);

          octave_quit ();

          const octave_value_list tmp = fun (new_args, nargout);

          if (tmp.length () < nargout)
            {
              error ("%s: do_simple_cellfun: internal error", fun_name);
              break;
            }
          else
            {
              for (int i = 0; i < nargout; i++)
                rcells[i](j) = tmp(i);
            }
        }
    }

  if (! error_state)
    {
      retval.resize (nargout);
      for (int i = 0; i < nargout; i++)
        retval(i) = rcells[i];
    }

  return retval;
}

octave_value
do_simple_cellfun (octave_value_list (*fun) (const octave_value_list&, int),
                   const char *fun_name, const octave_value_list& args)
{
  octave_value retval;
  const octave_value_list tmp = do_simple_cellfun (fun, fun_name, args, 1);
  if (tmp.length () > 0)
    retval = tmp(0);

  return retval;
}
