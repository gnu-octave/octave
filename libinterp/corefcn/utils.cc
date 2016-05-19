/*

Copyright (C) 1993-2015 John W. Eaton
Copyright (C) 2010 VZLU Prague

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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cerrno>
#include <cstring>

#include <fstream>
#include <iostream>
#include <limits>
#include <string>

#include <sys/types.h>
#include <unistd.h>

#include "vasnprintf.h"

#include "dir-ops.h"
#include "file-ops.h"
#include "file-stat.h"
#include "lo-mappers.h"
#include "lo-utils.h"
#include "oct-cmplx.h"
#include "oct-env.h"
#include "oct-locbuf.h"
#include "pathsearch.h"
#include "quit.h"
#include "str-vec.h"

#include "Cell.h"
#include <defaults.h>
#include "defun.h"
#include "dirfns.h"
#include "error.h"
#include "errwarn.h"
#include "input.h"
#include "lex.h"
#include "load-path.h"
#include "oct-errno.h"
#include "oct-hist.h"
#include "ovl.h"
#include "ov-range.h"
#include "pager.h"
#include "parse.h"
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
@deftypefn {} {} isvarname (@var{name})\n\
Return true if @var{name} is a valid variable name.\n\
@seealso{iskeyword, exist, who}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  octave_value retval = false;

  if (args(0).is_string ())
    {
      std::string varname = args(0).string_value ();

      retval = valid_identifier (varname) && ! is_keyword (varname);
    }

  return retval;
}

/*
%!assert (isvarname ("foo"), true)
%!assert (isvarname ("_foo"), true)
%!assert (isvarname ("_1"), true)
%!assert (isvarname ("1foo"), false)
%!assert (isvarname (""), false)
%!assert (isvarname (12), false)
%!assert (isvarname ("foo+bar"), false)

%!error isvarname ()
%!error isvarname ("foo", "bar")
*/

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
keyword_almost_match (const char * const *std, int *min_len,
                      const std::string& s,
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

// Return nonzero if either NR or NC is zero.
// Return -1 if this should be considered fatal; return 1 if this is ok.

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

  return octave::sys::env::make_absolute (p.find_first_of (names));
}

// Find all locations of the given file in the path.

string_vector
search_path_for_all_files (const std::string& path, const string_vector& names)
{
  dir_path p (path);

  string_vector sv = p.find_all_first_of (names);

  octave_idx_type len = sv.numel ();

  for (octave_idx_type i = 0; i < len; i++)
    sv[i] = octave::sys::env::make_absolute (sv[i]);

  return sv;
}

static string_vector
make_absolute (const string_vector& sv)
{
  octave_idx_type len = sv.numel ();

  string_vector retval (len);

  for (octave_idx_type i = 0; i < len; i++)
    retval[i] = octave::sys::env::make_absolute (sv[i]);

  return retval;
}

DEFUN (file_in_loadpath, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {} {} file_in_loadpath (@var{file})\n\
@deftypefnx {} {} file_in_loadpath (@var{file}, \"all\")\n\
\n\
Return the absolute name of @var{file} if it can be found in\n\
the list of directories specified by @code{path}.\n\
\n\
If no file is found, return an empty character string.\n\
\n\
If the first argument is a cell array of strings, search each directory of\n\
the loadpath for element of the cell array and return the first that\n\
matches.\n\
\n\
If the second optional argument @qcode{\"all\"} is supplied, return a cell\n\
array containing the list of all files that have the same name in the path.\n\
If no files are found, return an empty cell array.\n\
@seealso{file_in_path, dir_in_loadpath, path}\n\
@end deftypefn")
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  string_vector names = args(0).xstring_vector_value ("file_in_loadpath: FILE argument must be a string");

  if (names.empty ())
    error ("file_in_loadpath: FILE argument must not be empty");

  if (nargin == 1)
    return ovl (octave::sys::env::make_absolute (load_path::find_first_of (names)));
  else
    {
      std::string opt = args(1).xstring_value ("file_in_loadpath: optional second argument must be a string");

      if (opt != "all")
        error ("file_in_loadpath: \"all\" is only valid second argument");

      return ovl (Cell (make_absolute (load_path::find_all_first_of (names))));
    }
}

/*
%!test
%! f = file_in_loadpath ("plot.m");
%! assert (ischar (f));
%! assert (! isempty (f));

%!test
%! f = file_in_loadpath ("$$probably_!! _not_&&_a_!! _file$$");
%! assert (f, "");

%!test
%! lst = file_in_loadpath ("$$probably_!! _not_&&_a_!! _file$$", "all");
%! assert (lst, {});

%!error file_in_loadpath ()
%!error file_in_loadpath ("foo", "bar", 1)
%!error file_in_loadpath ([])
%!error file_in_loadpath ("plot.m", "bar")
*/

DEFUN (file_in_path, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {} {} file_in_path (@var{path}, @var{file})\n\
@deftypefnx {} {} file_in_path (@var{path}, @var{file}, \"all\")\n\
Return the absolute name of @var{file} if it can be found in @var{path}.\n\
\n\
The value of @var{path} should be a colon-separated list of directories in\n\
the format described for @code{path}.  If no file is found, return an empty\n\
character string.  For example:\n\
\n\
@example\n\
@group\n\
file_in_path (EXEC_PATH, \"sh\")\n\
     @result{} \"/bin/sh\"\n\
@end group\n\
@end example\n\
\n\
If the second argument is a cell array of strings, search each directory of\n\
the path for element of the cell array and return the first that matches.\n\
\n\
If the third optional argument @qcode{\"all\"} is supplied, return a cell\n\
array containing the list of all files that have the same name in the path.\n\
If no files are found, return an empty cell array.\n\
@seealso{file_in_loadpath, dir_in_loadpath, path}\n\
@end deftypefn")
{
  int nargin = args.length ();

  if (nargin < 2 || nargin > 3)
    print_usage ();

  std::string path = args(0).xstring_value ("file_in_path: PATH must be a string");

  string_vector names = args(1).xstring_vector_value ("file_in_path: FILE argument must be a string");

  if (names.empty ())
    error ("file_in_path: FILE argument must not be empty");

  if (nargin == 2)
    return ovl (search_path_for_file (path, names));
  else
    {
      std::string opt = args(2).xstring_value ("file_in_path: optional third argument must be a string");

      if (opt != "all")
        error ("file_in_path: \"all\" is only valid third argument");

      return ovl (Cell (make_absolute (search_path_for_all_files (path, names))));
    }
}

/*
%!test
%! f = file_in_path (path (), "plot.m");
%! assert (ischar (f));
%! assert (! isempty (f));

%!test
%! f = file_in_path (path (), "$$probably_!! _not_&&_a_!! _file$$");
%! assert (f, "");

%!test
%! lst = file_in_path (path (), "$$probably_!! _not_&&_a_!! _file$$", "all");
%! assert (lst, {});

%!error file_in_path ()
%!error file_in_path ("foo")
%!error file_in_path ("foo", "bar", "baz", 1)
%!error file_in_path ([])
%!error file_in_path (path (), [])
%!error file_in_path (path (), "plot.m", "bar")
*/

std::string
file_in_path (const std::string& name, const std::string& suffix)
{
  std::string nm = name;

  if (! suffix.empty ())
    nm.append (suffix);

  return octave::sys::env::make_absolute (load_path::find_file (nm));
}

std::string
find_data_file_in_load_path  (const std::string& fcn,
                              const std::string& file,
                              bool require_regular_file)
{
  std::string fname = file;

  if (! (octave::sys::env::absolute_pathname (fname)
         || octave::sys::env::rooted_relative_pathname (fname)))
    {
      // Load path will also search "." first, but we don't want to
      // issue a warning if the file is found in the current directory,
      // so do an explicit check for that.
      file_stat fs (fname);

      bool local_file_ok
        = fs.exists () && (fs.is_reg () || ! require_regular_file);

      if (! local_file_ok)
        {
          // Not directly found; search load path.
          std::string tmp
            = octave::sys::env::make_absolute (load_path::find_file (fname));

          if (! tmp.empty ())
            {
              warn_data_file_in_path (fcn, tmp);

              fname = tmp;
            }
        }
    }

  return fname;
}

// See if there is an function file in the path.
// If so, return the full path to the file.

std::string
fcn_file_in_path (const std::string& name)
{
  std::string retval;

  int len = name.length ();

  if (len > 0)
    {
      if (octave::sys::env::absolute_pathname (name))
        {
          file_stat fs (name);

          if (fs.exists () && ! fs.is_dir ())
            retval = name;
        }
      else if (len > 2 && name[len - 2] == '.' && name[len - 1] == 'm')
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
// contains a Contents.m file.  If so, return the full path to this file.

std::string
contents_file_in_path (const std::string& dir)
{
  std::string retval;

  if (dir.length () > 0)
    {
      std::string tcontents = octave::sys::file_ops::concat (load_path::find_dir (dir),
                                                std::string ("Contents.m"));

      file_stat fs (tcontents);

      if (fs.exists ())
        retval = octave::sys::env::make_absolute (tcontents);
    }

  return retval;
}

// Deprecated in 4.2, remove in 4.6.
// See if there is a .oct file in the path.
// If so, return the full path to the file.

std::string
oct_file_in_path (const std::string& name)
{
  std::string retval;

  int len = name.length ();

  if (len > 0)
    {
      if (octave::sys::env::absolute_pathname (name))
        {
          file_stat fs (name);

          if (fs.exists ())
            retval = name;
        }
      else if (len > 4 && name.find (".oct", len-5))
        retval = load_path::find_oct_file (name.substr (0, len-4));
      else
        retval = load_path::find_oct_file (name);
    }

  return retval;
}

// Deprecated in 4.2, remove in 4.6.
// See if there is a .mex file in the path.
// If so, return the full path to the file.

std::string
mex_file_in_path (const std::string& name)
{
  std::string retval;

  int len = name.length ();

  if (len > 0)
    {
      if (octave::sys::env::absolute_pathname (name))
        {
          file_stat fs (name);

          if (fs.exists ())
            retval = name;
        }
      else if (len > 4 && name.find (".mex", len-5))
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
            case 'a': // alarm
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

            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7': // octal input
            {
              size_t k;
              int tmpi = s[j] - '0';
              for (k = j+1; k < std::min (j+3, len); k++)
                {
                  int digit = s[k] - '0';
                  if (digit < 0 || digit > 7)
                    break;
                  tmpi <<= 3;
                  tmpi += digit;
                }
              retval[i] = tmpi;
              j = k - 1;
              break;
            }

            case 'x': // hex input
            {
              size_t k;
              int tmpi = 0;
              for (k = j+1; k < std::min (j+3, len); k++)
                {
                  if (! isxdigit (s[k]))
                    break;

                  tmpi <<= 4;
                  int digit = s[k];
                  if (digit >= 'a')
                    tmpi += digit - 'a' + 10;
                  else if (digit >= 'A')
                    tmpi += digit - 'A' + 10;
                  else
                    tmpi += digit - '0';
                }

              if (k == j+1)
                warning ("malformed hex escape sequence '\\x' --\
 converting to '\\0'");

              retval[i] = tmpi;
              j = k - 1;
              break;
            }

            default:
              warning ("unrecognized escape sequence '\\%c' --\
 converting to '%c'", s[j], s[j]);
              retval[i] = s[j];
              break;
            }
        }
      else
        retval[i] = s[j];

      i++;
      j++;
    }

  retval.resize (i);

  return retval;
}

DEFUN (do_string_escapes, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {} do_string_escapes (@var{string})\n\
Convert escape sequences in @var{string} to the characters they represent.\n\
\n\
Escape sequences begin with a leading backslash\n\
(@qcode{'@xbackslashchar{}'}) followed by 1--3 characters\n\
(.e.g., @qcode{\"@xbackslashchar{}n\"} => newline).\n\
@seealso{undo_string_escapes}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  std::string str = args(0).xstring_value ("do_string_escapes: STRING argument must be of type string");

  return ovl (do_string_escapes (str));
}

/*
%!assert (do_string_escapes ('foo\nbar'), "foo\nbar")
%!assert (do_string_escapes ("foo\\nbar"), "foo\nbar")
%!assert (do_string_escapes ("foo\\nbar"), ["foo", char(10), "bar"])
%!assert ("foo\nbar", ["foo", char(10), "bar"])

%!assert (do_string_escapes ('\0\a\b\f\n\r\t\v'), "\0\a\b\f\n\r\t\v")
%!assert (do_string_escapes ("\\0\\a\\b\\f\\n\\r\\t\\v"), "\0\a\b\f\n\r\t\v")
%!assert (do_string_escapes ("\\0\\a\\b\\f\\n\\r\\t\\v"),
%!        char ([0, 7, 8, 12, 10, 13, 9, 11]))
%!assert ("\0\a\b\f\n\r\t\v", char ([0, 7, 8, 12, 10, 13, 9, 11]))

%!assert (do_string_escapes ('\\'), "\\")
%!assert (do_string_escapes ("\\\\"), "\\")
%!assert (do_string_escapes ("\\\\"), char (92))

%!assert (do_string_escapes ('\''single-quoted\'''), "'single-quoted'")
%!assert (do_string_escapes ("\\'single-quoted\\'"), "'single-quoted'")
%!assert (do_string_escapes ('\"double-quoted\"'), "\"double-quoted\"")
%!assert (do_string_escapes ("\\\"double-quoted\\\""), "\"double-quoted\"")

%!assert (do_string_escapes ('A\4B'), ["A" char(4) "B"])
%!assert (do_string_escapes ('A\45B'), ["A" char(37) "B"])
%!assert (do_string_escapes ('A\123B'), ["A" char(83) "B"])
%!assert (sprintf ('\117\143\164\141\166\145'), "Octave")

%!assert (do_string_escapes ('A\x4G'), ["A" char(4) "G"])
%!assert (do_string_escapes ('A\x4AG'), ["A" char(74) "G"])
%!assert (sprintf ('\x4f\x63\x74\x61\x76\x65'), "Octave")

%!error do_string_escapes ()
%!error do_string_escapes ("foo", "bar")
%!error <STRING argument> do_string_escapes (3)
%!warning <malformed hex escape sequence> do_string_escapes ('\xG');
%!warning <unrecognized escape sequence> do_string_escapes ('\G');
*/

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
        static char retval[2] = "\0";

        retval[0] = c;
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
@deftypefn {} {} undo_string_escapes (@var{s})\n\
Convert special characters in strings back to their escaped forms.\n\
\n\
For example, the expression\n\
\n\
@example\n\
bell = \"\\a\";\n\
@end example\n\
\n\
@noindent\n\
assigns the value of the alert character (control-g, ASCII code 7) to the\n\
string variable @code{bell}.  If this string is printed, the system will\n\
ring the terminal bell (if it is possible).  This is normally the desired\n\
outcome.  However, sometimes it is useful to be able to print the original\n\
representation of the string, with the special characters replaced by their\n\
escape sequences.  For example,\n\
\n\
@example\n\
@group\n\
octave:13> undo_string_escapes (bell)\n\
ans = \\a\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
replaces the unprintable alert character with its printable representation.\n\
@seealso{do_string_escapes}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  std::string str = args(0).xstring_value ("undo_string_escapes: S argument must be a string");

  return ovl (undo_string_escapes (str));
}

/*
%!assert (undo_string_escapes ("foo\nbar"), 'foo\nbar')
%!assert (undo_string_escapes ("foo\nbar"), "foo\\nbar")
%!assert (undo_string_escapes (["foo", char(10), "bar"]), "foo\\nbar")

%!assert (undo_string_escapes ("\a\b\f\n\r\t\v"), '\a\b\f\n\r\t\v')
%!assert (undo_string_escapes ("\a\b\f\n\r\t\v"), "\\a\\b\\f\\n\\r\\t\\v")
%!assert (undo_string_escapes (char ([7, 8, 12, 10, 13, 9, 11])),
%!        "\\a\\b\\f\\n\\r\\t\\v")

%!assert (undo_string_escapes ("\\"), '\\')
%!assert (undo_string_escapes ("\\"), "\\\\")
%!assert (undo_string_escapes (char (92)), "\\\\")

%!assert (undo_string_escapes ("\"double-quoted\""), '\"double-quoted\"')
%!assert (undo_string_escapes ("\"double-quoted\""), "\\\"double-quoted\\\"")

%!error undo_string_escapes ()
%!error undo_string_escapes ("foo", "bar")
%!error undo_string_escapes (3)
*/

DEFUN (is_absolute_filename, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {} is_absolute_filename (@var{file})\n\
Return true if @var{file} is an absolute filename.\n\
@seealso{is_rooted_relative_filename, make_absolute_filename, isdir}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).is_string ()
              && octave::sys::env::absolute_pathname (args(0).string_value ()));
}

/*
## FIXME: We need system-dependent tests here.

%!error is_absolute_filename ()
%!error is_absolute_filename ("foo", "bar")
*/

DEFUN (is_rooted_relative_filename, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {} is_rooted_relative_filename (@var{file})\n\
Return true if @var{file} is a rooted-relative filename.\n\
@seealso{is_absolute_filename, make_absolute_filename, isdir}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).is_string ()
              && octave::sys::env::rooted_relative_pathname (args(0).string_value ()));
}

/*
## FIXME: We need system-dependent tests here.

%!error is_rooted_relative_filename ()
%!error is_rooted_relative_filename ("foo", "bar")
*/

DEFUN (make_absolute_filename, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {} make_absolute_filename (@var{file})\n\
Return the full name of @var{file} beginning from the root of the file\n\
system.\n\
\n\
No check is done for the existence of @var{file}.\n\
@seealso{canonicalize_file_name, is_absolute_filename, is_rooted_relative_filename, isdir}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  std::string nm = args(0).xstring_value ("make_absolute_filename: FILE argument must be a filename");

  return ovl (octave::sys::env::make_absolute (nm));
}

/*
## FIXME: We need system-dependent tests here.

%!error make_absolute_filename ()
%!error make_absolute_filename ("foo", "bar")
*/

DEFUN (dir_in_loadpath, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {} {} dir_in_loadpath (@var{dir})\n\
@deftypefnx {} {} dir_in_loadpath (@var{dir}, \"all\")\n\
Return the full name of the path element matching @var{dir}.\n\
\n\
The match is performed at the end of each path element.  For example, if\n\
@var{dir} is @qcode{\"foo/bar\"}, it matches the path element\n\
@nospell{@qcode{\"/some/dir/foo/bar\"}}, but not\n\
@nospell{@qcode{\"/some/dir/foo/bar/baz\"}}\n\
@nospell{@qcode{\"/some/dir/allfoo/bar\"}}.\n\
\n\
If the optional second argument is supplied, return a cell array containing\n\
all name matches rather than just the first.\n\
@seealso{file_in_path, file_in_loadpath, path}\n\
@end deftypefn")
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  std::string dir;

  dir = args(0).xstring_value ("dir_in_loadpath: DIR must be a directory name");

  if (nargin == 1)
    return ovl (load_path::find_dir (dir));
  else
    return ovl (Cell (load_path::find_matching_dirs (dir)));
}

/*
%!test
%! f = dir_in_loadpath ("plot");
%! assert (ischar (f));
%! assert (! isempty (f));

%!test
%! f = dir_in_loadpath ("$$probably_!! _not_&&_a_!! _dir$$");
%! assert (f, "");

%!test
%! lst = dir_in_loadpath ("$$probably_!! _not_&&_a_!! _dir$$", "all");
%! assert (lst, {});

%!error dir_in_loadpath ()
%!error dir_in_loadpath ("foo", "bar", 1)
*/

DEFUNX ("errno", Ferrno, args, ,
        "-*- texinfo -*-\n\
@deftypefn  {} {@var{err} =} errno ()\n\
@deftypefnx {} {@var{err} =} errno (@var{val})\n\
@deftypefnx {} {@var{err} =} errno (@var{name})\n\
Return the current value of the system-dependent variable errno,\n\
set its value to @var{val} and return the previous value, or return\n\
the named error code given @var{name} as a character string, or -1\n\
if @var{name} is not found.\n\
@seealso{errno_list}\n\
@end deftypefn")
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  octave_value retval;

  if (nargin == 1)
    {
      if (args(0).is_string ())
        {
          std::string nm = args(0).string_value ();

          retval = octave_errno::lookup (nm);
        }
      else
        {
          int val = args(0).xint_value ("errno: argument must be string or integer");

          retval = octave_errno::set (val);
        }
    }
  else
    retval = octave_errno::get ();

  return retval;
}

/*
%!assert (isnumeric (errno ()))

%!test
%! lst = errno_list ();
%! fns = fieldnames (lst);
%! oldval = errno (fns{1});
%! assert (isnumeric (oldval));
%! errno (oldval);
%! newval = errno ();
%! assert (oldval, newval);

%!error errno ("foo", 1)
*/

DEFUN (errno_list, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {} errno_list ()\n\
Return a structure containing the system-dependent errno values.\n\
@seealso{errno}\n\
@end deftypefn")
{
  if (args.length () != 0)
    print_usage ();

  return ovl (octave_errno::list ());
}

/*
%!assert (isstruct (errno_list ()))

%!error errno_list ("foo")
*/

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

  for (int i = 0; i < dim.ndims (); i++)
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
  // We support dimensions to be specified by any vector, even if it's a
  // vector of dimensions 0x1, 1x0, 1x1x0, or 1x1x6.  If the vector ends
  // up being empty, the final dimensions end up being 0x0.
  if (! a.dims ().is_vector ())
    error ("%s (A): use %s (size (A)) instead", warn_for, warn_for);

  const Array<int> v = a.int_vector_value ();
  const octave_idx_type n = v.numel ();

  dim.resize (n); // even if n < 2, resize sets it back to 2
  if (n == 0)
    {
      dim(0) = 0;
      dim(1) = 0;
    }
  else if (n == 1)
    {
      dim(0) = v(0);
      dim(1) = v(0);
    }
  else
    for (octave_idx_type i = 0; i < n; i++)
      dim(i) = v(i);

  check_dimensions (dim, warn_for);
}


void
get_dimensions (const octave_value& a, const char *warn_for,
                octave_idx_type& nr, octave_idx_type& nc)
{
  if (a.is_scalar_type ())
    {
      nr = nc = a.idx_type_value ();
    }
  else
    {
      nr = a.rows ();
      nc = a.columns ();

      if ((nr != 1 || nc != 2) && (nr != 2 || nc != 1))
        error ("%s (A): use %s (size (A)) instead", warn_for, warn_for);

      Array<double> v = a.vector_value ();
      nr = static_cast<octave_idx_type> (fix (v(0)));
      nc = static_cast<octave_idx_type> (fix (v(1)));
    }

  check_dimensions (nr, nc, warn_for);
}

void
get_dimensions (const octave_value& a, const octave_value& b,
                const char *warn_for, octave_idx_type& nr, octave_idx_type& nc)
{
  nr = a.is_empty ()
       ? 0 : a.idx_type_value ("%s: row dimension must be a scalar", warn_for);
  nc = b.is_empty ()
       ? 0 : b.idx_type_value ("%s: column dimension must be a scalar", warn_for);

  check_dimensions (nr, nc, warn_for);
}

octave_idx_type
dims_to_numel (const dim_vector& dims, const octave_value_list& idx_arg)
{
  octave_idx_type retval;

  octave_idx_type len = idx_arg.length ();

  if (len == 0)
    retval = dims.numel ();
  else
    {
      const dim_vector dv = dims.redim (len);
      retval = 1;
      for (octave_idx_type i = 0; i < len; i++)
        {
          octave_value idxi = idx_arg(i);
          if (idxi.is_magic_colon ())
            retval *= dv(i);
          else if (idxi.is_numeric_type ())
            retval *= idxi.numel ();
          else
            {
              try
                {
                  idx_vector jdx = idxi.index_vector ();

                  retval *= jdx.length (dv(i));
                }
              catch (const index_exception& e)
                {
                  std::string idx = e.idx ();
                  std::string msg = e.details ();

                  error ("dims_to_numel: Invalid IDX %s. %s",
                         idx.c_str (), msg.c_str ());
                }
            }
        }
    }

  return retval;
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

size_t
octave_format (std::ostream& os, const char *fmt, ...)
{
  size_t retval;

  va_list args;
  va_start (args, fmt);

  retval = octave_vformat (os, fmt, args);

  va_end (args);

  return retval;
}

size_t
octave_vformat (std::ostream& os, const char *fmt, va_list args)
{
  std::string s = octave_vasprintf (fmt, args);

  os << s;

  return s.length ();
}

std::string
octave_vasprintf (const char *fmt, va_list args)
{
  std::string retval;

  char *result;

  int status = gnulib::vasprintf (&result, fmt, args);

  if (status >= 0)
    {
      retval = result;
      ::free (result);
    }

  return retval;
}

std::string
octave_asprintf (const char *fmt, ...)
{
  std::string retval;

  va_list args;
  va_start (args, fmt);

  retval = octave_vasprintf (fmt, args);

  va_end (args);

  return retval;
}

void
octave_sleep (double seconds)
{
  if (seconds <= 0)
    return;

  double fraction = std::modf (seconds, &seconds);
  fraction = gnulib::floor (fraction * 1000000000); // nanoseconds

  time_t sec = ((seconds > std::numeric_limits<time_t>::max ())
                ? std::numeric_limits<time_t>::max ()
                : static_cast<time_t> (seconds));

  struct timespec delay = { sec, static_cast<long> (fraction) };
  struct timespec remaining;
  gnulib::nanosleep (&delay, &remaining);

  octave_quit ();
}

DEFUN (isindex, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {} {} isindex (@var{ind})\n\
@deftypefnx {} {} isindex (@var{ind}, @var{n})\n\
Return true if @var{ind} is a valid index.\n\
\n\
Valid indices are either positive integers (although possibly of real data\n\
type), or logical arrays.\n\
\n\
If present, @var{n} specifies the maximum extent of the dimension to be\n\
indexed.  When possible the internal result is cached so that subsequent\n\
indexing using @var{ind} will not perform the check again.\n\
\n\
Implementation Note: Strings are first converted to double values before the\n\
checks for valid indices are made.  Unless a string contains the NULL\n\
character @nospell{\"@xbackslashchar{}0\"}, it will always be a valid index.\n\
@end deftypefn")
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  octave_idx_type n = 0;
  if (nargin == 2)
    n = args(1).idx_type_value ();

  octave_value retval;

  unwind_protect frame;

  frame.protect_var (discard_error_messages);
  discard_error_messages = true;

  try
    {
      idx_vector idx = args(0).index_vector (true);

      if (nargin == 2)
        retval = idx.extent (n) <= n;
      else
        retval = true;
    }
  catch (const octave_execution_exception&)
    {
      recover_from_exception ();

      retval = false;
    }

  return retval;
}

/*
%!assert (isindex ([1, 2, 3]))
%!assert (isindex (1:3))
%!assert (isindex (1:3, 2), false)
%!assert (isindex ([1, 2, -3]), false)

%!error isindex ()
%!error isindex (1:3, 2, 3)
*/

octave_value_list
do_simple_cellfun (octave_value_list (*fun) (const octave_value_list&, int),
                   const char *fun_name, const octave_value_list& args,
                   int nargout)
{
  octave_value_list new_args = args;
  octave_value_list retval;
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
            error ("%s: cell arguments must have matching sizes", fun_name);
        }
    }

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
        error ("%s: do_simple_cellfun: internal error", fun_name);

      for (int i = 0; i < nargout; i++)
        rcells[i](j) = tmp(i);
    }

  retval.resize (nargout);

  for (int i = 0; i < nargout; i++)
    retval(i) = rcells[i];

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

DEFUN (isstudent, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {} isstudent ()\n\
Return true if running in the student edition of @sc{matlab}.\n\
\n\
@code{isstudent} always returns false in Octave.\n\
@seealso{false}\n\
@end deftypefn")
{
  if (args.length () != 0)
    print_usage ();

  return ovl (false);
}

/*
%!assert (isstudent (), false)

%!error isstudent (1)
*/
