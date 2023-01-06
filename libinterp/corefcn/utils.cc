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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cerrno>
#include <cstring>

#include <fstream>
#include <limits>
#include <ostream>
#include <string>

#include "dir-ops.h"
#include "file-ops.h"
#include "file-stat.h"
#include "lo-mappers.h"
#include "lo-utils.h"
#include "nanosleep-wrapper.h"
#include "oct-cmplx.h"
#include "oct-env.h"
#include "oct-locbuf.h"
#include "oct-string.h"
#include "pathsearch.h"
#include "quit.h"
#include "str-vec.h"
#include "strcase-wrappers.h"
#include "vasprintf-wrapper.h"

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "graphics.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "lex.h"
#include "load-path.h"
#include "oct-errno.h"
#include "oct-hist.h"
#include "ovl.h"
#include "ov-range.h"
#include "pager.h"
#include "parse.h"
#include "sysdep.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Return TRUE if S is a valid identifier.

bool valid_identifier (const char *s)
{
  if (! s || ! (isalpha (*s) || *s == '_'))
    return false;

  while (*++s != '\0')
    if (! (isalnum (*s) || *s == '_'))
      return false;

  return true;
}

bool valid_identifier (const std::string& s)
{
  return valid_identifier (s.c_str ());
}

DEFUN (isvarname, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isvarname (@var{name})
Return true if @var{name} is a valid variable name.

A valid variable name is composed of letters, digits, and underscores ("_"),
and the first character must not be a digit.
@seealso{iskeyword, exist, who}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  octave_value retval = false;

  if (args(0).is_string ())
    {
      std::string varname = args(0).string_value ();

      retval = (valid_identifier (varname)
                && ! iskeyword (varname));
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

bool
make_valid_name (std::string& str, const make_valid_name_options& options)
{
  // If `isvarname (str)`, no modifications necessary.
  if (valid_identifier (str) && ! iskeyword (str))
    return false;

  // Change whitespace followed by lowercase letter to uppercase, except
  // for the first
  bool previous = false;
  bool any_non_space = false;
  for (char& c : str)
    {
      c = ((any_non_space && previous && std::isalpha (c)) ? std::toupper (c)
           : c);
      previous = std::isspace (c);
      any_non_space |= (! previous);  // once true, always true
    }

  // Remove any whitespace.
  str.erase (std::remove_if (str.begin(), str.end(),
                             [] (unsigned char x)
  { return std::isspace(x); }),
  str.end());
  if (str.empty ())
    str = options.get_prefix ();

  // Add prefix and capitalize first character, if `str` is a reserved
  // keyword.
  if (iskeyword (str))
    {
      str[0] = std::toupper (str[0]);
      str = options.get_prefix () + str;
    }

  // Add prefix if first character is not a letter or underscore.
  if (! std::isalpha (str[0]) && str[0] != '_')
    str = options.get_prefix () + str;

  // Replace non alphanumerics or underscores
  if (options.get_replacement_style () == "underscore")
    for (char& c : str)
      c = (std::isalnum (c) ? c : '_');
  else if (options.get_replacement_style () == "delete")
    str.erase (std::remove_if (str.begin(), str.end(),
                               [] (unsigned char x)
    { return ! std::isalnum (x) && x != '_'; }),
  str.end());
  else if (options.get_replacement_style () == "hex")
    {
      const std::string permitted_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                          "abcdefghijklmnopqrstuvwxyz"
                                          "_0123456789";
      // Get the first non-permitted char.
      std::size_t pos = str.find_first_not_of (permitted_chars);
      // Buffer for hex string "0xFF" (+1 for null terminator).
      char hex_str[5];
      // Repeat until end of string.
      while (pos != std::string::npos)
        {
          // Replace non-permitted char by it's hex value.
          std::snprintf (hex_str, sizeof (hex_str), "0x%02X", str[pos]);
          str.replace (pos, 1, hex_str);
          // Get the next occurrence from the last position.
          // (-1 for null terminator)
          pos = str.find_first_not_of (permitted_chars,
                                       pos + sizeof (hex_str) - 1);
        }
    }

  return true;
}

make_valid_name_options::make_valid_name_options
(const octave_value_list& args)
{
  auto nargs = args.length ();
  if (nargs == 0)
    return;

  // nargs = 2, 4, 6, ... permitted
  if (nargs % 2)
    error ("makeValidName: property/value options must occur in pairs");

  auto str_to_lower = [] (std::string& s)
  {
    std::transform (s.begin(), s.end(), s.begin(),
                    [] (unsigned char c)
    { return std::tolower(c); });
  };

  for (auto i = 0; i < nargs; i = i + 2)
    {
      std::string parameter = args(i).xstring_value ("makeValidName: "
                              "option argument must be a string");
      str_to_lower (parameter);
      if (parameter == "replacementstyle")
        {
          m_replacement_style = args(i + 1).xstring_value ("makeValidName: "
                                "'ReplacementStyle' value must be a string");
          str_to_lower (m_replacement_style);
          if ((m_replacement_style != "underscore")
              && (m_replacement_style != "delete")
              && (m_replacement_style != "hex"))
            error ("makeValidName: invalid 'ReplacementStyle' value '%s'",
                   m_replacement_style.c_str ());
        }
      else if (parameter == "prefix")
        {
          m_prefix = args(i + 1).xstring_value ("makeValidName: "
                                                "'Prefix' value must be a string");
          if (! valid_identifier (m_prefix)
              || iskeyword (m_prefix))
            error ("makeValidName: invalid 'Prefix' value '%s'",
                   m_prefix.c_str ());
        }
      else
        error ("makeValidName: unknown property '%s'", parameter.c_str ());
    }
}

DEFUN (__make_valid_name__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{varname} =} __make_valid_name__ (@var{str})
@deftypefnx {} {@var{varname} =} __make_valid_name__ (@var{str}, @qcode{"ReplacementStyle"})
@deftypefnx {} {@var{varname} =} __make_valid_name__ (@var{str}, @qcode{"ReplacementStyle"}, @qcode{"Prefix"})
@deftypefnx {} {[@var{varname}, @var{ismodified}] =} __make_valid_name__ (@dots{})
Return a valid variable name @var{varname} from input @var{str}.

For more documentation, see @code{matlab.lang.makeValidName}.

@seealso{isvarname, matlab.lang.makeValidName}
@end deftypefn */)
{
  auto nargin = args.length ();
  if (nargin < 1)
    print_usage ();

  make_valid_name_options options (args.slice (1, nargin - 1));

  if (args(0).is_string ())
    {
      std::string varname = args(0).string_value ();
      bool is_modified = make_valid_name (varname, options);
      return ovl (varname, is_modified);
    }
  else if (args(0).iscellstr ())
    {
      Array<std::string> varnames = args(0).cellstr_value ();
      Array<bool> is_modified (varnames.dims ());
      for (auto i = 0; i < varnames.numel (); i++)
        is_modified(i) = make_valid_name (varnames(i), options);
      return ovl (varnames, is_modified);
    }
  else
    error ("makeValidName: STR must be a string or cellstr");
}

// Return TRUE if F and G are both names for the same file.

bool same_file (const std::string& f, const std::string& g)
{
  return same_file_internal (f, g);
}

DEFUN (is_same_file, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{same} =} is_same_file (@var{filepath1}, @var{filepath2})
Return true if @var{filepath1} and @var{filepath2} refer to the same file.

If either @var{filepath1} or @var{filepath2} is a cell array of strings, then
an array of the same size is returned, containing the values described above
for every member of the cell array.  The other argument may also be a cell
array of strings (of the same size) or a string.

Programming Notes: Depending on the operating system and file system, the same
file or folder can be referred to with different paths.  In particular, paths
on the Windows platform may differ in case (@file{file1} vs.@: @file {FILE1}),
file separator (@samp{\} vs.@: @samp{/}), and format (@file{A~spaces.txt} (8.3
convention) vs.@: @file{A filename with spaces.txt}).  This function returns
true if the paths in @var{filepath1} and @var{filepath2} actually refer to the
same file or folder, and false otherwise.

Note that unlike @code{strcmp}, this function requires that @var{filepath1}
and @var{filepath2} exist, as well as point to the same location, in order to
return true.

@seealso{canonicalize_file_name, strcmp}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  octave_value retval;

  bool s1_string = args(0).is_string ();
  bool s1_cellstr = args(0).iscellstr ();
  bool s2_string = args(1).is_string ();
  bool s2_cellstr = args(1).iscellstr ();

  if (s1_string && s2_string)
    {
      std::string file1 = args(0).string_value ();
      std::string file2 = args(1).string_value ();

      retval = same_file (file1, file2);
    }
  else if ((s1_string && s2_cellstr) || (s1_cellstr && s2_string))
    {
      octave_value str_arg, cellstr_arg;

      if (s1_string)
        {
          str_arg = args(0);
          cellstr_arg = args(1);
        }
      else
        {
          str_arg = args(1);
          cellstr_arg = args(0);
        }

      const Array<std::string> cellstr = cellstr_arg.cellstr_value ();
      const std::string str = str_arg.string_value ();

      boolNDArray output (cellstr.dims (), false);

      for (octave_idx_type idx = 0; idx < cellstr.numel (); idx++)
        output(idx) = same_file (str, cellstr(idx));

      retval = output;
    }
  else if (s1_cellstr && s2_cellstr)
    {
      const Array<std::string> cellstr1 = args(0).cellstr_value ();
      const Array<std::string> cellstr2 = args(1).cellstr_value ();

      const dim_vector size1 = cellstr1.dims ();
      const dim_vector size2 = cellstr2.dims ();

      if (size1 != size2)
        error ("is_same_file: cellstr arrays FILEPATH1 and FILEPATH2 must be the same size");

      boolNDArray output (size1, false);

      for (octave_idx_type idx = 0; idx < cellstr1.numel (); idx++)
        output(idx) = same_file (cellstr1(idx), cellstr2(idx));

      retval = output;
    }
  else
    error ("is_same_file: FILEPATH1 and FILEPATH2 must be strings or cell arrays of strings");

  return retval;
}

/*
%!testif ; ! ispc ()
%! assert (is_same_file ("~", tilde_expand ("~")));
%!testif ; ispc ()
%! assert (is_same_file (tolower (tempdir ()), toupper (tempdir ())), true);
%!assert (is_same_file ({pwd(), ".", tempdir()}, canonicalize_file_name (".")),
%!        [true, true, false])

%!error is_same_file ()
%!error is_same_file ("foo")
%!error is_same_file ("foo", "bar", "baz")
%!error <must be strings or cell arrays of strings> is_same_file ("foo", 1)
%!error <must be strings or cell arrays of strings> is_same_file (1, "foo")
%!error <must be strings or cell arrays of strings> is_same_file ("foo", {1})
%!error <must be strings or cell arrays of strings> is_same_file ({1}, "foo")
%!error <arrays .* must be the same size> is_same_file ({"1", "2"}, {"1"; "2"})
*/

int almost_match (const std::string& std, const std::string& s,
                  int min_match_len, int case_sens)
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

int keyword_almost_match (const char *const *std, int *min_len,
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

  const char **to_match = new const char *[max_toks + 1];
  const char *const *s1 = std;
  const char **s2 = to_match;

  if (! s1 || ! s2)
    goto done;

  s2[tok_count] = beg;
  char *end;
  while ((end = strchr (beg, ' ')) != nullptr)
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
  s2[tok_count+1] = nullptr;

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

// See if the given file is in the path.

std::string search_path_for_file (const std::string& path,
                                  const string_vector& names)
{
  directory_path p (path);

  return sys::env::make_absolute (p.find_first_of (names.std_list ()));
}

// Find all locations of the given file in the path.

string_vector search_path_for_all_files (const std::string& path,
    const string_vector& names)
{
  directory_path p (path);

  string_vector sv = p.find_all_first_of (names.std_list ());

  octave_idx_type len = sv.numel ();

  for (octave_idx_type i = 0; i < len; i++)
    sv[i] = sys::env::make_absolute (sv[i]);

  return sv;
}

static string_vector make_absolute (const string_vector& sv)
{
  octave_idx_type len = sv.numel ();

  string_vector retval (len);

  for (octave_idx_type i = 0; i < len; i++)
    retval[i] = sys::env::make_absolute (sv[i]);

  return retval;
}

DEFMETHOD (file_in_loadpath, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{fname} =} file_in_loadpath (@var{file})
@deftypefnx {} {@var{fname} =} file_in_loadpath (@var{file}, "all")
Return the absolute name of @var{file} if it can be found in the list of
directories specified by @code{path}.

If no file is found, return an empty character string.

When @var{file} is already an absolute name, the name is checked against the
file system instead of Octave's loadpath.  In this case, if @var{file} exists
it will be returned in @var{fname}, otherwise an empty string is returned.

If the first argument is a cell array of strings, search each directory of
the loadpath for element of the cell array and return the first that
matches.

If the second optional argument @qcode{"all"} is supplied, return a cell
array containing the list of all files that have the same name in the path.
If no files are found, return an empty cell array.
@seealso{file_in_path, dir_in_loadpath, path}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  string_vector names = args(
                          0).xstring_vector_value ("file_in_loadpath: FILE argument must be a string");

  if (names.empty ())
    error ("file_in_loadpath: FILE argument must not be empty");

  load_path& lp = interp.get_load_path ();

  if (nargin == 1)
    return ovl (sys::env::make_absolute (lp.find_first_of (names)));
  else
    {
      std::string opt = args(
                          1).xstring_value ("file_in_loadpath: optional second argument must be a string");

      if (opt != "all")
        error (R"(file_in_loadpath: "all" is only valid second argument)");

      return ovl (Cell (make_absolute (lp.find_all_first_of (names))));
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
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{fname} =} file_in_path (@var{path}, @var{file})
@deftypefnx {} {@var{fname} =} file_in_path (@var{path}, @var{file}, "all")
Return the absolute name of @var{file} if it can be found in @var{path}.

The value of @var{path} should be a colon-separated list of directories in
the format described for @code{path}.  If no file is found, return an empty
character string.  For example:

@example
@group
file_in_path (EXEC_PATH, "sh")
     @result{} "/bin/sh"
@end group
@end example

If the second argument is a cell array of strings, search each directory of
the path for element of the cell array and return the first that matches.

If the third optional argument @qcode{"all"} is supplied, return a cell
array containing the list of all files that have the same name in the path.
If no files are found, return an empty cell array.
@seealso{file_in_loadpath, dir_in_loadpath, path}
@end deftypefn */)
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
        error (R"(file_in_path: "all" is only valid third argument)");

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

std::string file_in_path (const std::string& name, const std::string& suffix)
{
  std::string nm = name;

  if (! suffix.empty ())
    nm.append (suffix);

  load_path& lp = __get_load_path__ ();

  return sys::env::make_absolute (lp.find_file (nm));
}

std::string find_data_file_in_load_path  (const std::string& fcn,
    const std::string& file,
    bool require_regular_file)
{
  std::string fname = file;

  if (! (sys::env::absolute_pathname (fname)
         || sys::env::rooted_relative_pathname (fname)))
    {
      // Load path will also search "." first, but we don't want to
      // issue a warning if the file is found in the current directory,
      // so do an explicit check for that.
      sys::file_stat fs (fname);

      bool local_file_ok
        = fs.exists () && (fs.is_reg () || ! require_regular_file);

      if (! local_file_ok)
        {
          load_path& lp = __get_load_path__ ();

          // Not directly found; search load path.
          std::string tmp = sys::env::make_absolute (lp.find_file (fname));

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

std::string fcn_file_in_path (const std::string& name)
{
  std::string retval;

  int len = name.length ();

  if (len > 0)
    {
      if (sys::env::absolute_pathname (name))
        {
          sys::file_stat fs (name);

          if (fs.exists () && ! fs.is_dir ())
            retval = name;
        }
      else if (len > 2 && name[len - 2] == '.' && name[len - 1] == 'm')
        {
          load_path& lp = __get_load_path__ ();

          retval = lp.find_fcn_file (name.substr (0, len-2));
        }
      else
        {
          std::string fname = name;
          std::size_t pos = name.find_first_of ('>');
          if (pos != std::string::npos)
            fname = name.substr (0, pos);

          load_path& lp = __get_load_path__ ();

          retval = lp.find_fcn_file (fname);
        }
    }

  return retval;
}

// See if there is a directory called "name" in the path and if it
// contains a Contents.m file.  If so, return the full path to this file.

std::string contents_file_in_path (const std::string& dir)
{
  std::string retval;

  if (! dir.empty ())
    {
      load_path& lp = __get_load_path__ ();

      std::string tcontents
        = sys::file_ops::concat (lp.find_dir (dir), "Contents.m");

      sys::file_stat fs (tcontents);

      if (fs.exists ())
        retval = sys::env::make_absolute (tcontents);
    }

  return retval;
}

// Replace backslash escapes in a string with the real values.

std::string do_string_escapes (const std::string& s)
{
  std::string retval;

  std::size_t i = 0;
  std::size_t j = 0;
  std::size_t len = s.length ();

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
                std::size_t k;
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
                std::size_t k;
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
                  warning (R"(malformed hex escape sequence '\x' -- converting to '\0')");

                retval[i] = tmpi;
                j = k - 1;
                break;
              }

            default:
              warning (R"(unrecognized escape sequence '\%c' -- converting to '%c')", s[j], s[j]);
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
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{newstr} =} do_string_escapes (@var{string})
Convert escape sequences in @var{string} to the characters they represent.

Escape sequences begin with a leading backslash
(@qcode{'@backslashchar{}'}) followed by 1--3 characters
(.e.g., @qcode{"@backslashchar{}n"} => newline).
@seealso{undo_string_escapes}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  std::string str = args(
                      0).xstring_value ("do_string_escapes: STRING argument must be of type string");

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

const char * undo_string_escape (char c)
{
  if (! c)
    return "";

  switch (c)
    {
    case '\0':
      return R"(\0)";

    case '\a':
      return R"(\a)";

    case '\b': // backspace
      return R"(\b)";

    case '\f': // formfeed
      return R"(\f)";

    case '\n': // newline
      return R"(\n)";

    case '\r': // carriage return
      return R"(\r)";

    case '\t': // horizontal tab
      return R"(\t)";

    case '\v': // vertical tab
      return R"(\v)";

    case '\\': // backslash
      return R"(\\)";

    case '"': // double quote
      return R"(\")";

    default:
      {
        static char retval[2] {'\0', '\0'};

        retval[0] = c;
        return retval;
      }
    }
}

std::string undo_string_escapes (const std::string& s)
{
  std::string retval;

  for (std::size_t i = 0; i < s.length (); i++)
    retval.append (undo_string_escape (s[i]));

  return retval;
}

DEFUN (undo_string_escapes, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{newstr} =} undo_string_escapes (@var{string})
Convert special characters in @var{string} back to their escaped forms.

For example, the expression

@example
@var{bell} = "\a";
@end example

@noindent
assigns the value of the alert character (control-g, ASCII code 7) to the
string variable @code{bell}.  If this string is printed, the system will
ring the terminal bell (if it is possible).  This is normally the desired
outcome.  However, sometimes it is useful to be able to print the original
representation of the string, with the special characters replaced by their
escape sequences.  For example,

@example
@group
octave:13> undo_string_escapes (bell)
ans = \a
@end group
@end example

@noindent
replaces the unprintable alert character with its printable representation.
@seealso{do_string_escapes}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  std::string str = args(0).xstring_value ("undo_string_escapes: STRING argument must be a string");

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
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} is_absolute_filename (@var{file})
Return true if @var{file} is an absolute filename.
@seealso{is_rooted_relative_filename, make_absolute_filename, isfolder}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).is_string ()
              && sys::env::absolute_pathname (args(0).string_value ()));
}

/*
## FIXME: We need system-dependent tests here.

%!error is_absolute_filename ()
%!error is_absolute_filename ("foo", "bar")
*/

DEFUN (is_rooted_relative_filename, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} is_rooted_relative_filename (@var{file})
Return true if @var{file} is a rooted-relative filename.
@seealso{is_absolute_filename, make_absolute_filename, isfolder}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).is_string ()
              && sys::env::rooted_relative_pathname (args(0).string_value ()));
}

/*
## FIXME: We need system-dependent tests here.

%!error is_rooted_relative_filename ()
%!error is_rooted_relative_filename ("foo", "bar")
*/

DEFUN (make_absolute_filename, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{abs_fname} =} make_absolute_filename (@var{file})
Return the full name of @var{file} beginning from the root of the file system.

No check is done for the existence of @var{file}.  No tilde expansion of
@var{file} is performed.
@seealso{canonicalize_file_name, is_absolute_filename,
is_rooted_relative_filename, isfolder, tilde_expand}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  std::string nm = args(0).xstring_value ("make_absolute_filename: FILE argument must be a filename");

  return ovl (sys::env::make_absolute (nm));
}

/*
## FIXME: We need system-dependent tests here.

%!error make_absolute_filename ()
%!error make_absolute_filename ("foo", "bar")
*/

DEFMETHOD (dir_in_loadpath, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{dirname} =} dir_in_loadpath (@var{dir})
@deftypefnx {} {@var{dirname} =} dir_in_loadpath (@var{dir}, "all")
Return the absolute name of the loadpath element matching @var{dir} if it can
be found in the list of directories specified by @code{path}.

If no match is found, return an empty character string.

The match is performed at the end of each path element.  For example, if
@var{dir} is @qcode{"foo/bar"}, it matches the path element
@nospell{@qcode{"/some/dir/foo/bar"}}, but not
@nospell{@qcode{"/some/dir/foo/bar/baz"}}
@nospell{@qcode{"/some/dir/allfoo/bar"}}.  When @var{dir} is an absolute name,
rather than just a path fragment, it is matched against the file system
instead of Octave's loadpath.  In this case, if @var{dir} exists it will be
returned in @var{dirname}, otherwise an empty string is returned.

If the optional second argument is supplied, return a cell array containing
all name matches rather than just the first.
@seealso{file_in_path, file_in_loadpath, path}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  std::string dir;

  dir = args(0).xstring_value ("dir_in_loadpath: DIR must be a directory name");

  load_path& lp = interp.get_load_path ();

  if (nargin == 1)
    return ovl (lp.find_dir (dir));
  else
    return ovl (Cell (lp.find_matching_dirs (dir)));
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
        doc: /* -*- texinfo -*-
@deftypefn  {} {@var{err} =} errno ()
@deftypefnx {} {@var{err} =} errno (@var{val})
@deftypefnx {} {@var{err} =} errno (@var{name})
Query or set the system-dependent variable errno.

When called with no inputs, return the current value of errno.

When called with a numeric input @var{val}, set the current value of errno
to the specified value.  The previous value of errno is returned as @var{err}.

When called with a character string @var{name}, return the numeric value of
errno which corresponds to the specified error code.  If @var{name} is not
a recognized error code then -1 is returned.

@seealso{errno_list}
@end deftypefn */)
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
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{S} =} errno_list ()
Return a structure containing the system-dependent errno values.
@seealso{errno}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (octave_errno::list ());
}

/*
%!assert (isstruct (errno_list ()))

%!error errno_list ("foo")
*/

static void check_dimensions (octave_idx_type& nr, octave_idx_type& nc,
                              const char *warnfor)
{
  if (nr < 0 || nc < 0)
    {
      warning_with_id ("Octave:neg-dim-as-zero",
                       "%s: converting negative dimension to zero", warnfor);

      nr = (nr < 0) ? 0 : nr;
      nc = (nc < 0) ? 0 : nc;
    }
}

void check_dimensions (dim_vector& dim, const char *warnfor)
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

void get_dimensions (const octave_value& a, const char *warn_for,
                     dim_vector& dim)
{
  // We support dimensions to be specified by a vector, even if it's empty.
  // If the vector is empty, the final dimensions end up being 0x0.
  if (! a.dims ().isvector () && a.dims ().numel () != 0)
    error ("%s (A): use %s (size (A)) instead", warn_for, warn_for);

  const Array<octave_idx_type> v = a.octave_idx_type_vector_value (true);
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

void get_dimensions (const octave_value& a, const char *warn_for,
                     octave_idx_type& nr, octave_idx_type& nc)
{
  if (a.is_scalar_type ())
    {
      nr = nc = a.idx_type_value (true);
    }
  else
    {
      nr = a.rows ();
      nc = a.columns ();

      if ((nr != 1 || nc != 2) && (nr != 2 || nc != 1))
        error ("%s (A): use %s (size (A)) instead", warn_for, warn_for);

      Array<octave_idx_type> v = a.octave_idx_type_vector_value (true);
      nr = v(0);
      nc = v(1);
    }

  check_dimensions (nr, nc, warn_for);
}

void get_dimensions (const octave_value& a, const octave_value& b,
                     const char *warn_for, octave_idx_type& nr,
                     octave_idx_type& nc)
{
  nr = (a.isempty () ? 0 : a.idx_type_value (true));
  nc = (b.isempty () ? 0 : b.idx_type_value (true));

  check_dimensions (nr, nc, warn_for);
}

octave_idx_type dims_to_numel (const dim_vector& dims,
                               const octave_value_list& idx_arg)
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
          else if (idxi.isnumeric ())
            retval *= idxi.numel ();
          else
            {
              try
                {
                  idx_vector jdx = idxi.index_vector ();

                  retval *= jdx.length (dv(i));
                }
              catch (const index_exception& ie)
                {
                  error ("dims_to_numel: invalid index %s", ie.what ());
                }
            }
        }
    }

  return retval;
}

Matrix identity_matrix (octave_idx_type nr, octave_idx_type nc)
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

FloatMatrix float_identity_matrix (octave_idx_type nr, octave_idx_type nc)
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

std::size_t format (std::ostream& os, const char *fmt, ...)
{
  std::size_t retval;

  va_list args;
  va_start (args, fmt);

  retval = vformat (os, fmt, args);

  va_end (args);

  return retval;
}

std::size_t vformat (std::ostream& os, const char *fmt, va_list args)
{
  std::string s = vasprintf (fmt, args);

  os << s;

  return s.length ();
}

std::string vasprintf (const char *fmt, va_list args)
{
  std::string retval;

  char *result;

  int status = octave_vasprintf_wrapper (&result, fmt, args);

  if (status >= 0)
    {
      retval = result;
      ::free (result);
    }

  return retval;
}

std::string asprintf (const char *fmt, ...)
{
  std::string retval;

  va_list args;
  va_start (args, fmt);

  retval = vasprintf (fmt, args);

  va_end (args);

  return retval;
}

// FIXME: sleep is complicated because we want it to be interruptible.
// With the way this program handles signals, the sleep system call
// won't respond to SIGINT.  Maybe there is a better way than
// breaking this up into multiple shorter intervals?

void sleep (double seconds, bool do_graphics_events)
{
  if (seconds <= 0)
    return;

  // Allow free access to graphics resources while the interpreter thread
  // is asleep

  gh_manager& gh_mgr = __get_gh_manager__ ();

  if (do_graphics_events)
    gh_mgr.unlock ();

  if (math::isinf (seconds))
    {
      // Wait for kbhit
      int c = -1;
      flush_stdout ();

      struct timespec one_tenth = { 0, 100000000 };

      while (c < 0)
        {
          octave_nanosleep_wrapper (&one_tenth, nullptr);

          octave_quit ();

          if (do_graphics_events)
            gh_mgr.process_events ();

          c = kbhit (false);
        }
    }
  else
    {
      sys::time now;
      double end_time = now.double_value () + seconds;
      double remaining_time = seconds;

      // Split pause into 100 ms time steps to allow the execution of
      // graphics events and interrupts.
      struct timespec nano_laps = { 0, 100000000 };

      while (remaining_time > 0.1)
        {
          octave_quit ();

          if (do_graphics_events)
            {
              gh_mgr.process_events ();

              now.stamp ();
              remaining_time = end_time - now.double_value ();

              if (remaining_time < 0.1)
                break;
            }

          octave_nanosleep_wrapper (&nano_laps, nullptr);

          now.stamp ();
          remaining_time = end_time - now.double_value ();
        }

      if (remaining_time > 0.0)
        {
          nano_laps = { 0, static_cast<int> (remaining_time * 1e9) };
          octave_nanosleep_wrapper (&nano_laps, nullptr);
        }
    }
}

DEFMETHOD (isindex, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{tf} =} isindex (@var{ind})
@deftypefnx {} {@var{tf} =} isindex (@var{ind}, @var{n})
Return true if @var{ind} is a valid index.

Valid indices are either positive integers (although possibly of real data
type), or logical arrays.

If present, @var{n} specifies the maximum extent of the dimension to be
indexed.  When possible the internal result is cached so that subsequent
indexing using @var{ind} will not perform the check again.

Implementation Note: Strings are first converted to double values before the
checks for valid indices are made.  Unless a string contains the NULL
character @nospell{"@backslashchar{}0"}, it will always be a valid index.
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  octave_idx_type n = 0;
  if (nargin == 2)
    n = args(1).idx_type_value ();

  octave_value retval;

  try
    {
      idx_vector idx = args(0).index_vector (true);

      if (nargin == 2)
        retval = idx.extent (n) <= n;
      else
        retval = true;
    }
  catch (const execution_exception&)
    {
      interp.recover_from_exception ();

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
do_simple_cellfun (octave_value_list (*fcn) (const octave_value_list&, int),
                   const char *fcn_name, const octave_value_list& args,
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
      iscell[i] = arg.iscell ();
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
            error ("%s: cell arguments must have matching sizes", fcn_name);
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

      const octave_value_list tmp = fcn (new_args, nargout);

      if (tmp.length () < nargout)
        error ("%s: do_simple_cellfun: internal error", fcn_name);

      for (int i = 0; i < nargout; i++)
        rcells[i](j) = tmp(i);
    }

  retval.resize (nargout);

  for (int i = 0; i < nargout; i++)
    retval(i) = rcells[i];

  return retval;
}

octave_value
do_simple_cellfun (octave_value_list (*fcn) (const octave_value_list&, int),
                   const char *fcn_name, const octave_value_list& args)
{
  octave_value retval;

  const octave_value_list tmp = do_simple_cellfun (fcn, fcn_name, args, 1);

  if (tmp.length () > 0)
    retval = tmp(0);

  return retval;
}

DEFUN (isstudent, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isstudent ()
Return true if running in the student edition of @sc{matlab}.

@code{isstudent} always returns false in Octave.
@seealso{false}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (false);
}

/*
%!assert (isstudent (), false)

%!error isstudent (1)
*/

OCTAVE_END_NAMESPACE(octave)
