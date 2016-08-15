/*

Copyright (C) 1993-2015 John W. Eaton

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

#include <cstdlib>
#include <cstring>

#include <algorithm>
#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <string>

#include "cmd-edit.h"
#include "file-ops.h"
#include "file-stat.h"
#include "oct-env.h"
#include "oct-locbuf.h"
#include "str-vec.h"

#include "call-stack.h"
#include <defaults.h>
#include "Cell.h"
#include "defun.h"
#include "dirfns.h"
#include "error.h"
#include "errwarn.h"
#include "help.h"
#include "input.h"
#include "load-path.h"
#include "ovl.h"
#include "ov-usr-fcn.h"
#include "ov-fcn-handle.h"
#include "pager.h"
#include "parse.h"
#include "pathsearch.h"
#include "procstream.h"
#include "pt-pr-code.h"
#include "sighandlers.h"
#include "symtab.h"
#include "interpreter.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include "version.h"
#include "quit.h"

// Name of the doc cache file specified on the command line.
// (--doc-cache-file file)
std::string Vdoc_cache_file;

// Name of the file containing local Texinfo macros that are prepended
// to doc strings before processing.
// (--texi-macros-file)
std::string Vtexi_macros_file;

// Name of the info file specified on command line.
// (--info-file file)
std::string Vinfo_file;

// Name of the info reader we'd like to use.
// (--info-program program)
std::string Vinfo_program;

// Name of the makeinfo program to run.
static std::string Vmakeinfo_program = "makeinfo";

// If TRUE, don't print additional help message in help and usage
// functions.
static bool Vsuppress_verbose_help_message = false;

const static char * const operators[] =
{
  "!",
  "~",
  "!=",
  "~=",
  "\"",
  "#",
  "%",
  "#{",
  "%{",
  "#}",
  "%}",
  "...",
  "&",
  "&&",
  "'",
  "(",
  ")",
  "*",
  "**",
  "^",
  "+",
  "++",
  ",",
  "-",
  "--",
  ".'",
  ".*",
  ".**",
  ".^",
  "./",
  "/",
  ".\\",
  "\\",
  ":",
  ";",
  "<",
  "<=",
  "=",
  "==",
  ">",
  ">=",
  "[",
  "]",
  "|",
  "||",
  0
};

const static string_vector operator_names (operators);

const static char * const keywords[] =
{
  "break",
  "case",
  "catch",
  "continue",
  "do",
  "else",
  "elseif",
  "end",
  "end_try_catch",
  "end_unwind_protect",
  "endfor",
  "endfunction",
  "endif",
  "endparfor",
  "endswitch",
  "endwhile",
  "for",
  "function",
  "global",
  "if",
  "otherwise",
  "parfor",
  "persistent",
  "return",
  "static",
  "switch",
  "try",
  "until",
  "unwind_protect",
  "unwind_protect_cleanup",
  "varargin",
  "varargout",
  "while",
  0
};

const static string_vector keyword_names (keywords);

// Return a vector of all functions from this file,
// for use in command line auto-completion.
static string_vector
local_functions (void)
{
  string_vector retval;

  octave_user_code *curr_fcn = octave_call_stack::caller_user_code ();

  if (! curr_fcn)
    return retval;

  // All subfunctions are listed in the top-level function of this file.
  while (curr_fcn->is_subfunction ())
    curr_fcn = symbol_table::get_curr_fcn (curr_fcn->parent_fcn_scope ());

  // Get subfunctions.
  const std::list<std::string> names = curr_fcn->subfunction_names ();

  size_t sz = names.size ();
  retval.resize (sz);

  // Loop over them.
  size_t i = 0;
  for (std::list<std::string>::const_iterator p = names.begin ();
       p != names.end (); p++)
    retval(i++) = *p;

  retval.resize (i);
  return retval;
}

// FIXME: It's not likely that this does the right thing now.

string_vector
make_name_list (void)
{
  const int key_len = keyword_names.numel ();

  const string_vector bif = symbol_table::built_in_function_names ();
  const int bif_len = bif.numel ();

  const string_vector cfl = symbol_table::cmdline_function_names ();
  const int cfl_len = cfl.numel ();

  const string_vector lcl = symbol_table::variable_names ();
  const int lcl_len = lcl.numel ();

  const string_vector ffl = load_path::fcn_names ();
  const int ffl_len = ffl.numel ();

  const string_vector afl = autoloaded_functions ();
  const int afl_len = afl.numel ();

  const string_vector lfl = local_functions ();
  const int lfl_len = lfl.numel ();

  const int total_len
    = key_len + bif_len + cfl_len + lcl_len + ffl_len + afl_len + lfl_len;

  string_vector list (total_len);

  // Put all the symbols in one big list.

  int j = 0;
  int i = 0;
  for (i = 0; i < key_len; i++)
    list[j++] = keyword_names[i];

  for (i = 0; i < bif_len; i++)
    list[j++] = bif[i];

  for (i = 0; i < cfl_len; i++)
    list[j++] = cfl[i];

  for (i = 0; i < lcl_len; i++)
    list[j++] = lcl[i];

  for (i = 0; i < ffl_len; i++)
    list[j++] = ffl[i];

  for (i = 0; i < afl_len; i++)
    list[j++] = afl[i];

  for (i = 0; i < lfl_len; i++)
    list[j++] = lfl[i];

  return list;
}

static bool
looks_like_html (const std::string& msg)
{
  const size_t p1 = msg.find ('\n');
  std::string t = msg.substr (0, p1);
  // FIXME: this comparison should be case-insensitive
  const size_t p2 = t.find ("<html");

  return (p2 != std::string::npos);
}

static bool
looks_like_texinfo (const std::string& msg, size_t& p1)
{
  p1 = msg.find ('\n');

  std::string t = msg.substr (0, p1);

  if (p1 == std::string::npos)
    p1 = 0;

  size_t p2 = t.find ("-*- texinfo -*-");

  return (p2 != std::string::npos);
}

static bool
raw_help_from_symbol_table (const std::string& nm, std::string& h,
                            std::string& w, bool& symbol_found)
{
  bool retval = false;

  octave_value val = symbol_table::find_function (nm);

  if (val.is_defined ())
    {
      octave_function *fcn = val.function_value ();

      if (fcn)
        {
          symbol_found = true;

          h = fcn->doc_string ();

          retval = true;

          w = fcn->fcn_file_name ();

          if (w.empty ())
            w = fcn->is_user_function ()
                ? "command-line function" : "built-in function";
        }
    }

  return retval;
}

static bool
raw_help_from_file (const std::string& nm, std::string& h,
                    std::string& file, bool& symbol_found)
{
  bool retval = false;

  h = get_help_from_file (nm, symbol_found, file);

  if (h.length () > 0)
    retval = true;

  return retval;
}

static bool
raw_help_from_docstrings_file (const std::string& nm, std::string& h,
                               bool& symbol_found)
{
  typedef std::pair<std::streampos, std::streamoff> txt_limits_type;
  typedef std::map<std::string, txt_limits_type> help_txt_map_type;

  static help_txt_map_type help_txt_map;
  static bool initialized = false;

  h = "";
  symbol_found = false;

  // FIXME: Should we cache the timestamp of the file and reload the
  // offsets if it changes?  Or just warn about that?  Or just ignore
  // it, and assume it won't change?

  if (! initialized)
    {
      std::string fname = Vbuilt_in_docstrings_file;

      std::ifstream file (fname.c_str (), std::ios::in | std::ios::binary);

      if (! file)
        error ("failed to open docstrings file: %s", fname.c_str ());

      // Ignore header;
      file.ignore (std::numeric_limits<std::streamsize>::max(), 0x1d);

      if (file.eof ())
        error ("invalid built-in-docstrings file!");

      // FIXME: eliminate fixed buffer size.
      size_t bufsize = 1000;
      OCTAVE_LOCAL_BUFFER (char, buf, bufsize);

      while (! file.eof ())
        {
          std::string name;
          int i = 0;
          int c;
          while (file && (c = file.get ()) != std::istream::traits_type::eof ())
            {
              if (c == '\n' || c == '\r')
                {
                  buf[i] = '\0';
                  name = buf;
                  break;
                }
              else
                buf[i++] = c;
            }

          // Skip @c FILENAME which is part of current DOCSTRINGS
          // syntax.  This may disappear if a specific format for
          // docstring files is developed.
          while (file
                 && (c = file.get ()) != std::istream::traits_type::eof ()
                 && c != '\n' && c != '\r')
            ; // skip text

          // skip newline characters
          while (file
                 && (c = file.get ()) != std::istream::traits_type::eof ()
                 && c == '\n' && c == '\r')
            ; // skip text

          file.unget ();

          // Position of beginning of help text.
          std::streampos beg = file.tellg ();

          // Skip help text.
          file.ignore (std::numeric_limits<std::streamsize>::max(), 0x1d);

          // Position of end of help text.
          std::streamoff len = file.tellg () - beg - 1;

          help_txt_map[name] = txt_limits_type (beg, len);
        }

      initialized = true;
    }

  help_txt_map_type::const_iterator it = help_txt_map.find (nm);

  if (it != help_txt_map.end ())
    {
      txt_limits_type txt_limits = it->second;

      std::streampos beg = txt_limits.first;
      std::streamoff len = txt_limits.second;

      std::string fname = Vbuilt_in_docstrings_file;

      std::ifstream file (fname.c_str (), std::ios::in | std::ios::binary);

      if (! file)
        error ("failed to open docstrings file: %s", fname.c_str ());

      file.seekg (beg);

      size_t txt_len = len;
      OCTAVE_LOCAL_BUFFER (char, buf, txt_len + 1);

      file.read (buf, txt_len);

      buf[txt_len] = '\0';

      h = buf;

      symbol_found = true;
    }

  return symbol_found;
}

std::string
raw_help (const std::string& nm, bool& symbol_found)
{
  std::string h;
  std::string w;
  std::string f;

  bool found;

  found = raw_help_from_symbol_table (nm, h, w, symbol_found);

  if (! found)
    found = raw_help_from_file (nm, h, f, symbol_found);

  if (! found || h == "external-doc")
    raw_help_from_docstrings_file (nm, h, symbol_found);

  return h;
}

DEFUN (built_in_docstrings_file, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} built_in_docstrings_file ()
@deftypefnx {} {@var{old_val} =} built_in_docstrings_file (@var{new_val})
@deftypefnx {} {} built_in_docstrings_file (@var{new_val}, "local")
Query or set the internal variable that specifies the name of the
file containing docstrings for built-in Octave functions.

The default value is
@file{@var{octave-home}/share/octave/@var{version}/etc/built-in-docstrings},
in which @var{octave-home} is the root directory of the Octave installation,
and @var{version} is the Octave version number.  The default value may be
overridden by the environment variable
@w{@env{OCTAVE_BUILT_IN_DOCSTRINGS_FILE}}, or the command line argument
@option{--built-in-docstrings-file FNAME}.

Note: This variable is only used when Octave is initializing itself.
Modifying it during a running session of Octave will have no effect.
@end deftypefn */)
{
  return SET_NONEMPTY_INTERNAL_STRING_VARIABLE (built_in_docstrings_file);
}

static void
do_get_help_text (const std::string& name, std::string& text,
                  std::string& format)
{
  bool symbol_found = false;
  text = raw_help (name, symbol_found);

  format = "Not found";
  if (symbol_found)
    {
      size_t idx = -1;
      if (text.empty ())
        {
          format = "Not documented";
        }
      else if (looks_like_texinfo (text, idx))
        {
          format = "texinfo";
          text.erase (0, idx);
        }
      else if (looks_like_html (text))
        {
          format = "html";
        }
      else
        {
          format = "plain text";
        }
    }
}

DEFUN (get_help_text, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {[@var{text}, @var{format}] =} get_help_text (@var{name})
Return the raw help text of function @var{name}.

The raw help text is returned in @var{text} and the format in @var{format}
The format is a string which is one of @qcode{"texinfo"},
@qcode{"html"}, or @qcode{"plain text"}.
@seealso{get_help_text_from_file}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  const std::string name = args(0).xstring_value ("get_help_text: NAME must be a string");

  std::string text, format;

  do_get_help_text (name, text, format);

  return ovl (text, format);
}

static void
do_get_help_text_from_file (const std::string& fname, std::string& text,
                            std::string& format)
{
  bool symbol_found = false;

  std::string f;

  raw_help_from_file (fname, text, f, symbol_found);

  format = "Not found";
  if (symbol_found)
    {
      size_t idx = -1;
      if (text.empty ())
        {
          format = "Not documented";
        }
      else if (looks_like_texinfo (text, idx))
        {
          format = "texinfo";
          text.erase (0, idx);
        }
      else if (looks_like_html (text))
        {
          format = "html";
        }
      else
        {
          format = "plain text";
        }
    }
}

DEFUN (get_help_text_from_file, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {[@var{text}, @var{format}] =} get_help_text_from_file (@var{fname})
Return the raw help text from the file @var{fname}.

The raw help text is returned in @var{text} and the format in @var{format}
The format is a string which is one of @qcode{"texinfo"},
@qcode{"html"}, or @qcode{"plain text"}.
@seealso{get_help_text}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  const std::string fname = args(0).xstring_value ("get_help_text_from_file: NAME must be a string");

  std::string text, format;

  do_get_help_text_from_file (fname, text, format);

  return ovl (text, format);
}

// Return a cell array of strings containing the names of all
// operators.

DEFUN (__operators__, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} __operators__ ()
Undocumented internal function.
@end deftypefn */)
{
  return ovl (Cell (operator_names));
}

// Return a cell array of strings containing the names of all
// keywords.

DEFUN (__keywords__, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} __keywords__ ()
Undocumented internal function.
@end deftypefn */)
{
  return ovl (Cell (keyword_names));
}

// Return a cell array of strings containing the names of all builtin
// functions.

DEFUN (__builtins__, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} __builtins__ ()
Undocumented internal function.
@end deftypefn */)
{
  const string_vector bif = symbol_table::built_in_function_names ();

  return ovl (Cell (bif));
}

DEFUN (localfunctions, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} localfunctions ()
Return a list of all local functions, i.e., subfunctions, within the current
file.

The return value is a column cell array of function handles to all local
functions accessible from the function from which @code{localfunctions} is
called.  Nested functions are @emph{not} included in the list.

If the call is from the command line, an anonymous function, or a script,
the return value is an empty cell array.

Compatibility Note: Subfunctions which contain nested functions are not
included in the list.  This is a known issue.
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  Cell retval;

  // Find the main function we are in.
  octave_user_code *parent_fcn = octave_call_stack::debug_user_code ();

  if (! parent_fcn)
    return ovl (retval);

  // Find the subfunctions of this function.
  // FIXME: This includes all nested functions.
  //        Once handles of nested functions are implemented,
  //        we will need to exclude ones not in scope.
  const std::list<std::string> names = parent_fcn->subfunction_names ();
  const std::map<std::string, octave_value> h = parent_fcn->subfunctions ();

  size_t sz = names.size ();
  retval.resize (dim_vector (sz, 1));

  // loop over them.
  size_t i = 0;
  for (std::list<std::string>::const_iterator p = names.begin ();
       p != names.end (); p++)
    {
      std::map<std::string, octave_value>::const_iterator q = h.find (*p);
      if (q != h.end () &&
          ! q->second.user_function_value ()->is_nested_function ())
        retval(i++) = octave_value (new octave_fcn_handle (q->second, *p));
    }

  // remove pre-allocation for nested functions
  retval.resize (dim_vector (i, 1));

  return ovl (retval);
}

/*
%!test
%! f = tempname (".", "oct_");
%! fcn_name = f(3:end);
%! f = [f ".m"];
%! unwind_protect
%!   fid = fopen (f, "w+");
%!   fprintf (fid, "function z = %s\n z = localfunctions; end\n", fcn_name);
%!   fprintf (fid, "function z = b(x)\n z = x+1; end\n");
%!   fprintf (fid, "function z = c(x)\n z = 2*x; end\n");
%!   fclose (fid);
%!   d = eval (fcn_name);
%!   assert (size (d), [2, 1]);
%!   assert (d{1}(3), 4);
%!   assert (d{2}(3), 6);
%! unwind_protect_cleanup
%!   unlink (f);
%! end_unwind_protect
*/

static std::string
do_which (const std::string& name, std::string& type)
{
  std::string file;

  type = "";

  octave_value val = symbol_table::find_function (name);

  if (name.find_first_of ('.') == std::string::npos)
    {
      if (val.is_defined ())
        {
          octave_function *fcn = val.function_value ();

          if (fcn)
            {
              file = fcn->fcn_file_name ();

              if (file.empty ())
                {
                  if (fcn->is_user_function ())
                    type = "command-line function";
                  else
                    {
                      file = fcn->src_file_name ();
                      type = "built-in function";
                    }
                }
              else
                type = val.is_user_script ()
                       ? std::string ("script") : std::string ("function");
            }
        }
      else
        {
          // We might find a file that contains only a doc string.

          file = load_path::find_fcn_file (name);
        }
    }
  else
    {
      // File query.

      // For compatibility: "file." queries "file".
      if (name.size () > 1 && name[name.size () - 1] == '.')
        file = load_path::find_file (name.substr (0, name.size () - 1));
      else
        file = load_path::find_file (name);

      file = octave::sys::env::make_absolute (file);
    }

  return file;
}

std::string
do_which (const std::string& name)
{
  std::string retval;

  std::string type;

  retval = do_which (name, type);

  return retval;
}

DEFUN (__which__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} __which__ (@var{name}, @dots{})
Undocumented internal function.
@end deftypefn */)
{
  string_vector argv = args.make_argv ();

  int nargin = argv.numel ();

  octave_map m (dim_vector (1, nargin));

  Cell names (1, nargin);
  Cell files (1, nargin);
  Cell types (1, nargin);

  for (int i = 0; i < nargin; i++)
    {
      std::string name = argv[i];

      std::string type;

      std::string file = do_which (name, type);

      names(i) = name;
      files(i) = file;
      types(i) = type;
    }

  m.assign ("name", names);
  m.assign ("file", files);
  m.assign ("type", types);

  return ovl (m);
}

// FIXME: Are we sure this function always does the right thing?
inline bool
file_is_in_dir (const std::string filename, const std::string dir)
{
  if (filename.find (dir) == 0)
    {
      const int dir_len = dir.size ();
      const int filename_len = filename.size ();
      const int max_allowed_seps = octave::sys::file_ops::is_dir_sep (dir[dir_len-1]) ? 0
                                                                         : 1;

      int num_seps = 0;
      for (int i = dir_len; i < filename_len; i++)
        if (octave::sys::file_ops::is_dir_sep (filename[i]))
          num_seps++;

      return (num_seps <= max_allowed_seps);
    }
  else
    return false;
}

// Return a cell array of strings containing the names of all
// functions available in DIRECTORY.  If no directory is given, search
// the current path.

DEFUN (__list_functions__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{retval} =} __list_functions__ ()
@deftypefnx {} {@var{retval} =} __list_functions__ (@var{directory})
Undocumented internal function.
@end deftypefn */)
{
  octave_value retval;

  // Get list of functions
  string_vector ffl = load_path::fcn_names ();
  string_vector afl = autoloaded_functions ();

  if (args.length () == 0)
    retval = Cell (ffl.append (afl));
  else
    {
      std::string dir = args(0).xstring_value ("__list_functions__: DIRECTORY argument must be a string");

      string_vector fl = load_path::files (dir, true);

      // Return a sorted list with unique entries (in case of
      // .m and .oct versions of the same function in a given
      // directory, for example).
      fl.sort (true);

      retval = Cell (fl);
    }

  return retval;
}

DEFUN (doc_cache_file, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} doc_cache_file ()
@deftypefnx {} {@var{old_val} =} doc_cache_file (@var{new_val})
@deftypefnx {} {} doc_cache_file (@var{new_val}, "local")
Query or set the internal variable that specifies the name of the
Octave documentation cache file.

A cache file significantly improves the performance of the @code{lookfor}
command.  The default value is
@file{@var{octave-home}/share/octave/@var{version}/etc/doc-cache},
in which @var{octave-home} is the root directory of the Octave installation,
and @var{version} is the Octave version number.
The default value may be overridden by the environment variable
@w{@env{OCTAVE_DOC_CACHE_FILE}}, or the command line argument
@option{--doc-cache-file FNAME}.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{doc_cache_create, lookfor, info_program, doc, help, makeinfo_program}
@seealso{lookfor}
@end deftypefn */)
{
  return SET_NONEMPTY_INTERNAL_STRING_VARIABLE (doc_cache_file);
}

DEFUN (texi_macros_file, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} texi_macros_file ()
@deftypefnx {} {@var{old_val} =} texi_macros_file (@var{new_val})
@deftypefnx {} {} texi_macros_file (@var{new_val}, "local")
Query or set the internal variable that specifies the name of the
file containing Texinfo macros that are prepended to documentation strings
before they are passed to makeinfo.

The default value is
@file{@var{octave-home}/share/octave/@var{version}/etc/macros.texi},
in which @var{octave-home} is the root directory of the Octave installation,
and @var{version} is the Octave version number.
The default value may be overridden by the environment variable
@w{@env{OCTAVE_TEXI_MACROS_FILE}}, or the command line argument
@option{--texi-macros-file FNAME}.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{makeinfo_program}
@end deftypefn */)
{
  return SET_NONEMPTY_INTERNAL_STRING_VARIABLE (texi_macros_file);
}

DEFUN (info_file, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} info_file ()
@deftypefnx {} {@var{old_val} =} info_file (@var{new_val})
@deftypefnx {} {} info_file (@var{new_val}, "local")
Query or set the internal variable that specifies the name of the
Octave info file.

The default value is
@file{@var{octave-home}/info/octave.info}, in
which @var{octave-home} is the root directory of the Octave installation.
The default value may be overridden by the environment variable
@w{@env{OCTAVE_INFO_FILE}}, or the command line argument
@option{--info-file FNAME}.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{info_program, doc, help, makeinfo_program}
@end deftypefn */)
{
  return SET_NONEMPTY_INTERNAL_STRING_VARIABLE (info_file);
}

DEFUN (info_program, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} info_program ()
@deftypefnx {} {@var{old_val} =} info_program (@var{new_val})
@deftypefnx {} {} info_program (@var{new_val}, "local")
Query or set the internal variable that specifies the name of the
info program to run.

The default value is
@file{@var{octave-home}/libexec/octave/@var{version}/exec/@var{arch}/info}
in which @var{octave-home} is the root directory of the Octave installation,
@var{version} is the Octave version number, and @var{arch} is the system
type (for example, @code{i686-pc-linux-gnu}).  The default value may be
overridden by the environment variable
@w{@env{OCTAVE_INFO_PROGRAM}}, or the command line argument
@option{--info-program NAME}.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{info_file, doc, help, makeinfo_program}
@end deftypefn */)
{
  return SET_NONEMPTY_INTERNAL_STRING_VARIABLE (info_program);
}

DEFUN (makeinfo_program, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} makeinfo_program ()
@deftypefnx {} {@var{old_val} =} makeinfo_program (@var{new_val})
@deftypefnx {} {} makeinfo_program (@var{new_val}, "local")
Query or set the internal variable that specifies the name of the
program that Octave runs to format help text containing
Texinfo markup commands.

The default value is @code{makeinfo}.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{texi_macros_file, info_file, info_program, doc, help}
@end deftypefn */)
{
  return SET_NONEMPTY_INTERNAL_STRING_VARIABLE (makeinfo_program);
}

DEFUN (suppress_verbose_help_message, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} suppress_verbose_help_message ()
@deftypefnx {} {@var{old_val} =} suppress_verbose_help_message (@var{new_val})
@deftypefnx {} {} suppress_verbose_help_message (@var{new_val}, "local")
Query or set the internal variable that controls whether Octave
will add additional help information to the end of the output from
the @code{help} command and usage messages for built-in commands.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@end deftypefn */)
{
  return SET_INTERNAL_VARIABLE (suppress_verbose_help_message);
}
