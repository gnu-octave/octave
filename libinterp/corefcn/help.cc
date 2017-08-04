/*

Copyright (C) 1993-2017 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

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

#include "Cell.h"
#include "builtin-defun-decls.h"
#include "call-stack.h"
#include "defaults.h"
#include "defun.h"
#include "dirfns.h"
#include "error.h"
#include "errwarn.h"
#include "help.h"
#include "input.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "load-path.h"
#include "ov-fcn-handle.h"
#include "ov-usr-fcn.h"
#include "ovl.h"
#include "pager.h"
#include "parse.h"
#include "pathsearch.h"
#include "procstream.h"
#include "pt-pr-code.h"
#include "quit.h"
#include "sighandlers.h"
#include "symtab.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include "version.h"

#include "default-defs.h"

const static char * const operators[] =
{
  "!",
  "~",
  "!=",
  "~=",
  R"(")",
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
  R"(.\)",
  R"(\)",
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
  nullptr
};

const static string_vector operator_names (operators);

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

// FIXME: Are we sure this function always does the right thing?
static inline bool
file_is_in_dir (const std::string filename, const std::string dir)
{
  if (filename.find (dir) == 0)
    {
      const int dir_len = dir.size ();
      const int filename_len = filename.size ();
      const int max_allowed_seps =
        (octave::sys::file_ops::is_dir_sep (dir[dir_len-1]) ? 0 : 1);

      int num_seps = 0;
      for (int i = dir_len; i < filename_len; i++)
        if (octave::sys::file_ops::is_dir_sep (filename[i]))
          num_seps++;

      return (num_seps <= max_allowed_seps);
    }
  else
    return false;
}

namespace octave
{
  octave_value
  help_system::built_in_docstrings_file (const octave_value_list& args,
                                         int nargout)
  {
    return set_internal_variable (m_built_in_docstrings_file, args, nargout,
                                  "built_in_docstrings_file", false);
  }

  octave_value
  help_system::doc_cache_file (const octave_value_list& args, int nargout)
  {
    return set_internal_variable (m_doc_cache_file, args, nargout,
                                  "doc_cache_file", false);
  }

  octave_value
  help_system::info_file (const octave_value_list& args, int nargout)
  {
    return set_internal_variable (m_info_file, args, nargout,
                                  "info_file", false);
  }

  octave_value
  help_system::info_program (const octave_value_list& args, int nargout)
  {
    return set_internal_variable (m_info_program, args, nargout,
                                  "info_program", false);
  }

  octave_value
  help_system::makeinfo_program (const octave_value_list& args, int nargout)
  {
    return set_internal_variable (m_makeinfo_program, args, nargout,
                                  "makeinfo_program", false);
  }

  octave_value
  help_system::suppress_verbose_help_message (const octave_value_list& args,
                                              int nargout)
  {
    return set_internal_variable (m_suppress_verbose_help_message, args,
                                  nargout, "suppress_verbose_help_message");
  }

  octave_value
  help_system::texi_macros_file (const octave_value_list& args, int nargout)
  {
    return set_internal_variable (m_texi_macros_file, args, nargout,
                                  "texi_macros_file", false);
  }

  std::string
  help_system::raw_help (const std::string& nm, bool& symbol_found) const
  {
    std::string h;
    std::string w;
    std::string f;

    bool found;

    found = raw_help_from_symbol_table (nm, h, w, symbol_found);

    if (! found)
      found = raw_help_from_file (nm, h, f, symbol_found);

    bool external_doc = h.compare (0, 12, "external-doc") == 0;

    if (! found || external_doc)
      {
        std::string tmp_nm = nm;

        if (external_doc && h.length () > 12 && h[12] == ':')
          tmp_nm = h.substr (13);

        raw_help_from_docstrings_file (tmp_nm, h, symbol_found);
      }

    return h;
  }

  std::string help_system::which (const std::string& name,
                                  std::string& type) const
  {
    std::string file;

    type = "";

    symbol_table& symtab = m_interpreter.get_symbol_table ();

    octave_value val = symtab.find_function (name);

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
                  type = val.is_user_script () ? "script" : "function";
              }
          }
        else
          {
            // We might find a file that contains only a doc string.

            load_path& lp = m_interpreter.get_load_path ();

            file = lp.find_fcn_file (name);
          }
      }
    else
      {
        // File query.

        load_path& lp = m_interpreter.get_load_path ();

        // For compatibility: "file." queries "file".
        if (name.size () > 1 && name[name.size () - 1] == '.')
          file = lp.find_file (name.substr (0, name.size () - 1));
        else
          file = lp.find_file (name);

        file = sys::env::make_absolute (file);
      }

    return file;
  }

  std::string help_system::which (const std::string& name) const
  {
    std::string type;

    return which (name, type);
  }

  string_vector help_system::make_name_list (void) const
  {
    const static string_vector keywords
      = Fiskeyword ()(0).string_vector_value ();

    const static int key_len = keywords.numel ();

    symbol_table& symtab = m_interpreter.get_symbol_table ();

    const string_vector bif = symtab.built_in_function_names ();
    const int bif_len = bif.numel ();

    const string_vector cfl = symtab.cmdline_function_names ();
    const int cfl_len = cfl.numel ();

    const string_vector lcl = symtab.variable_names ();
    const int lcl_len = lcl.numel ();

    load_path& lp = m_interpreter.get_load_path ();

    const string_vector ffl = lp.fcn_names ();
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
      list[j++] = keywords[i];

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

  void help_system::get_help_text (const std::string& name, std::string& text,
                                   std::string& format) const
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

  void help_system::get_help_text_from_file (const std::string& fname,
                                             std::string& text,
                                             std::string& format) const
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

  std::string help_system::init_built_in_docstrings_file (void)
  {
    std::string df = sys::env::getenv ("OCTAVE_BUILT_IN_DOCSTRINGS_FILE");

    std::string dir_sep = sys::file_ops::dir_sep_str ();

    if (df.empty ())
      df = config::oct_etc_dir () + dir_sep + "built-in-docstrings";

    return df;
  }

  std::string help_system::init_doc_cache_file (void)
  {
    std::string def_file = config::prepend_octave_home (OCTAVE_DOC_CACHE_FILE);

    std::string env_file = sys::env::getenv ("OCTAVE_DOC_CACHE_FILE");

    return (env_file.empty () ? def_file : env_file);
  }

  std::string help_system::init_info_file (void)
  {
    std::string std_info_file = config::prepend_octave_home (OCTAVE_INFOFILE);

    std::string oct_info_file = sys::env::getenv ("OCTAVE_INFO_FILE");

    return (oct_info_file.empty () ? std_info_file : oct_info_file);
  }

  std::string help_system::init_info_program (void)
  {
    std::string info_prog = sys::env::getenv ("OCTAVE_INFO_PROGRAM");

    if (info_prog.empty ())
      info_prog = "info";

    return info_prog;
  }

  std::string help_system::init_texi_macros_file (void)
  {
    std::string def_file
      = config::prepend_octave_home (OCTAVE_TEXI_MACROS_FILE);

    std::string env_file = sys::env::getenv ("OCTAVE_TEXI_MACROS_FILE");

    return (env_file.empty () ? def_file : env_file);
  }

  // Return a vector of all functions from this file,
  // for use in command line auto-completion.
  string_vector help_system::local_functions (void) const
  {
    string_vector retval;

    call_stack& cs = m_interpreter.get_call_stack ();

    octave_user_code *curr_fcn = cs.caller_user_code ();

    if (! curr_fcn)
      return retval;

    // All subfunctions are listed in the top-level function of this file.
    while (curr_fcn->is_subfunction ())
      {
        symbol_table::scope *pscope = curr_fcn->parent_fcn_scope ();
        curr_fcn = pscope->function ();
      }

    // Get subfunctions.
    const std::list<std::string> names = curr_fcn->subfunction_names ();

    size_t sz = names.size ();
    retval.resize (sz);

    // Loop over them.
    size_t i = 0;
    for (const auto& nm : names)
      retval(i++) = nm;

    return retval;
  }

  bool help_system::raw_help_from_symbol_table (const std::string& nm,
                                                std::string& h, std::string& w,
                                                bool& symbol_found) const
  {
    bool retval = false;

    symbol_table& symtab = m_interpreter.get_symbol_table ();

    octave_value val = symtab.find_function (nm);

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

  bool help_system::raw_help_from_file (const std::string& nm,
                                        std::string& h, std::string& file,
                                        bool& symbol_found) const
  {
    bool retval = false;

    h = octave::get_help_from_file (nm, symbol_found, file);

    if (h.length () > 0)
      retval = true;

    return retval;
  }

  bool
  help_system::raw_help_from_docstrings_file (const std::string& nm,
                                              std::string& h,
                                              bool& symbol_found) const
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
        std::ifstream file (m_built_in_docstrings_file.c_str (),
                            std::ios::in | std::ios::binary);

        if (! file)
          error ("failed to open docstrings file: %s",
                 m_built_in_docstrings_file.c_str ());

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
            std::streamoff len;

            if (! file.eof ())
              len = file.tellg () - beg - 1;
            else
              {
                file.seekg (0, file.end);
                len = file.tellg () - beg - 1;
                file.setstate (file.eofbit);  // reset eof flag
              }

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

        std::ifstream file (m_built_in_docstrings_file.c_str (),
                            std::ios::in | std::ios::binary);

        if (! file)
          error ("failed to open docstrings file: %s",
                 m_built_in_docstrings_file.c_str ());

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

  // FIXME: It's not likely that this does the right thing now.

  string_vector make_name_list (void)
  {
    octave::help_system& help_sys
      = octave::__get_help_system__ ("make_name_list");

    return help_sys.make_name_list ();
  }
}

DEFMETHOD (get_help_text, interp, args, ,
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

  octave::help_system& help_sys = interp.get_help_system ();

  std::string text, format;

  help_sys.get_help_text (name, text, format);

  return ovl (text, format);
}

DEFMETHOD (get_help_text_from_file, interp, args, ,
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

  octave::help_system& help_sys = interp.get_help_system ();

  std::string text, format;

  help_sys.get_help_text_from_file (fname, text, format);

  return ovl (text, format);
}

// Return a cell array of strings containing the names of all operators.

DEFUN (__operators__, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} __operators__ ()
Undocumented internal function.
@end deftypefn */)
{
  return ovl (Cell (operator_names));
}

// Return a cell array of strings containing the names of all keywords.
// iskeyword() function is located in lex.ll and is based on what the parser
// thinks is a keyword.

DEFALIAS (__keywords__, iskeyword)

// Return a cell array of strings with the names of all builtin functions.

DEFMETHOD (__builtins__, interp, , ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __builtins__ ()
Undocumented internal function.
@end deftypefn */)
{
  octave::symbol_table& symtab = interp.get_symbol_table ();

  const string_vector bif = symtab.built_in_function_names ();

  return ovl (Cell (bif));
}

DEFMETHOD (localfunctions, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} localfunctions ()
Return a list of all local functions, i.e., subfunctions, within the current
file.

The return value is a column cell array of function handles to all local
functions accessible from the function from which @code{localfunctions} is
called.  Nested functions are @emph{not} included in the list.

If the call is from the command line, an anonymous function, or a script,
the return value is an empty cell array.

@seealso{functions}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  Cell retval;

  // Find the main function we are in.
  octave::call_stack& cs = interp.get_call_stack ();
  octave_user_code *parent_fcn = cs.debug_user_code ();

  if (! parent_fcn)
    return ovl (retval);

  // Find the subfunctions of this function.
  // 1) subfunction_names contains only valid subfunctions
  // 2) subfunctions contains both nested functions and subfunctions
  const std::list<std::string> names = parent_fcn->subfunction_names ();
  const std::map<std::string, octave_value> h = parent_fcn->subfunctions ();

  size_t sz = names.size ();
  retval.resize (dim_vector (sz, 1));

  // loop over them.
  size_t i = 0;
  for (const auto& nm : names)
    {
      std::map<std::string, octave_value>::const_iterator nm_fcn = h.find (nm);
      if (nm_fcn != h.end ())
        retval(i++) = octave_value (new octave_fcn_handle (nm_fcn->second, nm));
    }

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
%!   assert (d{1} (3), 4);
%!   assert (d{2} (3), 6);
%! unwind_protect_cleanup
%!   unlink (f);
%! end_unwind_protect
*/

DEFMETHOD (__which__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} __which__ (@var{name}, @dots{})
Undocumented internal function.
@end deftypefn */)
{
  octave::help_system& help_sys = interp.get_help_system ();

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

      std::string file = help_sys.which (name, type);

      names(i) = name;
      files(i) = file;
      types(i) = type;
    }

  m.assign ("name", names);
  m.assign ("file", files);
  m.assign ("type", types);

  return ovl (m);
}

// Return a cell array of strings containing the names of all
// functions available in DIRECTORY.  If no directory is given, search
// the current path.

DEFMETHOD (__list_functions__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{retval} =} __list_functions__ ()
@deftypefnx {} {@var{retval} =} __list_functions__ (@var{directory})
Return a list of all functions (.m and .oct functions) in the load path.

If the optional argument @var{directory} is given then list only the functions
in that directory.
@seealso{path}
@end deftypefn */)
{
  octave_value retval;

  octave::load_path& lp = interp.get_load_path ();

  if (args.length () == 0)
    {
      // Get list of all functions
      string_vector ffl = lp.fcn_names ();
      string_vector afl = octave::autoloaded_functions ();

      retval = Cell (ffl.append (afl));
    }
  else
    {
      std::string dir = args(0).xstring_value ("__list_functions__: DIRECTORY argument must be a string");

      string_vector fl = lp.files (dir, true);

      // Return a sorted list with unique entries (in case of .m and .oct
      // versions of the same function in a given directory, for example).
      fl.sort (true);

      retval = Cell (fl);
    }

  return retval;
}

DEFMETHOD (built_in_docstrings_file, interp, args, nargout,
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
  octave::help_system& help_sys = interp.get_help_system ();

  return help_sys.built_in_docstrings_file (args, nargout);
}

DEFMETHOD (doc_cache_file, interp, args, nargout,
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
  octave::help_system& help_sys = interp.get_help_system ();

  return help_sys.doc_cache_file (args, nargout);
}

DEFMETHOD (info_file, interp, args, nargout,
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
  octave::help_system& help_sys = interp.get_help_system ();

  return help_sys.info_file (args, nargout);
}

DEFMETHOD (info_program, interp, args, nargout,
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
  octave::help_system& help_sys = interp.get_help_system ();

  return help_sys.info_program (args, nargout);
}

DEFMETHOD (makeinfo_program, interp, args, nargout,
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
  octave::help_system& help_sys = interp.get_help_system ();

  return help_sys.makeinfo_program (args, nargout);
}

DEFMETHOD (suppress_verbose_help_message, interp, args, nargout,
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
  octave::help_system& help_sys = interp.get_help_system ();

  return help_sys.suppress_verbose_help_message (args, nargout);
}

DEFMETHOD (texi_macros_file, interp, args, nargout,
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
  octave::help_system& help_sys = interp.get_help_system ();

  return help_sys.texi_macros_file (args, nargout);
}
