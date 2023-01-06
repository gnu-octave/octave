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

#include <cstdio>
#include <cstring>

#include <iomanip>
#include <list>
#include <set>
#include <string>

#include "file-stat.h"
#include "oct-env.h"
#include "file-ops.h"
#include "glob-match.h"
#include "lo-regexp.h"
#include "str-vec.h"

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "event-manager.h"
#include "help.h"
#include "input.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "lex.h"
#include "load-path.h"
#include "octave-preserve-stream-state.h"
#include "oct-map.h"
#include "ovl.h"
#include "ov.h"
#include "ov-class.h"
#include "ov-usr-fcn.h"
#include "pager.h"
#include "parse.h"
#include "pt-eval.h"
#include "syminfo.h"
#include "symtab.h"
#include "sysdep.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// Attributes of variables and functions.

// Is this octave_value a valid function?

octave_function *
is_valid_function (const std::string& fcn_name,
                   const std::string& warn_for, bool warn)
{
  octave_function *ans = nullptr;

  if (! fcn_name.empty ())
    {
      octave::symbol_table& symtab = octave::__get_symbol_table__ ();

      octave_value val = symtab.find_function (fcn_name);

      if (val.is_defined ())
        ans = val.function_value (true);
    }

  // FIXME: Should this be "err" and "error_for", rather than warn?
  if (! ans && warn)
    error ("%s: the symbol '%s' is not valid as a function",
           warn_for.c_str (), fcn_name.c_str ());

  return ans;
}

octave_function *
is_valid_function (const octave_value& arg,
                   const std::string& warn_for, bool warn)
{
  octave_function *ans = nullptr;

  std::string fcn_name;

  if (arg.is_string ())
    {
      fcn_name = arg.string_value ();

      ans = is_valid_function (fcn_name, warn_for, warn);
    }
  else if (warn)
    // FIXME: Should this be "err" and "error_for", rather than warn?
    error ("%s: argument must be a string containing function name",
           warn_for.c_str ());

  return ans;
}

OCTAVE_BEGIN_NAMESPACE(octave)

DEFMETHOD (isglobal, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isglobal (@var{name})
Return true if @var{name} is a globally visible variable.

For example:

@example
@group
global x
isglobal ("x")
   @result{} 1
@end group
@end example
@seealso{isvarname, exist}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  std::string name = args(0).xstring_value ("isglobal: NAME must be a string");

  return ovl (interp.isglobal (name));
}

/*
%!test
%! global x;
%! assert (isglobal ("x"), true);
%! clear -global x;  # cleanup after test

%!error isglobal ()
%!error isglobal ("a", "b")
%!error isglobal (1)
*/

static int
symbol_exist (interpreter& interp, const std::string& name,
              const std::string& type = "any")
{
  if (iskeyword (name))
    return 0;

  bool search_any = type == "any";
  bool search_var = type == "var";
  bool search_dir = type == "dir";
  bool search_file = type == "file";
  bool search_builtin = type == "builtin";
  bool search_class = type == "class";

  if (! (search_any || search_var || search_dir || search_file
         || search_builtin || search_class))
    error (R"(exist: unrecognized type argument "%s")", type.c_str ());

  if (search_any || search_var)
    {
      octave_value val = interp.varval (name);

      if (val.is_constant () || val.isobject ()
          || val.is_function_handle ()
          || val.is_anonymous_function ()
          || val.is_inline_function ())
        return 1;

      if (search_var)
        return 0;
    }

  symbol_table& symtab = interp.get_symbol_table ();

  // We shouldn't need to look in the global symbol table, since any name
  // that is visible in the current scope will be in the local symbol table.

  if (search_any || search_file || search_dir || search_class)
    {
      bool have_fcn_ext = false;

      std::string xname = name;
      std::string ext;

      std::size_t pos = name.rfind ('.');

      if (pos != std::string::npos)
        {
          ext = name.substr (pos+1);

          if (ext == "m" || ext == "oct" || ext == "mex")
            {
              xname = name.substr (0, pos);
              have_fcn_ext = true;
            }
        }

      std::string file_name;

      if (search_any || search_file || search_class)
        {
          load_path& lp = interp.get_load_path ();

          // Look for class constructor first
          file_name = lp.find_method (xname, xname);

          if (have_fcn_ext && ! file_name.empty ())
            {
              // Verify extension of file_name found matches ext of name.
              pos = file_name.rfind ('.');

              if (pos != std::string::npos)
                {
                  std::string fext = file_name.substr (pos+1);

                  if (ext != fext)
                    file_name = "";
                }
            }

          if (search_any && file_name.empty ())
            {
              // Command line function which Matlab does not support
              octave_value val = symtab.find_cmdline_function (xname);

              if (val.is_defined ())
                return 103;
            }

          // Autoloads can only have simple names without extensions.
          if (! have_fcn_ext && file_name.empty ())
            {
              tree_evaluator& tw = interp.get_evaluator ();

              file_name = tw.lookup_autoload (name);
            }

          // If nothing found, look for function using original name.
          if (file_name.empty ())
            file_name = lp.find_fcn (name);
        }

      std::size_t len = file_name.length ();

      if (len > 0 && (search_any || search_file || search_class))
        {
          if (search_any || search_file)
            {
              if (len > 4 && (file_name.substr (len-4) == ".oct"
                              || file_name.substr (len-4) == ".mex"))
                return 3;
            }

          if (search_class)
            {
              octave_value oval = symtab.find_function (name);
              if (oval.is_defined () && oval.is_classdef_meta ())
                return 8;
              else
                return 0;
            }

          return 2;
        }

      // Nothing found in symbol table, try searching in path
      file_name = file_in_path (name, "");

      if (file_name.empty ())
        file_name = name;

      // "stat" doesn't work on UNC shares and drive letters.
      if ((search_any || search_file) && drive_or_unc_share (file_name))
        return 7;

      sys::file_stat fs (file_name);

      if (fs)
        {
          if (search_any || search_file)
            {
              if (fs.is_dir ())
                return 7;

              len = file_name.length ();

              if (len > 4 && (file_name.substr (len-4) == ".oct"
                              || file_name.substr (len-4) == ".mex"))
                return 3;
              else
                return 2;
            }
          else if (search_dir && fs.is_dir ())
            return 7;
        }

      if (search_file || search_dir)
        return 0;
    }

  if ((search_any || search_builtin)
      && symtab.is_built_in_function_name (name))
    return 5;

  return 0;
}

int
symbol_exist (const std::string& name, const std::string& type)
{
  octave::interpreter& interp = octave::__get_interpreter__ ();

  return octave::symbol_exist (interp, name, type);
}


#define GET_IDX(LEN)                                                    \
  static_cast<int> (((LEN)-1) * static_cast<double> (rand ()) / RAND_MAX)

std::string
unique_symbol_name (const std::string& basename)
{
  static const std::string alpha
    = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

  static std::size_t len = alpha.length ();

  std::string nm = basename + alpha[GET_IDX (len)];

  std::size_t pos = nm.length ();

  if (nm.substr (0, 2) == "__")
    nm.append ("__");

  octave::interpreter& interp = octave::__get_interpreter__ ();

  while (symbol_exist (interp, nm, "any"))
    nm.insert (pos++, 1, alpha[GET_IDX (len)]);

  return nm;
}

DEFMETHOD (exist, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{c} =} exist (@var{name})
@deftypefnx {} {@var{c} =} exist (@var{name}, @var{type})
Check for the existence of @var{name} as a variable, function, file, directory,
or class.

The return code @var{c} is one of

@table @asis
@item 1
@var{name} is a variable.

@item 2
@var{name} is an absolute filename, an ordinary file in Octave's @code{path},
or (after appending @samp{.m}) a function file in Octave's @code{path}.

@item 3
@var{name} is a @samp{.oct} or @samp{.mex} file in Octave's @code{path}.

@item 5
@var{name} is a built-in function.

@item 7
@var{name} is a directory.

@item 8
@var{name} is a classdef class.

@item 103
@var{name} is a function not associated with a file (entered on the command
line).

@item 0
@var{name} does not exist.
@end table

If the optional argument @var{type} is supplied, check only for symbols of the
specified type.  Valid types are

@table @asis
@item @qcode{"var"}
Check only for variables.

@item @qcode{"builtin"}
Check only for built-in functions.

@item @qcode{"dir"}
Check only for directories.

@item @qcode{"file"}
Check only for files and directories.

@item @qcode{"class"}
Check only for classdef classes.
@end table

If no type is given, and there are multiple possible matches for name,
@code{exist} will return a code according to the following priority list:
variable, built-in function, oct-file, directory, file, class.

@code{exist} returns 2 if a regular file called @var{name} is present in
Octave's search path.  For information about other types of files not on the
search path use some combination of the functions @code{file_in_path} and
@code{stat} instead.

Programming Note: If @var{name} is implemented by a buggy .oct/.mex file,
calling @var{exist} may cause Octave to crash.  To maintain high performance,
Octave trusts .oct/.mex files instead of @nospell{sandboxing} them.

@seealso{file_in_loadpath, file_in_path, dir_in_loadpath, stat}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  // For compatibility with undocumented Matlab behavior, return 0 if
  // there is an empty built-in object as the only argument.
  if (args(0).builtin_type () != btyp_unknown && args(0).isempty ())
    return ovl (0);

  // Also for compatibility, return 0 if the second argument is an empty
  // built-in object.
  if (nargin == 2 && args(1).builtin_type () != btyp_unknown
      && args(1).isempty ())
    return ovl (0);

  std::string name = args(0).xstring_value ("exist: NAME must be a string");

  if (nargin == 2)
    {
      std::string type
        = args(1).xstring_value ("exist: TYPE must be a string");

      return ovl (symbol_exist (interp, name, type));
    }
  else
    return ovl (symbol_exist (interp, name));
}

/*
%!shared dirtmp, __var1
%! dirtmp = P_tmpdir ();
%! __var1 = 1;

%!assert (exist ("__%Highly_unlikely_name%__"), 0)
%!assert (exist ("__var1"), 1)
%!assert (exist ("__var1", "var"), 1)
%!assert (exist ("__var1", "builtin"), 0)
%!assert (exist ("__var1", "dir"), 0)
%!assert (exist ("__var1", "file"), 0)
%!assert (exist ("__var1", "class"), 0)

%!testif ; isunix ()
%! assert (exist ("/bin/sh"), 2);
%! assert (exist ("/bin/sh", "file"), 2);
%! assert (exist ("/bin/sh", "dir"), 0);
%! assert (exist ("/dev/null"), 2);
%! assert (exist ("/dev/null", "file"), 2);
%! assert (exist ("/dev/null", "dir"), 0);

%!assert (exist ("print_usage"), 2)
%!assert (exist ("print_usage.m"), 2)
%!assert (exist ("print_usage", "file"), 2)
%!assert (exist ("print_usage", "dir"), 0)

## Don't search path for rooted relative filenames
%!assert (exist ("plot.m", "file"), 2)
%!assert (exist ("./plot.m", "file"), 0)
%!assert (exist ("./%nonexistentfile%", "file"), 0)
%!assert (exist ("%nonexistentfile%", "file"), 0)

## Don't search path for absolute filenames
%!test
%! tname = tempname ();
%! unwind_protect
%!   ## open/close file to create it, equivalent of touch
%!   fid = fopen (tname, "w");
%!   fclose (fid);
%!   [~, fname] = fileparts (tname);
%!   assert (exist (fullfile (tempdir (), fname), "file"), 2);
%! unwind_protect_cleanup
%!   unlink (tname);
%! end_unwind_protect
%! assert (exist (fullfile (pwd (), "%nonexistentfile%"), "file"), 0);

%!assert (exist ("fftw"), 3)
%!assert (exist ("fftw.oct"), 3)
%!assert (exist ("fftw", "file"), 3)
%!assert (exist ("fftw", "builtin"), 0)

%!assert (exist ("ftp"), 2)
%!assert (exist ("ftp.m"), 2)
%!assert (exist ("@ftp/ftp"), 2)
%!assert (exist ("@ftp/ftp.m"), 2)
%!assert (exist ("ftp", "class"), 0)

%!assert (exist ("inputParser"), 2)
%!assert (exist ("inputParser.m"), 2)
%!assert (exist ("inputParser", "class"), 8)

%!assert (exist ("sin"), 5)
%!assert (exist ("sin", "builtin"), 5)
%!assert (exist ("sin", "file"), 0)

%!assert (exist (dirtmp), 7)
%!assert (exist (dirtmp, "dir"), 7)
%!assert (exist (dirtmp, "file"), 7)

%!error exist ()
%!error exist (1,2,3)
%!error <TYPE must be a string> exist ("a", 1)
%!error <NAME must be a string> exist (1)
%!error <unrecognized type argument "foobar"> exist ("a", "foobar")

*/

// Variable values.

static bool
wants_local_change (const octave_value_list& args, int& nargin)
{
  bool retval = false;

  if (nargin == 2)
    {
      if (! args(1).is_string () || args(1).string_value () != "local")
        error_with_cfn (R"(second argument must be "local")");

      nargin = 1;
      retval = true;
    }

  return retval;
}

static octave::unwind_protect *
curr_fcn_unwind_protect_frame (void)
{
  octave::tree_evaluator& tw = octave::__get_evaluator__ ();

  return tw.curr_fcn_unwind_protect_frame ();
}

template <typename T>
static bool
try_local_protect (T& var)
{
  octave::unwind_protect *frame = curr_fcn_unwind_protect_frame ();

  if (frame)
    {
      frame->protect_var (var);
      return true;
    }
  else
    return false;
}

octave_value
set_internal_variable (bool& var, const octave_value_list& args,
                       int nargout, const char *nm)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargout > 0 || nargin == 0)
    retval = var;

  if (wants_local_change (args, nargin))
    {
      if (! try_local_protect (var))
        warning (R"("local" has no effect outside a function)");
    }

  if (nargin > 1)
    print_usage ();

  if (nargin == 1)
    {
      bool bval = args(0).xbool_value ("%s: argument must be a logical value", nm);

      var = bval;
    }

  return retval;
}

octave_value
set_internal_variable (char& var, const octave_value_list& args,
                       int nargout, const char *nm)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargout > 0 || nargin == 0)
    retval = var;

  if (wants_local_change (args, nargin))
    {
      if (! try_local_protect (var))
        warning (R"("local" has no effect outside a function)");
    }

  if (nargin > 1)
    print_usage ();

  if (nargin == 1)
    {
      std::string sval = args(0).xstring_value ("%s: argument must be a single character", nm);

      switch (sval.length ())
        {
        case 1:
          var = sval[0];
          break;

        case 0:
          var = '\0';
          break;

        default:
          error ("%s: argument must be a single character", nm);
          break;
        }
    }

  return retval;
}

octave_value
set_internal_variable (int& var, const octave_value_list& args,
                       int nargout, const char *nm,
                       int minval, int maxval)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargout > 0 || nargin == 0)
    retval = var;

  if (wants_local_change (args, nargin))
    {
      if (! try_local_protect (var))
        warning (R"("local" has no effect outside a function)");
    }

  if (nargin > 1)
    print_usage ();

  if (nargin == 1)
    {
      int ival = args(0).xint_value ("%s: argument must be an integer value", nm);

      if (ival < minval)
        error ("%s: arg must be greater than %d", nm, minval);
      if (ival > maxval)
        error ("%s: arg must be less than or equal to %d", nm, maxval);

      var = ival;
    }

  return retval;
}

octave_value
set_internal_variable (double& var, const octave_value_list& args,
                       int nargout, const char *nm,
                       double minval, double maxval)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargout > 0 || nargin == 0)
    retval = var;

  if (wants_local_change (args, nargin))
    {
      if (! try_local_protect (var))
        warning (R"("local" has no effect outside a function)");
    }

  if (nargin > 1)
    print_usage ();

  if (nargin == 1)
    {
      double dval = args(0).xscalar_value ("%s: argument must be a scalar value", nm);

      if (dval < minval)
        error ("%s: argument must be greater than %g", nm, minval);
      if (dval > maxval)
        error ("%s: argument must be less than or equal to %g", nm, maxval);

      var = dval;
    }

  return retval;
}

octave_value
set_internal_variable (std::string& var, const octave_value_list& args,
                       int nargout, const char *nm, bool empty_ok)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargout > 0 || nargin == 0)
    retval = var;

  if (wants_local_change (args, nargin))
    {
      if (! try_local_protect (var))
        warning (R"("local" has no effect outside a function)");
    }

  if (nargin > 1)
    print_usage ();

  if (nargin == 1)
    {
      std::string sval = args(0).xstring_value ("%s: first argument must be a string", nm);

      if (! empty_ok && sval.empty ())
        error ("%s: value must not be empty", nm);

      var = sval;
    }

  return retval;
}

octave_value
set_internal_variable (int& var, const octave_value_list& args,
                       int nargout, const char *nm, const char **choices)
{
  octave_value retval;
  int nchoices = 0;
  while (choices[nchoices] != nullptr)
    nchoices++;

  int nargin = args.length ();

  error_unless (var < nchoices);

  if (nargout > 0 || nargin == 0)
    retval = choices[var];

  if (wants_local_change (args, nargin))
    {
      if (! try_local_protect (var))
        warning (R"("local" has no effect outside a function)");
    }

  if (nargin > 1)
    print_usage ();

  if (nargin == 1)
    {
      std::string sval = args(0).xstring_value ("%s: first argument must be a string", nm);

      int i = 0;
      for (; i < nchoices; i++)
        {
          if (sval == choices[i])
            {
              var = i;
              break;
            }
        }
      if (i == nchoices)
        error (R"(%s: value not allowed ("%s"))", nm, sval.c_str ());
    }

  return retval;
}

octave_value
set_internal_variable (std::string& var, const octave_value_list& args,
                       int nargout, const char *nm, const char **choices)
{
  octave_value retval;
  int nchoices = 0;
  while (choices[nchoices] != nullptr)
    nchoices++;

  int nargin = args.length ();

  if (nargout > 0 || nargin == 0)
    retval = var;

  if (wants_local_change (args, nargin))
    {
      if (! try_local_protect (var))
        warning (R"("local" has no effect outside a function)");
    }

  if (nargin > 1)
    print_usage ();

  if (nargin == 1)
    {
      std::string sval = args(0).xstring_value ("%s: first argument must be a string", nm);

      int i = 0;
      for (; i < nchoices; i++)
        {
          if (sval == choices[i])
            {
              var = sval;
              break;
            }
        }
      if (i == nchoices)
        error (R"(%s: value not allowed ("%s"))", nm, sval.c_str ());
    }

  return retval;
}

// NOTE: Calling Fmlock directly (without an associated stack frame)
// will probably not do what you expect because it will lock the calling
// function.  You should use interpreter::mlock directly if you want to
// lock a .oct function.  For .mex, you would normally use mexLock.
//
// FIXME: with the current implementation, calling "builtin ('mlock')"
// will also not do what you expect.  Is there any reasonable way to fix
// that?

DEFMETHOD (mlock, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} mlock ()
Lock the current function into memory so that it can't be removed with
@code{clear}.
@seealso{munlock, mislocked, persistent, clear}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  interp.mlock (true);

  return ovl ();
}

DEFMETHOD (munlock, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} munlock ()
@deftypefnx {} {} munlock (@var{fcn})
Unlock the named function @var{fcn} so that it may be removed from memory with
@code{clear}.

If no function is named then unlock the current function.
@seealso{mlock, mislocked, persistent, clear}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  if (nargin == 1)
    {
      std::string name
        = args(0).xstring_value ("munlock: FCN must be a string");

      interp.munlock (name);
    }
  else
    interp.munlock (true);

  return ovl ();
}

DEFMETHOD (mislocked, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{tf} =} mislocked ()
@deftypefnx {} {@var{tf} =} mislocked (@var{fcn})
Return true if the named function @var{fcn} is locked in memory.

If no function is named then return true if the current function is locked.
@seealso{mlock, munlock, persistent, clear}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  if (nargin == 1)
    {
      std::string name
        = args(0).xstring_value ("mislocked: FCN must be a string");

      return ovl (interp.mislocked (name));
    }
  else
    return ovl (interp.mislocked (true));
}

// Deleting names from the symbol tables.

static inline bool
name_matches_any_pattern (const std::string& nm, const string_vector& argv,
                          int argc, int idx, bool have_regexp = false)
{
  bool retval = false;

  for (int k = idx; k < argc; k++)
    {
      std::string patstr = argv[k];
      if (! patstr.empty ())
        {
          if (have_regexp)
            {
              if (regexp::is_match (patstr, nm))
                {
                  retval = true;
                  break;
                }
            }
          else
            {
              glob_match pattern (patstr);

              if (pattern.match (nm))
                {
                  retval = true;
                  break;
                }
            }
        }
    }

  return retval;
}

static inline void
maybe_warn_exclusive (bool exclusive)
{
  if (exclusive)
    warning ("clear: ignoring --exclusive option");
}

static void
do_clear_functions (interpreter& interp,
                    const string_vector& argv, int argc, int idx,
                    bool exclusive = false)
{
  if (idx == argc)
    interp.clear_functions ();
  else
    {
      if (exclusive)
        {
          std::list<std::string> fcns = interp.user_function_names ();

          for (const auto& name : fcns)
            {
              if (! name_matches_any_pattern (name, argv, argc, idx))
                interp.clear_function (name);
            }
        }
      else
        {
          while (idx < argc)
            interp.clear_function_pattern (argv[idx++]);
        }
    }
}

static void
do_clear_globals (interpreter& interp,
                  const string_vector& argv, int argc, int idx,
                  bool exclusive = false)
{
  if (idx == argc)
    {
      std::list<std::string> gvars = interp.global_variable_names ();

      for (const auto& name : gvars)
        {
          interp.clear_variable (name);
          interp.clear_global_variable (name);
        }
    }
  else
    {
      if (exclusive)
        {
          std::list<std::string> gvars = interp.global_variable_names ();

          for (const auto& name : gvars)
            {
              if (! name_matches_any_pattern (name, argv, argc, idx))
                {
                  interp.clear_variable (name);
                  interp.clear_global_variable (name);
                }
            }
        }
      else
        {
          while (idx < argc)
            {
              std::string pattern = argv[idx++];

              interp.clear_variable_pattern (pattern);
              interp.clear_global_variable_pattern (pattern);
            }
        }
    }
}

static void
do_clear_variables (interpreter& interp,
                    const string_vector& argv, int argc, int idx,
                    bool exclusive = false, bool have_regexp = false)
{
  if (idx == argc)
    interp.clear_variables ();
  else
    {
      if (exclusive)
        {
          std::list<std::string> lvars = interp.variable_names ();

          for (const auto& name : lvars)
            {
              if (! name_matches_any_pattern (name, argv, argc, idx,
                                              have_regexp))
                interp.clear_variable (name);
            }
        }
      else
        {
          if (have_regexp)
            while (idx < argc)
              interp.clear_variable_regexp (argv[idx++]);
          else
            while (idx < argc)
              interp.clear_variable_pattern (argv[idx++]);
        }
    }
}

static void
do_clear_symbols (interpreter& interp,
                  const string_vector& argv, int argc, int idx,
                  bool exclusive = false)
{
  if (idx == argc)
    {
      interp.clear_variables ();
    }
  else
    {
      if (exclusive)
        {
          // FIXME: is this really what we want, or do we
          // somehow want to only clear the functions that are not
          // shadowed by local variables?  It seems that would be a
          // bit harder to do.

          do_clear_variables (interp, argv, argc, idx, exclusive);
          do_clear_functions (interp, argv, argc, idx, exclusive);
        }
      else
        {
          while (idx < argc)
            interp.clear_symbol_pattern (argv[idx++]);
        }
    }
}

static void
do_matlab_compatible_clear (interpreter& interp,
                            const string_vector& argv, int argc, int idx)
{
  // This is supposed to be mostly Matlab compatible.

  for (; idx < argc; idx++)
    {
      if (argv[idx] == "all" && ! interp.is_local_variable ("all"))
        {
          interp.clear_all ();
        }
      else if (argv[idx] == "functions"
               && ! interp.is_local_variable ("functions"))
        {
          do_clear_functions (interp, argv, argc, ++idx);
        }
      else if (argv[idx] == "global"
               && ! interp.is_local_variable ("global"))
        {
          do_clear_globals (interp, argv, argc, ++idx);
        }
      else if (argv[idx] == "variables"
               && ! interp.is_local_variable ("variables"))
        {
          interp.clear_variables ();
        }
      else if (argv[idx] == "classes"
               && ! interp.is_local_variable ("classes"))
        {
          interp.clear_objects ();
          octave_class::clear_exemplar_map ();
          interp.clear_all ();
        }
      else
        {
          interp.clear_symbol_pattern (argv[idx]);
        }
    }
}

DEFMETHOD (clear, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} clear
@deftypefnx {} {} clear @var{pattern} @dots{}
@deftypefnx {} {} clear @var{options} @var{pattern} @dots{}
Delete the names matching the given @var{pattern}s thereby freeing memory.

The @var{pattern} may contain the following special characters:

@table @code
@item ?
Match any single character.

@item *
Match zero or more characters.

@item [ @var{list} ]
Match the list of characters specified by @var{list}.  If the first character
is @code{!} or @code{^}, match all characters except those specified by
@var{list}.  For example, the pattern @code{[a-zA-Z]} will match all lowercase
and uppercase alphabetic characters.
@end table

For example, the command

@example
clear foo b*r
@end example

@noindent
clears the name @code{foo} and all names that begin with the letter @samp{b}
and end with the letter @samp{r}.

If @code{clear} is called without any arguments, all user-defined variables
are cleared from the current workspace (i.e., local variables).  Any global
variables present will no longer be visible in the current workspace, but they
will continue to exist in the global workspace.  Functions are unaffected by
this form of @code{clear}.

The following options are available in both long and short form

@table @code
@item all, -all, -a
Clear all local and global user-defined variables, and all functions from the
symbol table.

@item -exclusive, -x
Clear variables that do @strong{not} match the following pattern.

@item functions, -functions, -f
Clear function names from the function symbol table.  Persistent variables
will be re-initialized to their default value unless the function has been
locked in memory with @code{mlock}.

@item global, -global, -g
Clear global variable names.

@item variables, -variables, -v
Clear local variable names.

@item classes, -classes, -c
Clear the class structure table and all objects.

@item -regexp, -r
The @var{pattern} arguments are treated as regular expressions and any matches
will be cleared.
@end table

With the exception of @option{-exclusive} and @option{-regexp}, all long
options can be used without the dash as well.  Note that, aside from
@option{-exclusive}, only one other option may appear.  All options must
appear before any patterns.

Programming Notes: The command @code{clear @var{name}} only clears the variable
@var{name} when both a variable and a (shadowed) function named @var{name}
are currently defined.  For example, suppose you have defined a function
@code{foo}, and then hidden it by performing the assignment @code{foo = 2}.
Executing the command @code{clear foo} once will clear the variable
definition and restore the definition of @code{foo} as a function.
Executing @code{clear foo} a second time will clear the function definition.

When a local variable name, which is linked to a global variable, is cleared
only the local copy of the variable is removed.  The global copy is untouched
and can be restored with @code{global @var{global_varname}}.  Conversely,
@code{clear -g @var{global_varname}} will remove both the local and global
variables.

@seealso{clearvars, who, whos, exist, mlock}
@end deftypefn */)
{
  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("clear");

  if (argc == 1)
    {
      do_clear_variables (interp, argv, argc, true);

      event_manager& evmgr = interp.get_event_manager ();

      evmgr.clear_workspace ();
    }
  else
    {
      int idx = 0;

      bool clear_all = false;
      bool clear_functions = false;
      bool clear_globals = false;
      bool clear_variables = false;
      bool clear_objects = false;
      bool exclusive = false;
      bool have_regexp = false;
      bool have_dash_option = false;

      while (++idx < argc)
        {
          if (argv[idx] == "-all" || argv[idx] == "-a")
            {
              if (have_dash_option)
                print_usage ();

              have_dash_option = true;
              clear_all = true;
            }
          else if (argv[idx] == "-exclusive" || argv[idx] == "-x")
            {
              exclusive = true;
            }
          else if (argv[idx] == "-functions" || argv[idx] == "-f")
            {
              if (have_dash_option)
                print_usage ();

              have_dash_option = true;
              clear_functions = true;
            }
          else if (argv[idx] == "-global" || argv[idx] == "-g")
            {
              if (have_dash_option)
                print_usage ();

              have_dash_option = true;
              clear_globals = true;
            }
          else if (argv[idx] == "-variables" || argv[idx] == "-v")
            {
              if (have_dash_option)
                print_usage ();

              have_dash_option = true;
              clear_variables = true;
            }
          else if (argv[idx] == "-classes" || argv[idx] == "-c")
            {
              if (have_dash_option)
                print_usage ();

              have_dash_option = true;
              clear_objects = true;
            }
          else if (argv[idx] == "-regexp" || argv[idx] == "-r")
            {
              if (have_dash_option)
                print_usage ();

              have_dash_option = true;
              have_regexp = true;
            }
          else
            break;
        }

      if (idx <= argc)
        {
          if (! have_dash_option && ! exclusive)
            do_matlab_compatible_clear (interp, argv, argc, idx);
          else
            {
              if (clear_all)
                {
                  maybe_warn_exclusive (exclusive);

                  if (++idx < argc)
                    warning ("clear: ignoring extra arguments after -all");

                  interp.clear_all ();
                }
              else if (have_regexp)
                {
                  do_clear_variables (interp, argv, argc, idx, exclusive, true);
                }
              else if (clear_functions)
                {
                  do_clear_functions (interp, argv, argc, idx, exclusive);
                }
              else if (clear_globals)
                {
                  do_clear_globals (interp, argv, argc, idx, exclusive);
                }
              else if (clear_variables)
                {
                  do_clear_variables (interp, argv, argc, idx, exclusive);
                }
              else if (clear_objects)
                {
                  interp.clear_objects ();
                  octave_class::clear_exemplar_map ();
                  interp.clear_all ();
                }
              else
                {
                  do_clear_symbols (interp, argv, argc, idx, exclusive);
                }
            }
        }
    }

  return ovl ();
}

/*
## This test must be wrapped in its own function or the 'clear' command will
## break the %!test environment.
%!function __test_clear_no_args__ ()
%!  global x;
%!  x = 3;
%!  clear
%!  assert (! exist ("x", "var"));  # x is not in the current workspace anymore
%!  global x;                       # but still lives in the global workspace
%!  assert (exist ("x", "var"));
%!endfunction

%!test
%! unwind_protect
%!   __test_clear_no_args__ ();
%! unwind_protect_cleanup
%!   clear -g x
%! end_unwind_protect

## Test that multiple options cannot be given
%!error clear -f -g
*/

static std::string Vmissing_function_hook = "__unimplemented__";

DEFUN (missing_function_hook, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} missing_function_hook ()
@deftypefnx {} {@var{old_val} =} missing_function_hook (@var{new_val})
@deftypefnx {} {@var{old_val} =} missing_function_hook (@var{new_val}, "local")
Query or set the internal variable that specifies the function to call
to provide extra information when an unknown identifier is referenced.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{missing_component_hook}
@end deftypefn */)
{
  return set_internal_variable (Vmissing_function_hook, args, nargout,
                                "missing_function_hook");
}

std::string
maybe_missing_function_hook (const std::string& name)
{
  octave::interpreter& interp = octave::__get_interpreter__ ();

  // Don't do this if we're handling errors.
  if (Vmissing_function_hook.empty ())
    return "";

  octave::symbol_table& symtab = interp.get_symbol_table ();

  octave_value val = symtab.find_function (Vmissing_function_hook);

  if (val.is_defined ())
    {
      // Ensure auto-restoration.
      octave::unwind_protect_var<std::string>
      restore_var (Vmissing_function_hook);

      // Clear the variable prior to calling the function.
      const std::string fcn_name = Vmissing_function_hook;
      Vmissing_function_hook.clear ();

      // Call.
      octave_value_list tmp = octave::feval (fcn_name, octave_value (name), 1);

      if (tmp.length () == 1 && tmp(0).is_string ())
        return tmp(0).string_value ();
    }

  return "";
}

DEFMETHOD (__varval__, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{value} =} __varval__ (@var{name})
Return the value of the variable @var{name} directly from the symbol table.

If @var{name} does not exist then nothing is returned, not even an empty matrix
(@code{[]}), since there would be no way to distinguish between a variable
not found in the symbol table and a variable who's value was @code{[]}.

A standard usage pattern is to code a @code{try}/@code{catch} block around a
call to @code{__varval__}.

Example Code

@example
@group
try
  @var{val} = __varval__ (@var{name});
catch
  ## No variable @var{name} found in symbol table
  @var{val} = NA;                  # Substitute Not Available (NA)
  error ("@var{name} not found");  # or, throw an error.
end_try_catch
@end group
@end example

Programming Note: The magic @var{name} @qcode{".argn."} will retrieve the text
of input arguments to a function and is used by @code{inputname} internally.
@seealso{inputname}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  std::string name = args(0).xstring_value ("__varval__: NAME must be a string");

  // We need this kluge to implement inputname in a .m file.
  if (name == ".argn.")
    {
      tree_evaluator& tw = interp.get_evaluator ();

      return tw.get_auto_fcn_var (stack_frame::ARG_NAMES);
    }

  return interp.varval (name);
}

static std::string Vmissing_component_hook;

DEFUN (missing_component_hook, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} missing_component_hook ()
@deftypefnx {} {@var{old_val} =} missing_component_hook (@var{new_val})
@deftypefnx {} {@var{old_val} =} missing_component_hook (@var{new_val}, "local")
Query or set the internal variable that specifies the function to call when
a component of Octave is missing.

This can be useful for packagers that may split the Octave installation into
multiple sub-packages, for example, to provide a hint to users for how to
install the missing components.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.

The hook function is expected to be of the form

@example
@var{fcn} (@var{component})
@end example

Octave will call @var{fcn} with the name of the function that requires the
component and a string describing the missing component.  The hook function
should return an error message to be displayed.
@seealso{missing_function_hook}
@end deftypefn */)
{
  return set_internal_variable (Vmissing_component_hook, args, nargout,
                                "missing_component_hook");
}

OCTAVE_END_NAMESPACE(octave)
