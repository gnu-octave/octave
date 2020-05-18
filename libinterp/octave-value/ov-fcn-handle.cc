////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2003-2020 The Octave Project Developers
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

#include <istream>
#include <list>
#include <ostream>
#include <sstream>
#include <string>
#include <vector>

#include "file-ops.h"
#include "oct-locbuf.h"

#include "defaults.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "file-stat.h"
#include "input.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "load-path.h"
#include "oct-env.h"
#include "oct-hdf5.h"
#include "oct-map.h"
#include "ov-base.h"
#include "ov-fcn-handle.h"
#include "ov-usr-fcn.h"
#include "parse.h"
#include "pr-output.h"
#include "pt-arg-list.h"
#include "pt-assign.h"
#include "pt-cmd.h"
#include "pt-eval.h"
#include "pt-exp.h"
#include "pt-idx.h"
#include "pt-misc.h"
#include "pt-pr-code.h"
#include "pt-stmt.h"
#include "syminfo.h"
#include "symscope.h"
#include "unwind-prot.h"
#include "variables.h"

#include "byte-swap.h"
#include "ls-ascii-helper.h"
#include "ls-hdf5.h"
#include "ls-oct-text.h"
#include "ls-oct-binary.h"
#include "ls-utils.h"


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_fcn_handle,
                                     "function handle",
                                     "function_handle");

const std::string octave_fcn_handle::anonymous ("@<anonymous>");

octave_fcn_handle::octave_fcn_handle (const octave::symbol_scope& scope,
                                      const std::string& n)
  : m_fcn (), m_obj (), m_name (n), m_scope (scope), m_is_nested (false),
    m_closure_frames (nullptr), m_dispatch_class ()
{
  if (! m_name.empty () && m_name[0] == '@')
    m_name = m_name.substr (1);

  size_t pos = m_name.find ('.');

  if (pos != std::string::npos)
    {
      // If we are looking at
      //
      //   obj . meth
      //
      // Store the object so that calling METH for OBJ will work even if
      // it is done outside of the scope whre OBJ was initially defined
      // or if OBJ is cleared before the method call is made through the
      // function handle.

      std::string obj_name = m_name.substr (0, pos);

      octave::interpreter& interp
        = octave::__get_interpreter__ ("octave_fcn_handle::octave_fcn_handle");

      octave_value val = interp.varval (obj_name);

      if (val.is_classdef_object ())
        m_obj = val;
    }
}

octave_fcn_handle::octave_fcn_handle (const octave::symbol_scope& scope,
                                      const octave_value& f,
                                      const std::string& n)
  : m_fcn (f), m_obj (), m_name (n), m_scope (scope), m_is_nested (false),
    m_closure_frames (nullptr), m_dispatch_class ()
{
  octave_user_function *uf = m_fcn.user_function_value (true);

  if (uf && m_name != anonymous)
    {
      octave::symbol_scope uf_scope = uf->scope ();

      if (uf_scope)
        uf_scope.cache_name (m_name);
    }

  if (uf && uf->is_nested_function () && ! uf->is_subfunction ())
    m_is_nested = true;
}

octave_fcn_handle::octave_fcn_handle (const octave_value& f,
                                      const std::string& n)
  : m_fcn (f), m_obj (), m_name (n), m_scope (), m_is_nested (false),
    m_closure_frames (nullptr), m_dispatch_class ()
{
  octave_user_function *uf = m_fcn.user_function_value (true);

  if (uf && m_name != anonymous)
    {
      octave::symbol_scope uf_scope = uf->scope ();

      if (uf_scope)
        uf_scope.cache_name (m_name);
    }

  if (uf && uf->is_nested_function () && ! uf->is_subfunction ())
    m_is_nested = true;
}

octave_fcn_handle::~octave_fcn_handle (void)
{
  if (m_closure_frames)
    {
      while (m_closure_frames->size () > 0)
        {
          octave::stack_frame *elt = m_closure_frames->back ();

          delete elt;

          m_closure_frames->pop_back ();
        }

      delete m_closure_frames;
    }
}

octave_value_list
octave_fcn_handle::subsref (const std::string& type,
                            const std::list<octave_value_list>& idx,
                            int nargout)
{
  octave_value_list retval;

  switch (type[0])
    {
    case '(':
      {
        int tmp_nargout = (type.length () > 1 && nargout == 0) ? 1 : nargout;

        retval = call (tmp_nargout, idx.front ());
      }
      break;

    case '{':
    case '.':
      {
        std::string tnm = type_name ();
        error ("%s cannot be indexed with %c", tnm.c_str (), type[0]);
      }
      break;

    default:
      panic_impossible ();
    }

  // FIXME: perhaps there should be an
  // octave_value_list::next_subsref member function?  See also
  // octave_builtin::subsref.

  if (idx.size () > 1)
    retval = retval(0).next_subsref (nargout, type, idx);

  return retval;
}

static void
err_invalid_fcn_handle (const std::string& name)
{
  error ("%s: invalid function handle", name.c_str ());
}

octave_value_list
octave_fcn_handle::call (int nargout, const octave_value_list& args)
{
  // FIXME: if m_name has a '.' in the name, lookup first component.  If
  // it is a classdef meta object, then build TYPE and IDX arguments and
  // make a subsref call using them.

  octave_value fcn_to_call = m_fcn;

  octave::interpreter& interp
    = octave::__get_interpreter__ ("octave_fcn_handle::call");

  if (! fcn_to_call.is_defined ())
    {
      // The following code is similar to part of
      // tree_evaluator::visit_index_expression but simpler because it
      // handles a more restricted case.

      octave::symbol_table& symtab = interp.get_symbol_table ();

      size_t pos = m_name.find ('.');

      if (pos != std::string::npos)
        {
          // We can have one of
          //
          //   pkg-list . fcn  (args)
          //   pkg-list . cls . meth (args)
          //   cls . meth  (args)

          // Evaluate package elements until we find a function,
          // classdef object, or classdef_meta object that is not a
          // package.  An object may only appear as the first element,
          // then it must be followed directly by a function name.

          size_t beg = 0;
          size_t end = pos;

          std::vector<std::string> idx_elts;

          while (true)
            {
              end = m_name.find ('.', beg);

              idx_elts.push_back (m_name.substr (beg, end-beg));

              if (end == std::string::npos)
                break;

              beg = end+1;
            }

          size_t n_elts = idx_elts.size ();

          bool have_object = false;
          octave_value partial_expr_val;

          if (m_obj.is_defined ())
            {
              // The first element was already defined elsewhere,
              // possibly in the scope where the function handle was
              // created.

              partial_expr_val = m_obj;

              if (m_obj.is_classdef_object ())
                have_object = true;
              else
                err_invalid_fcn_handle (m_name);
            }
          else
            {
              // Lazy evaluation.  The first element was not known to be
              // defined as an object in the scope where the handle was
              // created.  See if there is a definition in the current
              // scope.

              partial_expr_val = interp.varval (idx_elts[0]);
            }

          if (partial_expr_val.is_defined ())
            {
              if (! partial_expr_val.is_classdef_object () || n_elts != 2)
                err_invalid_fcn_handle (m_name);

              have_object = true;
            }
          else
            partial_expr_val
              = symtab.find_function (idx_elts[0], ovl (), m_scope);

          std::string type;
          std::list<octave_value_list> arg_list;

          for (size_t i = 1; i < n_elts; i++)
            {
              if (partial_expr_val.is_package ())
                {
                  if (have_object)
                    err_invalid_fcn_handle (m_name);

                  type = ".";
                  arg_list.push_back (ovl (idx_elts[i]));

                  try
                    {
                      // Silently ignore extra output values.

                      octave_value_list tmp_list
                        = partial_expr_val.subsref (type, arg_list, 0);

                      partial_expr_val
                        = tmp_list.length () ? tmp_list(0) : octave_value ();

                      if (partial_expr_val.is_cs_list ())
                        err_invalid_fcn_handle (m_name);

                      arg_list.clear ();
                    }
                  catch (octave::index_exception&)
                    {
                      err_invalid_fcn_handle (m_name);
                    }
                }
              else if (have_object || partial_expr_val.is_classdef_meta ())
                {
                  // Object or class name must be the next to the last
                  // element (it was the previous one, so if this is the
                  // final element, it should be a classdef method,
                  // but we'll let the classdef or classdef_meta subsref
                  // function sort that out.

                  if (i != n_elts-1)
                    err_invalid_fcn_handle (m_name);

                  type = ".(";
                  arg_list.push_back (ovl (idx_elts[i]));
                  arg_list.push_back (args);

                  return partial_expr_val.subsref (type, arg_list, nargout);
                }
              else
                err_invalid_fcn_handle (m_name);
            }

          // If we get here, we must have a function to call.

          if (! partial_expr_val.is_function ())
            err_invalid_fcn_handle (m_name);

          fcn_to_call = partial_expr_val;
        }
      else
        fcn_to_call = symtab.find_function (m_name, args, m_scope);
    }

  if (! fcn_to_call.is_defined ())
    err_invalid_fcn_handle (m_name);

  octave::stack_frame *closure_context = nullptr;

  if (m_closure_frames && m_closure_frames->size () > 0)
    closure_context = m_closure_frames->front ();

  octave::tree_evaluator& tw = interp.get_evaluator ();

  octave_function *of = fcn_to_call.function_value ();

  octave::unwind_protect frame;

  frame.add_method (tw, &octave::tree_evaluator::set_dispatch_class,
                    std::string ());

  tw.set_dispatch_class (m_dispatch_class);

  return of->call (tw, nargout, args, closure_context);
}

dim_vector
octave_fcn_handle::dims (void) const
{
  static dim_vector dv (1, 1);
  return dv;
}

octave_function * octave_fcn_handle::function_value (bool)
{
  if (m_fcn.is_defined ())
    return m_fcn.function_value ();

  octave::symbol_table& symtab
    = octave::__get_symbol_table__ ("octave_fcn_handle::set_fcn");

  // Cache this value so that the pointer will be valid as long as the
  // function handle object is valid.

  m_generic_fcn = symtab.find_function (m_name, octave_value_list (), m_scope);

  return (m_generic_fcn.is_defined ()
          ? m_generic_fcn.function_value () : nullptr);
}

octave_user_function * octave_fcn_handle::user_function_value (bool)
{
  if (m_fcn.is_defined ())
    return m_fcn.user_function_value ();

  octave::symbol_table& symtab
    = octave::__get_symbol_table__ ("octave_fcn_handle::set_fcn");

  // Cache this value so that the pointer will be valid as long as the
  // function handle object is valid.

  m_generic_fcn = symtab.find_user_function (m_name);

  return (m_generic_fcn.is_defined ()
          ? m_generic_fcn.user_function_value () : nullptr);
}

// Save call stack frames for handles to nested functions.

void
octave_fcn_handle::push_closure_context (octave::tree_evaluator& tw)
{
  if (! m_closure_frames)
    m_closure_frames = new std::list<octave::stack_frame *> ();

  octave::stack_frame& curr_frame = tw.get_current_stack_frame ();

  octave::stack_frame *dup_frame = curr_frame.dup ();

  if (! m_closure_frames->empty ())
    {
      octave::stack_frame *top_frame = m_closure_frames->back ();

      // Arrange for static and access links in the top stack frame (the
      // last one saved before this one) to point to the new duplicated
      // frame.  This way we will look up through the duplicated frames
      // when evaluating the function.

      top_frame->set_closure_links (dup_frame);
    }

  m_closure_frames->push_back (dup_frame);
}

octave_value
octave_fcn_handle::workspace (void) const
{
  if (m_name == anonymous)
    {
      octave_user_function *fu = m_fcn.user_function_value ();

      octave_scalar_map ws;

      if (fu)
        {
          for (const auto& nm_val : fu->local_var_init_vals ())
            ws.assign (nm_val.first, nm_val.second);
        }

      return ws;
    }
  else if (m_closure_frames)
    {
      octave_idx_type num_frames = m_closure_frames->size ();

      Cell ws_frames (num_frames, 1);

      octave_idx_type i = 0;

      for (auto elt : *m_closure_frames)
        {
          octave::symbol_info_list symbols = elt->all_variables ();

          octave_scalar_map ws;

          for (auto sym_name : symbols.names ())
            {
              octave_value val = symbols.varval (sym_name);

              if (val.is_defined ())
                ws.assign (sym_name, val);
            }

          ws_frames(i++) = ws;
        }

      return ws_frames;
    }

  return Cell ();
}

bool
octave_fcn_handle::is_equal_to (const octave_fcn_handle& h) const
{
  if (m_fcn.is_defined () && h.m_fcn.is_defined ())
    return m_fcn.is_copy_of (h.m_fcn);
  else if (m_fcn.is_undefined () && h.m_fcn.is_undefined ())
    return m_name == h.m_name;
  else
    return false;
}

bool
octave_fcn_handle::set_fcn (const std::string& octaveroot,
                            const std::string& fpath)
{
  if (octaveroot.length () != 0
      && fpath.length () >= octaveroot.length ()
      && fpath.substr (0, octaveroot.length ()) == octaveroot
      && octave::config::octave_exec_home () != octaveroot)
    {
      // First check if just replacing matlabroot is enough
      std::string str
        = (octave::config::octave_exec_home ()
           + fpath.substr (octaveroot.length ()));
      octave::sys::file_stat fs (str);

      if (fs.exists ())
        {
          size_t xpos = str.find_last_of (octave::sys::file_ops::dir_sep_chars ());

          std::string dir_name = str.substr (0, xpos);

          octave_value ov_fcn
            = octave::load_fcn_from_file (str, dir_name, "", "", m_name);

          if (ov_fcn.is_undefined ())
            error ("function handle points to non-existent function");

          m_fcn = octave_value (new octave_fcn_handle (ov_fcn, m_name));
        }
      else
        {
          // Next just search for it anywhere in the system path
          std::list<std::string> names;
          names.push_back (m_name + ".oct");
          names.push_back (m_name + ".mex");
          names.push_back (m_name + ".m");

          octave::load_path& lp
            = octave::__get_load_path__ ("octave_fcn_handle::set_fcn");

          octave::directory_path p (lp.system_path ());

          str = octave::sys::env::make_absolute (p.find_first_of (names));

          size_t xpos = str.find_last_of (octave::sys::file_ops::dir_sep_chars ());

          std::string dir_name = str.substr (0, xpos);

          octave_value ov_fcn
            = octave::load_fcn_from_file (str, dir_name, "", "", m_name);

          if (ov_fcn.is_undefined ())
            error ("function handle points to non-existent function");

          m_fcn = octave_value (new octave_fcn_handle (ov_fcn, m_name));
        }
    }
  else
    {
      if (fpath.length () > 0)
        {
          size_t xpos = fpath.find_last_of (octave::sys::file_ops::dir_sep_chars ());

          std::string dir_name = fpath.substr (0, xpos);

          octave_value ov_fcn
            = octave::load_fcn_from_file (fpath, dir_name, "", "", m_name);

          if (ov_fcn.is_undefined ())
            error ("function handle points to non-existent function");

          m_fcn = octave_value (new octave_fcn_handle (ov_fcn, m_name));
        }
      else
        {
          octave::symbol_table& symtab
            = octave::__get_symbol_table__ ("octave_fcn_handle::set_fcn");

          m_fcn = symtab.find_function (m_name);

          if (! m_fcn.is_function ())
            error ("function handle points to non-existent function");
        }
    }

  return true;
}

octave_value
octave_fcn_handle::convert_to_str_internal (bool, bool, char type) const
{
  std::ostringstream buf;
  print_raw (buf, true);
  return octave_value (buf.str (), type);
}

bool
octave_fcn_handle::save_ascii (std::ostream& os)
{
  if (m_name == anonymous)
    {
      if (m_fcn.is_undefined ())
        return false;

      octave_user_function *f = m_fcn.user_function_value ();

      octave_user_function::local_vars_map local_vars
        = f->local_var_init_vals ();

      size_t varlen = local_vars.size ();

      os << m_name << "\n";

      print_raw (os, true);
      os << "\n";

      if (varlen > 0)
        {
          os << "# length: " << varlen << "\n";

          for (const auto& nm_val : local_vars)
            {
              if (! save_text_data (os, nm_val.second, nm_val.first, false, 0))
                return ! os.fail ();
            }
        }
    }
  else
    {
      octave_function *f = function_value ();
      std::string fnm = (f ? f->fcn_file_name () : "");

      os << "# octaveroot: " << octave::config::octave_exec_home () << "\n";
      if (! fnm.empty ())
        os << "# path: " << fnm << "\n";
      os << m_name << "\n";
    }

  return true;
}

bool
octave_fcn_handle::parse_anon_fcn_handle (const std::string& fcn_text)
{
  bool success = true;

  octave::interpreter& interp
    = octave::__get_interpreter__ ("octave_fcn_handle::parse_anon_fcn_handle");

  int parse_status;
  octave_value anon_fcn_handle
    = interp.eval_string (fcn_text, true, parse_status);

  if (parse_status == 0)
    {
      octave_fcn_handle *fh = anon_fcn_handle.fcn_handle_value ();

      if (fh)
        {
          m_fcn = fh->m_fcn;

          octave_user_function *uf = m_fcn.user_function_value (true);

          if (uf)
            {
              octave::symbol_scope uf_scope = uf->scope ();

              if (uf_scope)
                uf_scope.cache_name (m_name);
            }
        }
      else
        success = false;
    }
  else
    success = false;

  return success;
}

bool
octave_fcn_handle::load_ascii (std::istream& is)
{
  bool success = true;

  std::streampos pos = is.tellg ();
  std::string octaveroot = extract_keyword (is, "octaveroot", true);
  if (octaveroot.empty ())
    {
      is.seekg (pos);
      is.clear ();
    }
  pos = is.tellg ();
  std::string fpath = extract_keyword (is, "path", true);
  if (fpath.empty ())
    {
      is.seekg (pos);
      is.clear ();
    }

  is >> m_name;

  if (m_name == anonymous)
    {
      skip_preceeding_newline (is);

      std::string buf;

      if (is)
        {

          // Get a line of text whitespace characters included, leaving
          // newline in the stream.
          buf = read_until_newline (is, true);

        }

      pos = is.tellg ();

      octave::unwind_protect_safe frame;

      // Set up temporary scope to use for evaluating the text that
      // defines the anonymous function.

      octave::interpreter& interp
        = octave::__get_interpreter__ ("octave_fcn_handle::load_ascii");

      octave::tree_evaluator& tw = interp.get_evaluator ();

      tw.push_dummy_scope (buf);
      frame.add_method (tw, &octave::tree_evaluator::pop_scope);

      octave_idx_type len = 0;

      if (extract_keyword (is, "length", len, true) && len >= 0)
        {
          if (len > 0)
            {
              for (octave_idx_type i = 0; i < len; i++)
                {
                  octave_value t2;
                  bool dummy;

                  std::string name
                    = read_text_data (is, "", dummy, t2, i);

                  if (! is)
                    error ("load: failed to load anonymous function handle");

                  interp.assign (name, t2);
                }
            }
        }
      else
        {
          is.seekg (pos);
          is.clear ();
        }

      if (is && success)
        success = parse_anon_fcn_handle (buf);
      else
        success = false;
    }
  else
    success = set_fcn (octaveroot, fpath);

  return success;
}

bool
octave_fcn_handle::save_binary (std::ostream& os, bool save_as_floats)
{
  if (m_name == anonymous)
    {
      std::ostringstream nmbuf;

      if (m_fcn.is_undefined ())
        return false;

      octave_user_function *f = m_fcn.user_function_value ();

      octave_user_function::local_vars_map local_vars
        = f->local_var_init_vals ();

      size_t varlen = local_vars.size ();

      if (varlen > 0)
        nmbuf << m_name << ' ' << varlen;
      else
        nmbuf << m_name;

      std::string buf_str = nmbuf.str ();
      int32_t tmp = buf_str.length ();
      os.write (reinterpret_cast<char *> (&tmp), 4);
      os.write (buf_str.c_str (), buf_str.length ());

      std::ostringstream buf;
      print_raw (buf, true);
      std::string stmp = buf.str ();
      tmp = stmp.length ();
      os.write (reinterpret_cast<char *> (&tmp), 4);
      os.write (stmp.c_str (), stmp.length ());

      if (varlen > 0)
        {
          for (const auto& nm_val : local_vars)
            {
              if (! save_binary_data (os, nm_val.second, nm_val.first,
                                      "", 0, save_as_floats))
                return ! os.fail ();
            }
        }
    }
  else
    {
      std::ostringstream nmbuf;

      octave_function *f = function_value ();
      std::string fnm = (f ? f->fcn_file_name () : "");

      nmbuf << m_name << "\n" << octave::config::octave_exec_home () << "\n" << fnm;

      std::string buf_str = nmbuf.str ();
      int32_t tmp = buf_str.length ();
      os.write (reinterpret_cast<char *> (&tmp), 4);
      os.write (buf_str.c_str (), buf_str.length ());
    }

  return true;
}

bool
octave_fcn_handle::load_binary (std::istream& is, bool swap,
                                octave::mach_info::float_format fmt)
{
  bool success = true;

  int32_t tmp;
  if (! is.read (reinterpret_cast<char *> (&tmp), 4))
    return false;
  if (swap)
    swap_bytes<4> (&tmp);

  OCTAVE_LOCAL_BUFFER (char, ctmp1, tmp+1);
  // is.get (ctmp1, tmp+1, 0); caused is.eof () to be true though
  // effectively not reading over file end
  is.read (ctmp1, tmp);
  ctmp1[tmp] = 0;
  m_name = std::string (ctmp1);

  if (! is)
    return false;

  size_t anl = anonymous.length ();

  if (m_name.length () >= anl && m_name.substr (0, anl) == anonymous)
    {
      octave_idx_type len = 0;

      if (m_name.length () > anl)
        {
          std::istringstream nm_is (m_name.substr (anl));
          nm_is >> len;
          m_name = m_name.substr (0, anl);
        }

      if (! is.read (reinterpret_cast<char *> (&tmp), 4))
        return false;
      if (swap)
        swap_bytes<4> (&tmp);

      OCTAVE_LOCAL_BUFFER (char, ctmp2, tmp+1);
      // is.get (ctmp2, tmp+1, 0); caused is.eof () to be true though
      // effectively not reading over file end
      is.read (ctmp2, tmp);
      ctmp2[tmp] = 0;

      octave::unwind_protect_safe frame;

      // Set up temporary scope to use for evaluating the text that
      // defines the anonymous function.

      octave::interpreter& interp
        = octave::__get_interpreter__ ("octave_fcn_handle::load_binary");

      octave::tree_evaluator& tw = interp.get_evaluator ();

      tw.push_dummy_scope (ctmp2);
      frame.add_method (tw, &octave::tree_evaluator::pop_scope);

      if (len > 0)
        {
          for (octave_idx_type i = 0; i < len; i++)
            {
              octave_value t2;
              bool dummy;
              std::string doc;

              std::string name
                = read_binary_data (is, swap, fmt, "", dummy, t2, doc);

              if (! is)
                error ("load: failed to load anonymous function handle");

              interp.assign (name, t2);
            }
        }

      if (is && success)
        success = parse_anon_fcn_handle (ctmp2);
      else
        success = false;
    }
  else
    {
      std::string octaveroot;
      std::string fpath;

      if (m_name.find_first_of ('\n') != std::string::npos)
        {
          size_t pos1 = m_name.find_first_of ('\n');
          size_t pos2 = m_name.find_first_of ('\n', pos1 + 1);
          octaveroot = m_name.substr (pos1 + 1, pos2 - pos1 - 1);
          fpath = m_name.substr (pos2 + 1);
          m_name = m_name.substr (0, pos1);
        }

      success = set_fcn (octaveroot, fpath);
    }

  return success;
}

bool
octave_fcn_handle::save_hdf5 (octave_hdf5_id loc_id, const char *name,
                              bool save_as_floats)
{
#if defined (HAVE_HDF5)

  bool retval = true;

  hid_t group_hid = -1;
#if defined (HAVE_HDF5_18)
  group_hid = H5Gcreate (loc_id, name, octave_H5P_DEFAULT, octave_H5P_DEFAULT,
                         octave_H5P_DEFAULT);
#else
  group_hid = H5Gcreate (loc_id, name, 0);
#endif
  if (group_hid < 0)
    return false;

  hid_t space_hid, data_hid, type_hid;
  space_hid = data_hid = type_hid = -1;

  // attach the type of the variable
  type_hid = H5Tcopy (H5T_C_S1);
  H5Tset_size (type_hid, m_name.length () + 1);
  if (type_hid < 0)
    {
      H5Gclose (group_hid);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (hsize_t, hdims, 2);
  hdims[0] = 0;
  hdims[1] = 0;
  space_hid = H5Screate_simple (0, hdims, nullptr);
  if (space_hid < 0)
    {
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return false;
    }
#if defined (HAVE_HDF5_18)
  data_hid = H5Dcreate (group_hid, "nm",  type_hid, space_hid,
                        octave_H5P_DEFAULT, octave_H5P_DEFAULT,
                        octave_H5P_DEFAULT);
#else
  data_hid = H5Dcreate (group_hid, "nm",  type_hid, space_hid,
                        octave_H5P_DEFAULT);
#endif
  if (data_hid < 0
      || H5Dwrite (data_hid, type_hid, octave_H5S_ALL, octave_H5S_ALL,
                   octave_H5P_DEFAULT, m_name.c_str ()) < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return false;
    }
  H5Dclose (data_hid);

  if (m_name == anonymous)
    {
      std::ostringstream buf;
      print_raw (buf, true);
      std::string stmp = buf.str ();

      // attach the type of the variable
      H5Tset_size (type_hid, stmp.length () + 1);
      if (type_hid < 0)
        {
          H5Sclose (space_hid);
          H5Gclose (group_hid);
          return false;
        }

#if defined (HAVE_HDF5_18)
      data_hid = H5Dcreate (group_hid, "fcn",  type_hid, space_hid,
                            octave_H5P_DEFAULT, octave_H5P_DEFAULT,
                            octave_H5P_DEFAULT);
#else
      data_hid = H5Dcreate (group_hid, "fcn",  type_hid, space_hid,
                            octave_H5P_DEFAULT);
#endif
      if (data_hid < 0
          || H5Dwrite (data_hid, type_hid, octave_H5S_ALL, octave_H5S_ALL,
                       octave_H5P_DEFAULT, stmp.c_str ()) < 0)
        {
          H5Sclose (space_hid);
          H5Tclose (type_hid);
          H5Gclose (group_hid);
          return false;
        }

      H5Dclose (data_hid);

      octave_user_function *f = m_fcn.user_function_value ();

      octave_user_function::local_vars_map local_vars
        = f->local_var_init_vals ();

      size_t varlen = local_vars.size ();

      if (varlen > 0)
        {
          hid_t as_id = H5Screate (H5S_SCALAR);

          if (as_id >= 0)
            {
#if defined (HAVE_HDF5_18)
              hid_t a_id = H5Acreate (group_hid, "SYMBOL_TABLE",
                                      H5T_NATIVE_IDX, as_id,
                                      octave_H5P_DEFAULT, octave_H5P_DEFAULT);

#else
              hid_t a_id = H5Acreate (group_hid, "SYMBOL_TABLE",
                                      H5T_NATIVE_IDX, as_id, octave_H5P_DEFAULT);
#endif

              if (a_id >= 0)
                {
                  retval = (H5Awrite (a_id, H5T_NATIVE_IDX, &varlen) >= 0);

                  H5Aclose (a_id);
                }
              else
                retval = false;

              H5Sclose (as_id);
            }
          else
            retval = false;
#if defined (HAVE_HDF5_18)
          data_hid = H5Gcreate (group_hid, "symbol table",
                                octave_H5P_DEFAULT, octave_H5P_DEFAULT, octave_H5P_DEFAULT);
#else
          data_hid = H5Gcreate (group_hid, "symbol table", 0);
#endif
          if (data_hid < 0)
            {
              H5Sclose (space_hid);
              H5Tclose (type_hid);
              H5Gclose (group_hid);
              return false;
            }

          for (const auto& nm_val : local_vars)
            {
              if (! add_hdf5_data (data_hid, nm_val.second, nm_val.first,
                                   "", false, save_as_floats))
                break;
            }
          H5Gclose (data_hid);
        }
    }
  else
    {
      std::string octaveroot = octave::config::octave_exec_home ();

      octave_function *f = function_value ();
      std::string fpath = (f ? f->fcn_file_name () : "");

      H5Sclose (space_hid);
      hdims[0] = 1;
      hdims[1] = octaveroot.length ();
      space_hid = H5Screate_simple (0, hdims, nullptr);
      if (space_hid < 0)
        {
          H5Tclose (type_hid);
          H5Gclose (group_hid);
          return false;
        }

      H5Tclose (type_hid);
      type_hid = H5Tcopy (H5T_C_S1);
      H5Tset_size (type_hid, octaveroot.length () + 1);
#if defined (HAVE_HDF5_18)
      hid_t a_id = H5Acreate (group_hid, "OCTAVEROOT",
                              type_hid, space_hid, octave_H5P_DEFAULT, octave_H5P_DEFAULT);
#else
      hid_t a_id = H5Acreate (group_hid, "OCTAVEROOT",
                              type_hid, space_hid, octave_H5P_DEFAULT);
#endif

      if (a_id >= 0)
        {
          retval = (H5Awrite (a_id, type_hid, octaveroot.c_str ()) >= 0);

          H5Aclose (a_id);
        }
      else
        {
          H5Sclose (space_hid);
          H5Tclose (type_hid);
          H5Gclose (group_hid);
          return false;
        }

      H5Sclose (space_hid);
      hdims[0] = 1;
      hdims[1] = fpath.length ();
      space_hid = H5Screate_simple (0, hdims, nullptr);
      if (space_hid < 0)
        {
          H5Tclose (type_hid);
          H5Gclose (group_hid);
          return false;
        }

      H5Tclose (type_hid);
      type_hid = H5Tcopy (H5T_C_S1);
      H5Tset_size (type_hid, fpath.length () + 1);

#if defined (HAVE_HDF5_18)
      a_id = H5Acreate (group_hid, "FILE", type_hid, space_hid,
                        octave_H5P_DEFAULT, octave_H5P_DEFAULT);
#else
      a_id = H5Acreate (group_hid, "FILE", type_hid, space_hid, octave_H5P_DEFAULT);
#endif

      if (a_id >= 0)
        {
          retval = (H5Awrite (a_id, type_hid, fpath.c_str ()) >= 0);

          H5Aclose (a_id);
        }
      else
        retval = false;
    }

  H5Sclose (space_hid);
  H5Tclose (type_hid);
  H5Gclose (group_hid);

  return retval;

#else
  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);
  octave_unused_parameter (save_as_floats);

  warn_save ("hdf5");

  return false;
#endif
}

bool
octave_fcn_handle::load_hdf5 (octave_hdf5_id loc_id, const char *name)
{
#if defined (HAVE_HDF5)

  bool success = true;

  hid_t group_hid, data_hid, space_hid, type_hid, type_class_hid, st_id;
  hsize_t rank;
  int slen;

#if defined (HAVE_HDF5_18)
  group_hid = H5Gopen (loc_id, name, octave_H5P_DEFAULT);
#else
  group_hid = H5Gopen (loc_id, name);
#endif
  if (group_hid < 0)
    return false;

#if defined (HAVE_HDF5_18)
  data_hid = H5Dopen (group_hid, "nm", octave_H5P_DEFAULT);
#else
  data_hid = H5Dopen (group_hid, "nm");
#endif

  if (data_hid < 0)
    {
      H5Gclose (group_hid);
      return false;
    }

  type_hid = H5Dget_type (data_hid);
  type_class_hid = H5Tget_class (type_hid);

  if (type_class_hid != H5T_STRING)
    {
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  space_hid = H5Dget_space (data_hid);
  rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  slen = H5Tget_size (type_hid);
  if (slen < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (char, nm_tmp, slen);

  // create datatype for (null-terminated) string to read into:
  st_id = H5Tcopy (H5T_C_S1);
  H5Tset_size (st_id, slen);

  if (H5Dread (data_hid, st_id, octave_H5S_ALL, octave_H5S_ALL,
               octave_H5P_DEFAULT, nm_tmp)
      < 0)
    {
      H5Tclose (st_id);
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }
  H5Tclose (st_id);
  H5Dclose (data_hid);
  m_name = nm_tmp;

  if (m_name == anonymous)
    {
#if defined (HAVE_HDF5_18)
      data_hid = H5Dopen (group_hid, "fcn", octave_H5P_DEFAULT);
#else
      data_hid = H5Dopen (group_hid, "fcn");
#endif

      if (data_hid < 0)
        {
          H5Sclose (space_hid);
          H5Tclose (type_hid);
          H5Gclose (group_hid);
          return false;
        }

      H5Tclose (type_hid);
      type_hid = H5Dget_type (data_hid);
      type_class_hid = H5Tget_class (type_hid);

      if (type_class_hid != H5T_STRING)
        {
          H5Sclose (space_hid);
          H5Tclose (type_hid);
          H5Dclose (data_hid);
          H5Gclose (group_hid);
          return false;
        }

      H5Sclose (space_hid);
      space_hid = H5Dget_space (data_hid);
      rank = H5Sget_simple_extent_ndims (space_hid);

      if (rank != 0)
        {
          H5Sclose (space_hid);
          H5Tclose (type_hid);
          H5Dclose (data_hid);
          H5Gclose (group_hid);
          return false;
        }

      slen = H5Tget_size (type_hid);
      if (slen < 0)
        {
          H5Sclose (space_hid);
          H5Tclose (type_hid);
          H5Dclose (data_hid);
          H5Gclose (group_hid);
          return false;
        }

      OCTAVE_LOCAL_BUFFER (char, fcn_tmp, slen);

      // create datatype for (null-terminated) string to read into:
      st_id = H5Tcopy (H5T_C_S1);
      H5Tset_size (st_id, slen);

      if (H5Dread (data_hid, st_id, octave_H5S_ALL, octave_H5S_ALL,
                   octave_H5P_DEFAULT, fcn_tmp)
          < 0)
        {
          H5Tclose (st_id);
          H5Sclose (space_hid);
          H5Tclose (type_hid);
          H5Dclose (data_hid);
          H5Gclose (group_hid);
          return false;
        }
      H5Tclose (st_id);
      H5Dclose (data_hid);

      octave_idx_type len = 0;

      // we have to pull some shenanigans here to make sure
      // HDF5 doesn't print out all sorts of error messages if we
      // call H5Aopen for a non-existing attribute

      H5E_auto_t err_func;
      void *err_func_data;

      // turn off error reporting temporarily, but save the error
      // reporting function:
#if defined (HAVE_HDF5_18)
      H5Eget_auto (octave_H5E_DEFAULT, &err_func, &err_func_data);
      H5Eset_auto (octave_H5E_DEFAULT, nullptr, nullptr);
#else
      H5Eget_auto (&err_func, &err_func_data);
      H5Eset_auto (nullptr, nullptr);
#endif

      hid_t attr_id = H5Aopen_name (group_hid, "SYMBOL_TABLE");

      if (attr_id >= 0)
        {
          if (H5Aread (attr_id, H5T_NATIVE_IDX, &len) < 0)
            success = false;

          H5Aclose (attr_id);
        }

      // restore error reporting:
#if defined (HAVE_HDF5_18)
      H5Eset_auto (octave_H5E_DEFAULT, err_func, err_func_data);
#else
      H5Eset_auto (err_func, err_func_data);
#endif

      octave::unwind_protect_safe frame;

      // Set up temporary scope to use for evaluating the text that
      // defines the anonymous function.

      octave::interpreter& interp
        = octave::__get_interpreter__ ("octave_fcn_handle::load_hdf5");

      octave::tree_evaluator& tw = interp.get_evaluator ();

      tw.push_dummy_scope (fcn_tmp);
      frame.add_method (tw, &octave::tree_evaluator::pop_scope);

      if (len > 0 && success)
        {
          hsize_t num_obj = 0;
#if defined (HAVE_HDF5_18)
          data_hid = H5Gopen (group_hid, "symbol table", octave_H5P_DEFAULT);
#else
          data_hid = H5Gopen (group_hid, "symbol table");
#endif
          H5Gget_num_objs (data_hid, &num_obj);
          H5Gclose (data_hid);

          if (num_obj != static_cast<hsize_t> (len))
            error ("load: failed to load anonymous function handle");

          hdf5_callback_data dsub;
          int current_item = 0;
          for (octave_idx_type i = 0; i < len; i++)
            {
              if (hdf5_h5g_iterate (group_hid, "symbol table", &current_item,
                                    &dsub) <= 0)
                error ("load: failed to load anonymous function handle");

              interp.assign (dsub.name, dsub.tc);
            }
        }

      if (success)
        success = parse_anon_fcn_handle (fcn_tmp);
    }
  else
    {
      std::string octaveroot;
      std::string fpath;

      // we have to pull some shenanigans here to make sure
      // HDF5 doesn't print out all sorts of error messages if we
      // call H5Aopen for a non-existing attribute

      H5E_auto_t err_func;
      void *err_func_data;

      // turn off error reporting temporarily, but save the error
      // reporting function:
#if defined (HAVE_HDF5_18)
      H5Eget_auto (octave_H5E_DEFAULT, &err_func, &err_func_data);
      H5Eset_auto (octave_H5E_DEFAULT, nullptr, nullptr);
#else
      H5Eget_auto (&err_func, &err_func_data);
      H5Eset_auto (nullptr, nullptr);
#endif

      hid_t attr_id = H5Aopen_name (group_hid, "OCTAVEROOT");
      if (attr_id >= 0)
        {
          H5Tclose (type_hid);
          type_hid = H5Aget_type (attr_id);
          type_class_hid = H5Tget_class (type_hid);

          if (type_class_hid != H5T_STRING)
            success = false;
          else
            {
              slen = H5Tget_size (type_hid);
              st_id = H5Tcopy (H5T_C_S1);
              H5Tset_size (st_id, slen);
              OCTAVE_LOCAL_BUFFER (char, root_tmp, slen);

              if (H5Aread (attr_id, st_id, root_tmp) < 0)
                success = false;
              else
                octaveroot = root_tmp;

              H5Tclose (st_id);
            }

          H5Aclose (attr_id);
        }

      if (success)
        {
          attr_id = H5Aopen_name (group_hid, "FILE");
          if (attr_id >= 0)
            {
              H5Tclose (type_hid);
              type_hid = H5Aget_type (attr_id);
              type_class_hid = H5Tget_class (type_hid);

              if (type_class_hid != H5T_STRING)
                success = false;
              else
                {
                  slen = H5Tget_size (type_hid);
                  st_id = H5Tcopy (H5T_C_S1);
                  H5Tset_size (st_id, slen);
                  OCTAVE_LOCAL_BUFFER (char, path_tmp, slen);

                  if (H5Aread (attr_id, st_id, path_tmp) < 0)
                    success = false;
                  else
                    fpath = path_tmp;

                  H5Tclose (st_id);
                }

              H5Aclose (attr_id);
            }
        }

      // restore error reporting:
#if defined (HAVE_HDF5_18)
      H5Eset_auto (octave_H5E_DEFAULT, err_func, err_func_data);
#else
      H5Eset_auto (err_func, err_func_data);
#endif

      success = (success ? set_fcn (octaveroot, fpath) : success);
    }

  H5Tclose (type_hid);
  H5Sclose (space_hid);
  H5Gclose (group_hid);

  return success;

#else
  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);

  warn_load ("hdf5");

  return false;
#endif
}

/*
%!test <*33857>
%! a = 2;
%! f = @(x) a + x;
%! g = @(x) 2 * x;
%! hm = @version;
%! hdld = @svd;
%! hbi = @log2;
%! f2 = f;
%! g2 = g;
%! hm2 = hm;
%! hdld2 = hdld;
%! hbi2 = hbi;
%! modes = {"-text", "-binary"};
%! if (isfield (__octave_config_info__, "HAVE_HDF5")
%!     && __octave_config_info__ ("HAVE_HDF5"))
%!   modes(end+1) = "-hdf5";
%! endif
%! for i = 1:numel (modes)
%!   mode = modes{i};
%!   nm = tempname ();
%!   unwind_protect
%!     f2 (1);
%!     save (mode, nm, "f2", "g2", "hm2", "hdld2", "hbi2");
%!     clear f2 g2 hm2 hdld2 hbi2
%!     load (nm);
%!     assert (f (2), f2 (2));
%!     assert (g (2), g2 (2));
%!     assert (g (3), g2 (3));
%!     unlink (nm);
%!     save (mode, nm, "f2", "g2", "hm2", "hdld2", "hbi2");
%!   unwind_protect_cleanup
%!     unlink (nm);
%!   end_unwind_protect
%! endfor
*/

/*
%!function fcn_handle_save_recurse (n, mode, nm, f2, g2, hm2, hdld2, hbi2)
%!  if (n == 0)
%!    save (mode, nm, "f2", "g2", "hm2", "hdld2", "hbi2");
%!  else
%!    fcn_handle_save_recurse (n - 1, mode, nm, f2, g2, hm2, hdld2, hbi2);
%!  endif
%!endfunction
%!function [f2, g2, hm2, hdld2, hbi2] = fcn_handle_load_recurse (n, nm)
%!  if (n == 0)
%!    load (nm);
%!  else
%!    [f2, g2, hm2, hdld2, hbi2] = fcn_handle_load_recurse (n - 1, nm);
%!  endif
%!endfunction

%!test <*35876>
%! a = 2;
%! f = @(x) a + x;
%! g = @(x) 2 * x;
%! hm = @version;
%! hdld = @svd;
%! hbi = @log2;
%! f2 = f;
%! g2 = g;
%! hm2 = hm;
%! hdld2 = hdld;
%! hbi2 = hbi;
%! modes = {"-text", "-binary"};
%! if (isfield (__octave_config_info__, "HAVE_HDF5")
%!     && __octave_config_info__ ("HAVE_HDF5"))
%!   modes(end+1) = "-hdf5";
%! endif
%! for i = 1:numel (modes)
%!   mode = modes{i};
%!   nm = tempname ();
%!   unwind_protect
%!     fcn_handle_save_recurse (2, mode, nm, f2, g2, hm2, hdld2, hbi2);
%!     clear f2 g2 hm2 hdld2 hbi2
%!     [f2, f2, hm2, hdld2, hbi2] = fcn_handle_load_recurse (2, nm);
%!     load (nm);
%!     assert (f (2), f2 (2));
%!     assert (g (2), g2 (2));
%!     assert (g (3), g2 (3));
%!     unlink (nm);
%!     fcn_handle_save_recurse (2, mode, nm, f2, g2, hm2, hdld2, hbi2);
%!   unwind_protect_cleanup
%!     unlink (nm);
%!   end_unwind_protect
%! endfor
*/

void
octave_fcn_handle::print (std::ostream& os, bool pr_as_read_syntax)
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

void
octave_fcn_handle::print_raw (std::ostream& os, bool pr_as_read_syntax) const
{
  bool printed = false;

  if (m_name == anonymous)
    {
      octave::tree_print_code tpc (os);

      // FCN is const because this member function is, so we can't
      // use it to call user_function_value, so we make a copy first.

      octave_value ftmp = m_fcn;

      octave_user_function *f = ftmp.user_function_value ();

      if (f)
        {
          os << "@";

          // The parameter list should always be valid for anonymous
          // functions, so we should always call accept for it, and it
          // will print the parens for us.

          octave::tree_parameter_list *p = f->parameter_list ();

          if (p)
            p->accept (tpc);

          os << " ";

          octave::tree_statement_list *b = f->body ();

          assert (b->length () == 1);

          octave::tree_statement *s = b->front ();

          if (s)
            {
              assert (s->is_expression ());

              octave::tree_expression *e = s->expression ();

              if (e)
                tpc.print_fcn_handle_body (e);
            }

          printed = true;
        }
    }

  if (! printed)
    octave_print_internal (os, '@' + m_name, pr_as_read_syntax,
                           current_print_indent_level ());
}

namespace octave
{
  // Hmm, should this function be a member of the interpreter class,
  // possibly forwarded to an actual implementation in the
  // tree_evaluator class?

  octave_value
  make_fcn_handle (interpreter& interp, const std::string& nm)
  {
    octave_value retval;

    // Bow to the god of compatibility.

    // FIXME: it seems ugly to put this here, but there is no single
    // function in the parser that converts from the operator name to
    // the corresponding function name.  At least try to do it without N
    // string compares.

    std::string tnm = nm;

    size_t len = nm.length ();

    if (len == 3 && nm == ".**")
      tnm = "power";
    else if (len == 2)
      {
        if (nm[0] == '.')
          {
            switch (nm[1])
              {
              case '\'':
                tnm = "transpose";
                break;

              case '+':
                tnm = "plus";
                break;

              case '-':
                tnm = "minus";
                break;

              case '*':
                tnm = "times";
                break;

              case '/':
                tnm = "rdivide";
                break;

              case '^':
                tnm = "power";
                break;

              case '\\':
                tnm = "ldivide";
                break;
              }
          }
        else if (nm[1] == '=')
          {
            switch (nm[0])
              {
              case '<':
                tnm = "le";
                break;

              case '=':
                tnm = "eq";
                break;

              case '>':
                tnm = "ge";
                break;

              case '~':
              case '!':
                tnm = "ne";
                break;
              }
          }
        else if (nm == "**")
          tnm = "mpower";
      }
    else if (len == 1)
      {
        switch (nm[0])
          {
          case '~':
          case '!':
            tnm = "not";
            break;

          case '\'':
            tnm = "ctranspose";
            break;

          case '+':
            tnm = "plus";
            break;

          case '-':
            tnm = "minus";
            break;

          case '*':
            tnm = "mtimes";
            break;

          case '/':
            tnm = "mrdivide";
            break;

          case '^':
            tnm = "mpower";
            break;

          case '\\':
            tnm = "mldivide";
            break;

          case '<':
            tnm = "lt";
            break;

          case '>':
            tnm = "gt";
            break;

          case '&':
            tnm = "and";
            break;

          case '|':
            tnm = "or";
            break;
          }
      }

    tree_evaluator& tw = interp.get_evaluator ();

    symbol_scope curr_scope = tw.get_current_scope ();

    octave_fcn_handle *fh = new octave_fcn_handle (curr_scope, tnm);

    std::string dispatch_class;

    if (tw.is_class_method_executing (dispatch_class)
        || tw.is_class_constructor_executing (dispatch_class))
      fh->set_dispatch_class (dispatch_class);

    return octave_value (fh);
  }
}

/*
%!test
%! x = {".**", "power";
%!      ".'", "transpose";
%!      ".+", "plus";
%!      ".-", "minus";
%!      ".*", "times";
%!      "./", "rdivide";
%!      ".^", "power";
%!      ".\\", "ldivide";
%!      "<=", "le";
%!      "==", "eq";
%!      ">=", "ge";
%!      "!=", "ne";
%!      "~=", "ne";
%!      "**", "mpower";
%!      "~", "not";
%!      "!", "not";
%!      "\'", "ctranspose";
%!      "+", "plus";
%!      "-", "minus";
%!      "*", "mtimes";
%!      "/", "mrdivide";
%!      "^", "mpower";
%!      "\\", "mldivide";
%!      "<", "lt";
%!      ">", "gt";
%!      "&", "and";
%!      "|", "or"};
%! for i = 1:rows (x)
%!   assert (functions (str2func (x{i,1})).function, x{i,2});
%! endfor
*/

DEFUN (functions, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{s} =} functions (@var{fcn_handle})
Return a structure containing information about the function handle
@var{fcn_handle}.

The structure @var{s} always contains these three fields:

@table @asis
@item function
The function name.  For an anonymous function (no name) this will be the
actual function definition.

@item type
Type of the function.

@table @asis
@item anonymous
The function is anonymous.

@item private
The function is private.

@item overloaded
The function overloads an existing function.

@item simple
The function is a built-in or m-file function.

@item subfunction
The function is a subfunction within an m-file.
@end table

@item nested
The function is nested.

@item file
The m-file that will be called to perform the function.  This field is empty
for anonymous and built-in functions.
@end table

In addition, some function types may return more information in additional
fields.

@strong{Warning:} @code{functions} is provided for debugging purposes only.
Its behavior may change in the future and programs should not depend on any
particular output format.

@seealso{func2str, str2func}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  octave_fcn_handle *fh = args(0).xfcn_handle_value ("functions: FCN_HANDLE argument must be a function handle object");

  octave_function *fcn = (fh ? fh->function_value () : nullptr);

  if (! fcn)
    error ("functions: FCN_HANDLE is not a valid function handle object");

  octave_scalar_map m;

  std::string fh_nm = fh->fcn_name ();

  if (fh_nm == octave_fcn_handle::anonymous)
    {
      std::ostringstream buf;
      fh->print_raw (buf);
      m.setfield ("function", buf.str ());

      m.setfield ("type", "anonymous");
    }
  else
    {
      m.setfield ("function", fh_nm);

      if (fcn->is_subfunction ())
        {
          m.setfield ("type", "subfunction");
          Cell parentage (dim_vector (1, 2));
          parentage.elem (0) = fh_nm;
          parentage.elem (1) = fcn->parent_fcn_name ();
          m.setfield ("parentage", octave_value (parentage));
        }
      else if (fcn->is_private_function ())
        m.setfield ("type", "private");
      else if (fh->is_nested ())
        m.setfield ("type", "nested");
      else
        m.setfield ("type", "simple");
    }

  std::string nm = fcn->fcn_file_name ();

  if (fh_nm == octave_fcn_handle::anonymous)
    {
      m.setfield ("file", nm);

      m.setfield ("workspace", fh->workspace ());
    }
  else if (fcn->is_user_function () || fcn->is_user_script ())
    {
      octave_function *fu = fh->function_value ();
      m.setfield ("file", fu->fcn_file_name ());

      m.setfield ("workspace", fh->workspace ());
    }
  else
    m.setfield ("file", "");

  return ovl (m);
}

DEFUN (func2str, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} func2str (@var{fcn_handle})
Return a string containing the name of the function referenced by the
function handle @var{fcn_handle}.
@seealso{str2func, functions}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  octave_fcn_handle *fh = args(0).xfcn_handle_value ("func2str: FCN_HANDLE argument must be a function handle object");

  if (! fh)
    error ("func2str: FCN_HANDLE must be a valid function handle");

  octave_value retval;

  std::string fh_nm = fh->fcn_name ();

  if (fh_nm == octave_fcn_handle::anonymous)
    {
      std::ostringstream buf;

      fh->print_raw (buf);

      retval = buf.str ();
    }
  else
    retval = fh_nm;

  return retval;
}

DEFMETHOD (str2func, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} str2func (@var{fcn_name})
Return a function handle constructed from the string @var{fcn_name}.

Previous versions of Octave accepted an optional second argument,
@qcode{"global"}, that caused str2func to ignore locally visible
functions.  This option is no longer supported.
@seealso{func2str, inline, functions}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  std::string nm = args(0).xstring_value ("str2func: FCN_NAME must be a string");

  octave_value retval;

  if (nm[0] == '@')
    {
      int parse_status;
      octave_value anon_fcn_handle
        = interp.eval_string (nm, true, parse_status);

      if (parse_status == 0)
        retval = anon_fcn_handle;
    }
  else
    {
      if (nargin == 2)
        warning_with_id ("Octave:str2func-global-argument",
                         "str2func: second argument ignored");

      retval = octave::make_fcn_handle (interp, nm);
    }

  return retval;
}

/*
%!test
%! f = str2func ("<");
%! assert (class (f), "function_handle");
%! assert (func2str (f), "lt");
%! assert (f (1, 2), true);
%! assert (f (2, 1), false);

%!test
%! f = str2func ("@(x) sin (x)");
%! assert (func2str (f), "@(x) sin (x)");
%! assert (f (0:3), sin (0:3));

%!error <FCN_NAME must be a string> str2func ({"sin"})
*/

/*
%!function y = __testrecursionfunc (f, x, n)
%!  if (nargin < 3)
%!    n = 0;
%!  endif
%!  if (n > 2)
%!    y = f (x);
%!  else
%!    n++;
%!    y = __testrecursionfunc (@(x) f (2*x), x, n);
%!  endif
%!endfunction
%!
%!assert (__testrecursionfunc (@(x) x, 1), 8)
*/

DEFUN (is_function_handle, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} is_function_handle (@var{x})
Return true if @var{x} is a function handle.
@seealso{isa, typeinfo, class, functions}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).is_function_handle ());
}

/*
%!shared fh, finline
%! fh = @(x) x;
%! finline = inline ("x");

%!assert (is_function_handle (fh))
%!assert (is_function_handle (finline))
%!assert (! is_function_handle ({fh}))
%!assert (! is_function_handle (1))

%!error is_function_handle ()
%!error is_function_handle (1, 2)
*/

/*
%!test
%! f = @(t) eval ('2*t');
%! assert (f (21), 42);
*/

/*
%!test <*58389>
%! s = "x";
%! a.(s) = [e, pi];
%! f = @(x) a.(s)(x);
%! assert (f(1), e);
%! assert (f(2), pi);
%! assert (f([2,1]), [pi, e]);
*/

/*
%!function r = __f (g, i)
%!  r = g(i);
%!endfunction
%!test
%! x = [1,2;3,4];
%! assert (__f (@(i) x(:,i), 1), [1;3]);
*/
