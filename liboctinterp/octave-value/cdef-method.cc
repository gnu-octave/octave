////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2012-2026 The Octave Project Developers
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

#include "cdef-class.h"
#include "cdef-manager.h"
#include "cdef-utils.h"
#include "errwarn.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "load-path.h"
#include "ov-builtin.h"
#include "ov-classdef.h"
#include "ov-fcn-handle.h"
#include "ov-usr-fcn.h"
#include "parse.h"
#include "pt-assign.h"
#include "pt-classdef.h"
#include "pt-idx.h"
#include "pt-stmt.h"

OCTAVE_BEGIN_NAMESPACE(octave)

cdef_method::cdef_method_rep::cdef_method_rep (
  const cdef_object& defining_class,
  octave_value fcn,
  octave_value name,
  cdef_method_opt_properties opt_args)
  : cdef_meta_object_rep (), m_dispatch_type (),
    m_function (octave_value (fcn))
{
  set_class (__get_cdef_manager__ ().meta_method ());

  validate_access (opt_args.access, "Access", "method", name.string_value (),
                   cdef_class (defining_class));

  put ("Abstract", opt_args.abstract);
  put ("Access", opt_args.access);
  put ("DefiningClass", to_ov (defining_class));
  put ("Description", opt_args.description);
  put ("DetailedDescription", opt_args.detailed_description);
  put ("Hidden", opt_args.hidden);
  put ("Name", name);
  put ("Sealed", opt_args.sealed);

  put ("Static", opt_args.static_bool);

  // The following attribute combinations on methods are disallowed:
  //
  // (1) "Abstract" and "Static"
  // (2) "Abstract" and "Private"
  // (3) "Abstract" and "Sealed"
  //
  // Abstract methods must also not have a body.
  if (opt_args.abstract.bool_value () && opt_args.static_bool.bool_value ())
    error ("classdef method cannot be Abstract and Static at the same time");

  if (opt_args.abstract.bool_value () && opt_args.access.is_string ()
      && opt_args.access.string_value () == "private")
    error ("classdef method cannot be Abstract and Private at the same time");

  if (opt_args.abstract.bool_value () && opt_args.sealed.bool_value ())
    error ("classdef method cannot be Abstract and Sealed at the same time");

  // Dummy method means there is no body declared next to the method.
  // The method can be defined externally.
  bool dummy_method = is_dummy_method (fcn);

  if (opt_args.abstract.bool_value () && ! dummy_method)
    error ("classdef method cannot be Abstract and have a body");

  if (dummy_method)
    mark_as_external (defining_class.get ("Name").string_value ());
  else
    {
      std::string dc_name = defining_class.get ("Name").string_value ();

      octave_function* ov_fcn = fcn.function_value ();
      ov_fcn->stash_dispatch_class (dc_name);

      octave_user_function *uf = ov_fcn->user_function_value (true);

      if (uf)
        {
          std::string::size_type pos = dc_name.find_last_of ('.');

          if (pos != std::string::npos)
            dc_name = dc_name.substr (pos + 1);

          if (dc_name == uf->name ())
            uf->mark_as_classdef_constructor ();
          else
            uf->mark_as_classdef_method ();
        }
    }

}

void
cdef_method::cdef_method_rep::put (const std::string& pname,
                                   const octave_value& val)
{
  try
    {
      if (pname == "Abstract")
        {
          auto abstract = val.bool_value ();

          m_map.assign (pname, abstract);
        }
      else if (pname == "Access")
        {
          if (val.is_string ())
            {
              auto name = val.string_value ();

              if (name == "public" || name == "private" || name == "protected")
                m_map.assign (pname, name);
              else
                error ("Access must be either 'public', 'private', 'protected' or a list of classes");
            }
          else if (is_access_list (val))
            m_map.assign (pname, val);
          else
            error ("Access must be either 'public', 'private', 'protected' or a list of classes");
        }
      else if (pname == "DefiningClass")
        {
          auto cdef_obj = val.classdef_object_value ()->get_object ();

          if (cdef_obj.is_class ())
            m_map.assign (pname, val);
          else
            error ("DefiningClass must be a classdef");
        }
      else if (pname == "Description")
        {
          auto description = val.string_value ();

          m_map.assign (pname, description);
        }
      else if (pname == "DetailedDescription")
        {
          auto detailed_description = val.string_value ();

          m_map.assign (pname, detailed_description);
        }
      else if (pname == "Hidden")
        {
          auto hidden = val.bool_value ();

          m_map.assign (pname, hidden);
        }
      else if (pname == "Name")
        {
          auto name = val.string_value ();

          m_map.assign (pname, name);
        }
      else if (pname == "Sealed")
        {
          auto sealed = val.bool_value ();

          m_map.assign (pname, sealed);
        }
      else if (pname == "Static")
        {
          auto static_bool = val.bool_value ();

          m_map.assign (pname, static_bool);
        }
      else
        {
          error ("unknown property of meta.method");
        }
    }
  catch (const execution_exception& ee)
    {
      error ("put: setting '%s' failed with error: %s", pname.c_str (), ee.message ().c_str ());
    }
}

cdef_class
cdef_method::cdef_method_rep::get_defining_class () const
{
  return to_cdef (m_map.contents ("DefiningClass"));
}

cdef_class
cdef_method::get_defining_class () const
{
  return get_rep ()->get_defining_class ();
}

OCTAVE_NORETURN static
void
err_method_access (const std::string& from, const cdef_method& meth)
{
  octave_value acc = meth.get ("Access");
  std::string acc_s;

  if (acc.is_string ())
    acc_s = acc.string_value ();
  else
    acc_s = "class-restricted";

  error ("%s: method '%s' has %s access and cannot be run in this context",
         from.c_str (), meth.get_name ().c_str (), acc_s.c_str ());
}

void
cdef_method::cdef_method_rep::check_method ()
{
  if (is_external ())
    {
      if (is_dummy_method (m_function))
        {
          load_path& lp = __get_load_path__ ();

          std::string name = get_name ();
          std::string cls_name = m_dispatch_type;
          std::string pack_name;

          std::size_t pos = cls_name.rfind ('.');

          if (pos != std::string::npos)
            {
              pack_name = cls_name.substr (0, pos);
              cls_name = cls_name.substr (pos + 1);
            }

          std::string dir_name;
          std::string file_name = lp.find_method (cls_name, name,
                                                  dir_name, pack_name);

          if (! file_name.empty ())
            {
              octave_value ov_fcn
                = load_fcn_from_file (file_name, dir_name,
                                      m_dispatch_type, pack_name);

              if (ov_fcn.is_defined ())
                {
                  m_function = ov_fcn;

                  make_function_of_class (m_dispatch_type, m_function);
                }
            }
        }
      else
        {
          // FIXME: check out-of-date status
        }

      if (is_dummy_method (m_function))
        error ("no definition found for method '%s' of class '%s'",
               get_name ().c_str (), m_dispatch_type.c_str ());
    }
}

octave_value_list
cdef_method::cdef_method_rep::execute (const octave_value_list& args,
                                       int nargout, bool do_check_access,
                                       const std::string& who)
{
  octave_value_list retval;

  if (do_check_access && ! check_access ())
    err_method_access (who, wrap ());

  if (get ("Abstract").bool_value ())
    error ("%s: cannot execute abstract method",
           get ("Name").string_value ().c_str ());

  check_method ();

  if (m_function.is_defined ())
    {
      interpreter& interp = __get_interpreter__ ();

      retval = interp.feval (m_function, args, nargout);
    }

  return retval;
}

octave_value_list
cdef_method::cdef_method_rep::execute (const cdef_object& obj,
                                       const octave_value_list& args,
                                       int nargout, bool do_check_access,
                                       const std::string& who)
{
  octave_value_list prepended_args = args;
  prepended_args.prepend (to_ov (obj));

  return execute (prepended_args, nargout, do_check_access, who);
}

bool
cdef_method::cdef_method_rep::is_constructor () const
{
  if (m_function.is_function ())
    return m_function.function_value ()->is_classdef_constructor ();

  return false;
}

bool
cdef_method::cdef_method_rep::is_defined_in_class (const std::string& cname) const
{
  return (m_function.is_function ()
          ? m_function.function_value ()->dispatch_class () == cname
          : false);
}

std::string
cdef_method::cdef_method_rep::get_doc_string ()
{
  check_method ();

  octave_function *fcn = m_function.function_value ();

  return fcn ? fcn->doc_string () : "";
}

bool
cdef_method::cdef_method_rep::check_access () const
{
  cdef_class cls (to_cdef (get ("DefiningClass")));

  return octave::check_access (cls, get ("Access"), get_name ());
}

octave_value_list
cdef_method::cdef_method_rep::meta_subsref
(const std::string& type, const std::list<octave_value_list>& idx,
 int nargout)
{
  octave_value_list retval;

  switch (type[0])
    {
    case '(':
      retval = (execute (idx.front (),
                         type.length () > 1 ? 1 : nargout, true));
      break;

    default:
      error ("invalid meta.method indexing");
      break;
    }

  if (type.length () > 1 && idx.size () > 1 && ! retval.empty ())
    retval = retval(0).next_subsref (nargout, type, idx, 1);

  return retval;
}

std::vector<cdef_method>
cdef_method::make_method (cdef_class& cls,
                          tree_evaluator& tw,
                          tree_classdef_methods_block* mth_blk)
{
  std::vector<cdef_method> retval;

  std::map<std::string, octave_value> amap = {
    {"Abstract", false},
    {"Access", "public"},
    {"Description", ""},
    {"DetailedDescription", ""},
    {"Hidden", false},
    {"Sealed", false},
    {"Static", false}
  };

  if (mth_blk->attribute_list ())
    {
      for (auto& attr_p : *mth_blk->attribute_list ())
        {
          std::string aname = attr_p->ident ()->name ();
          auto it = amap.find (aname);

          if (it == amap.end ())
            error ("invalid attribute: attribute %s cannot be assigned in a methods block", aname.c_str ());

          it->second = compute_attribute_value (tw, attr_p);
        }
    }

  if (mth_blk->element_list ())
    {
      for (auto& mth : *mth_blk->element_list ())
        {
          auto *fcn = mth.user_function_value ();
          std::string mname = fcn->name ();

          cdef_method_opt_properties attrs {
            .abstract = amap["Abstract"],
            .access = amap["Access"],
            .description = amap["Description"],
            .detailed_description = amap["DetailedDescription"],
            .hidden = amap["Hidden"],
            .sealed = amap["Sealed"],
            .static_bool = amap["Static"]
          };

          cdef_method meth = cdef_method (cls,
                                          mth,
                                          mname,
                                          attrs);

          retval.push_back (meth);
        }
    }

    return retval;
}

OCTAVE_END_NAMESPACE(octave)
