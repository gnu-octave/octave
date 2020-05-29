////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2012-2020 The Octave Project Developers
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

#if ! defined (octave_classdef_h)
#define octave_classdef_h 1

#include "octave-config.h"

#include <string>

#include "cdef-object.h"
#include "ov-base.h"
#include "ov-fcn.h"

namespace octave
{
  class cdef_object;
  class interpreter;
  class tree_evaluator;
  class type_info;
}

class
octave_classdef : public octave_base_value
{
public:

  octave_classdef (void)
    : octave_base_value (), object () { }

  octave_classdef (const octave::cdef_object& obj)
    : octave_base_value (), object (obj) { }

  octave_classdef (const octave_classdef&) = delete;

  octave_classdef& operator = (const octave_classdef&) = delete;

  ~octave_classdef (void) = default;

  octave_base_value * clone (void) const
  {
    return new octave_classdef (object.clone ());
  }

  octave_base_value * empty_clone (void) const
  {
    return new octave_classdef (object.empty_clone ());
  }

  octave_classdef * classdef_object_value (bool = false) { return this; }

  octave::cdef_object get_object (void) const { return object; }

  octave::cdef_object& get_object_ref (void) { return object; }

  bool is_defined (void) const { return true; }

  bool isstruct (void) const { return false; }

  bool isobject (void) const { return true; }

  bool is_classdef_object (void) const { return true; }

  void print (std::ostream& os, bool pr_as_read_syntax = false);

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  bool is_instance_of (const std::string& cls_name) const;

  octave_value_list subsref (const std::string& type,
                             const std::list<octave_value_list>& idx,
                             int nargout);

  octave_value subsref (const std::string& type,
                        const std::list<octave_value_list>& idx)
  {
    octave_value_list retval = subsref (type, idx, 1);
    return (retval.length () > 0 ? retval(0) : octave_value ());
  }

  octave_value subsref (const std::string& type,
                        const std::list<octave_value_list>& idx,
                        bool auto_add);

  octave_value subsasgn (const std::string& type,
                         const std::list<octave_value_list>& idx,
                         const octave_value& rhs);

  octave_value
  undef_subsasgn (const std::string& type,
                  const std::list<octave_value_list>& idx,
                  const octave_value& rhs);

  Matrix size (void);

  octave_idx_type xnumel (const octave_value_list&);

  string_vector map_keys (void) const { return object.map_keys (); }

  octave_map map_value (void) const { return object.map_value (); }

  dim_vector dims (void) const { return object.dims (); }

  void set_property (octave_idx_type idx, const std::string& name,
                     const octave_value& pval)
  {
    object.set_property (idx, name, pval);
  }

  octave_value
  get_property (octave_idx_type idx, const std::string& name) const
  {
    return object.get_property (idx, name);
  }

  static octave_value superclass_ref (const std::string& meth,
                                      const std::string& cls);

  static octave_value metaclass_query (const std::string& cls);

public:

  int type_id (void) const { return t_id; }
  std::string type_name (void) const { return t_name; }
  std::string class_name (void) const { return object.class_name (); }

  static int static_type_id (void) { return t_id; }
  static std::string static_type_name (void) { return t_name; }
  static std::string static_class_name (void) { return "<unknown>"; }
  static void register_type (octave::type_info&);

private:

  octave::cdef_object object;

  static int t_id;

  static const std::string t_name;
};

OCTINTERP_API void install_classdef (octave::interpreter& interp);

class octave_classdef_meta : public octave_function
{
public:

  octave_classdef_meta (const octave::cdef_meta_object& obj)
    : object (obj)
  { }

  octave_classdef_meta (const octave_classdef_meta&) = delete;

  octave_classdef_meta& operator = (const octave_classdef_meta&) = delete;

  ~octave_classdef_meta (void) { object.meta_release (); }

  bool is_classdef_meta (void) const { return true; }

  bool is_package (void) const { return object.is_package(); }

  octave_function * function_value (bool = false) { return this; }

  // We don't need to override all three forms of subsref.  The using
  // declaration will avoid warnings about partially-overloaded virtual
  // functions.
  using octave_function::subsref;

  octave_value_list
  subsref (const std::string& type,
           const std::list<octave_value_list>& idx,
           int nargout)
  {
    return object.meta_subsref (type, idx, nargout);
  }

  // We don't need to override both forms of the call method.  The using
  // declaration will avoid warnings about partially-overloaded virtual
  // functions.
  using octave_function::call;

  octave_value_list call (octave::tree_evaluator&, int nargout,
                          const octave_value_list& args)
  {
    // Emulate ()-type meta subsref

    std::list<octave_value_list> idx (1, args);
    std::string type ("(");

    return subsref (type, idx, nargout);
  }

  bool accepts_postfix_index (char type) const
  { return object.meta_accepts_postfix_index (type); }

  bool is_classdef_constructor (const std::string& cname = "") const;

  std::string doc_string (const std::string& meth_name) const;

private:

  octave::cdef_meta_object object;
};

class octave_classdef_superclass_ref : public octave_function
{
public:
  octave_classdef_superclass_ref (void) = delete;

  octave_classdef_superclass_ref (const std::string& meth,
                                  const std::string& cls)
    : octave_function (), m_method_name (meth), m_class_name (cls)
  { }

  octave_classdef_superclass_ref (const octave_classdef_superclass_ref&) = delete;

  octave_classdef_superclass_ref& operator = (const octave_classdef_superclass_ref&) = delete;

  ~octave_classdef_superclass_ref (void) = default;

  bool is_classdef_superclass_ref (void) const { return true; }

  octave_function * function_value (bool = false) { return this; }

  // We don't need to override both forms of the call method.  The using
  // declaration will avoid warnings about partially-overloaded virtual
  // functions.
  using octave_function::call;

  octave_value_list
  call (octave::tree_evaluator& tw, int nargout, const octave_value_list& idx);

private:

  bool is_constructed_object (octave::tree_evaluator& tw,
                              const std::string& nm);

private:

  std::string m_method_name;
  std::string m_class_name;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
