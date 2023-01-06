////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2012-2023 The Octave Project Developers
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

OCTAVE_BEGIN_NAMESPACE(octave)

class cdef_object;
class interpreter;
class tree_evaluator;
class type_info;

OCTAVE_END_NAMESPACE(octave)

class
octave_classdef : public octave_base_value
{
public:

  octave_classdef (void)
    : octave_base_value (), m_object () { }

  octave_classdef (const octave::cdef_object& obj)
    : octave_base_value (), m_object (obj) { }

  octave_classdef (const octave_classdef&) = delete;

  octave_classdef& operator = (const octave_classdef&) = delete;

  ~octave_classdef (void) = default;

  octave_base_value * clone (void) const
  {
    return new octave_classdef (m_object.clone ());
  }

  octave_base_value * empty_clone (void) const
  {
    return new octave_classdef (m_object.empty_clone ());
  }

  octave_classdef * classdef_object_value (bool = false) { return this; }

  octave::cdef_object get_object (void) const { return m_object; }

  octave::cdef_object& get_object_ref (void) { return m_object; }

  bool is_defined (void) const { return true; }

  bool isstruct (void) const { return false; }

  bool isobject (void) const { return true; }

  bool is_classdef_object (void) const { return true; }

  OCTINTERP_API void print (std::ostream& os, bool pr_as_read_syntax = false);

  OCTINTERP_API void
  print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  OCTINTERP_API bool is_instance_of (const std::string& cls_name) const;

  void break_closure_cycles (const std::shared_ptr<octave::stack_frame>& frame)
  {
    m_object.break_closure_cycles (frame);
  }

  OCTINTERP_API octave_value_list
  subsref (const std::string& type, const std::list<octave_value_list>& idx,
           int nargout);

  octave_value subsref (const std::string& type,
                        const std::list<octave_value_list>& idx)
  {
    octave_value_list retval = subsref (type, idx, 1);
    return (retval.length () > 0 ? retval(0) : octave_value ());
  }

  OCTINTERP_API octave_value
  subsref (const std::string& type, const std::list<octave_value_list>& idx,
           bool auto_add);

  OCTINTERP_API octave_value
  subsasgn (const std::string& type, const std::list<octave_value_list>& idx,
            const octave_value& rhs);

  OCTINTERP_API octave_value
  undef_subsasgn (const std::string& type,
                  const std::list<octave_value_list>& idx,
                  const octave_value& rhs);

  OCTINTERP_API Matrix size (void);

  OCTINTERP_API octave_idx_type xnumel (const octave_value_list&);

  string_vector map_keys (void) const { return m_object.map_keys (); }

  octave_map map_value (void) const { return m_object.map_value (); }

  dim_vector dims (void) const { return m_object.dims (); }

  void set_property (octave_idx_type idx, const std::string& name,
                     const octave_value& pval)
  {
    m_object.set_property (idx, name, pval);
  }

  octave_value
  get_property (octave_idx_type idx, const std::string& name) const
  {
    return m_object.get_property (idx, name);
  }

  static OCTINTERP_API octave_value
  superclass_ref (const std::string& meth, const std::string& cls);

  static OCTINTERP_API octave_value metaclass_query (const std::string& cls);

public:

  int type_id (void) const { return t_id; }
  std::string type_name (void) const { return t_name; }
  std::string class_name (void) const { return m_object.class_name (); }

  static int static_type_id (void) { return t_id; }
  static std::string static_type_name (void) { return t_name; }
  static std::string static_class_name (void) { return "<unknown>"; }
  static OCTINTERP_API void register_type (octave::type_info&);

private:

  octave::cdef_object m_object;

  static int t_id;

  static const std::string t_name;
};

void install_classdef (octave::interpreter& interp);

class octave_classdef_meta : public octave_function
{
public:

  octave_classdef_meta (const octave::cdef_meta_object& obj)
    : m_object (obj)
  { }

  octave_classdef_meta (const octave_classdef_meta&) = delete;

  octave_classdef_meta& operator = (const octave_classdef_meta&) = delete;

  ~octave_classdef_meta (void) { m_object.meta_release (); }

  bool is_classdef_meta (void) const { return true; }

  bool is_package (void) const { return m_object.is_package(); }

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
    return m_object.meta_subsref (type, idx, nargout);
  }

  // Override default call method because we don't push a new stack
  // frame for this operation on classdef_meta objects.

  octave_value_list call (octave::tree_evaluator& tw, int nargout,
                          const octave_value_list& args)
  {
    return execute (tw, nargout, args);
  }

  octave_value_list execute (octave::tree_evaluator&, int nargout,
                             const octave_value_list& args)
  {
    // Emulate ()-type meta subsref

    std::list<octave_value_list> idx (1, args);
    std::string type ("(");

    return subsref (type, idx, nargout);
  }

  bool accepts_postfix_index (char type) const
  { return m_object.meta_accepts_postfix_index (type); }

  OCTINTERP_API bool is_classdef_method (const std::string& cname = "") const;

  OCTINTERP_API bool
  is_classdef_constructor (const std::string& cname = "") const;

  OCTINTERP_API std::string doc_string (const std::string& meth_name) const;

  OCTINTERP_API std::string file_name (void) const;

private:

  octave::cdef_meta_object m_object;
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

  // Override default call method because we don't push a new stack
  // frame for this operation on classdef_superclass_ref objects.

  octave_value_list call (octave::tree_evaluator& tw, int nargout,
                          const octave_value_list& args)
  {
    return execute (tw, nargout, args);
  }

  OCTINTERP_API octave_value_list
  execute (octave::tree_evaluator& tw, int nargout,
           const octave_value_list& idx);

private:

  OCTINTERP_API bool
  is_constructed_object (octave::tree_evaluator& tw, const std::string& nm);

private:

  std::string m_method_name;
  std::string m_class_name;
};

#endif

