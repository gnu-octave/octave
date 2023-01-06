////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2007-2023 The Octave Project Developers
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

#if ! defined (octave_ov_java_h)
#define octave_ov_java_h 1

#include "octave-config.h"

#include "ov.h"
#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class type_info;

OCTAVE_END_NAMESPACE(octave)

typedef void *voidptr;

class OCTINTERP_API octave_java : public octave_base_value
{
public:

  octave_java (void);

  octave_java (const voidptr& obj, void *cls = nullptr);

  octave_java (const octave_java& jobj)
    : octave_base_value (jobj), m_java_object (nullptr), m_java_class (nullptr)
  {
    init (jobj.m_java_object, jobj.m_java_class);
  }

  ~octave_java (void) { release (); }

  void * to_java (void) const { return m_java_object; }
  void * to_class (void) const { return m_java_class; }

  std::string java_class_name (void) const { return m_java_classname; }

  octave_base_value * clone (void) const { return new octave_java (*this); }
  octave_base_value * empty_clone (void) const { return new octave_java (); }

  bool is_instance_of (const std::string&) const;

  bool is_defined (void) const { return true; }

  bool is_constant (void) const { return true; }

  bool isstruct (void) const { return false; }

  bool isjava (void) const { return true; }

  string_vector map_keys (void) const;

  dim_vector dims (void) const;

  void print (std::ostream& os, bool pr_as_read_syntax = false);

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  bool save_ascii (std::ostream& os);

  bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool save_as_floats);

  bool load_binary (std::istream& is, bool swap,
                    octave::mach_info::float_format fmt);

  bool save_hdf5 (octave_hdf5_id loc_id, const char *name,
                  bool save_as_floats);

  bool load_hdf5 (octave_hdf5_id loc_id, const char *name);

  // We don't need to override all three forms of subsref.  The using
  // declaration will avoid warnings about partially-overloaded virtual
  // functions.
  using octave_base_value::subsref;

  octave_value_list
  subsref (const std::string& type,
           const std::list<octave_value_list>& idx, int nargout);

  octave_value
  subsref (const std::string& type, const std::list<octave_value_list>& idx)
  {
    octave_value_list retval = subsref (type, idx, 1);
    return (retval.length () > 0 ? retval(0) : octave_value ());
  }

  octave_value subsasgn (const std::string& type,
                         const std::list<octave_value_list>& idx,
                         const octave_value& rhs);

  octave_value convert_to_str_internal (bool pad, bool force, char type) const;

  bool is_java_string (void) const;

  octave_value do_javaMethod (void *jni_env, const std::string& name,
                              const octave_value_list& args);

  octave_value
  do_javaMethod (const std::string& name, const octave_value_list& args);

  static octave_value
  do_javaMethod (void *jni_env, const std::string& class_name,
                 const std::string& name, const octave_value_list& args);

  static octave_value
  do_javaMethod (const std::string& class_name, const std::string& name,
                 const octave_value_list& args);

  static octave_value
  do_javaObject (void *jni_env, const std::string& name,
                 const octave_value_list& args);

  static octave_value
  do_javaObject (const std::string& name, const octave_value_list& args);

  octave_value do_java_get (void *jni_env, const std::string& name);

  octave_value do_java_get (const std::string& name);

  static octave_value
  do_java_get (void *jni_env, const std::string& class_name,
               const std::string& name);

  static octave_value
  do_java_get (const std::string& class_name, const std::string& name);

  octave_value do_java_set (void *jni_env, const std::string& name,
                            const octave_value& val);

  octave_value do_java_set (const std::string& name, const octave_value& val);

  static octave_value
  do_java_set (void *jni_env, const std::string& class_name,
               const std::string& name, const octave_value& val);

  static octave_value
  do_java_set (const std::string& class_name, const std::string& name,
               const octave_value& val);

private:

  void init (void *jobj, void *jcls);

  void release (void);

private:

  void *m_java_object;

  void *m_java_class;

  std::string m_java_classname;

public:

  int type_id (void) const { return t_id; }
  std::string type_name (void) const { return t_name; }
  std::string class_name (void) const { return m_java_classname; }

  static int static_type_id (void) { return t_id; }
  static std::string static_type_name (void) { return t_name; }
  static std::string static_class_name (void) { return "<unknown>"; }
  static void register_type (octave::type_info&);

private:

  static int t_id;
  static const std::string t_name;
};

extern OCTINTERP_API bool Vjava_matrix_autoconversion;

extern OCTINTERP_API bool Vjava_unsigned_autoconversion;

extern OCTINTERP_API bool Vdebug_java;

#endif
