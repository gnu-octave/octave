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

#if ! defined (octave_cdef_manager_h)
#define octave_cdef_manager_h 1

#include "octave-config.h"

#include "cdef-class.h"
#include "cdef-fwd.h"
#include "cdef-method.h"
#include "cdef-package.h"
#include "cdef-property.h"
#include "ov-builtin.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class interpreter;

class
OCTINTERP_API
cdef_manager
{
public:

  OCTINTERP_API cdef_manager (interpreter& interp);

  // No copying!

  cdef_manager (const cdef_manager&) = delete;

  cdef_manager& operator = (const cdef_manager&) = delete;

  ~cdef_manager (void) = default;

  OCTINTERP_API cdef_class
  find_class (const std::string& name, bool error_if_not_found = true,
              bool load_if_not_found = true);

  OCTINTERP_API octave_value
  find_method_symbol (const std::string& method_name,
                      const std::string& class_name);

  OCTINTERP_API cdef_package
  find_package (const std::string& name, bool error_if_not_found = true,
                bool load_if_not_found = true);

  OCTINTERP_API octave_value
  find_package_symbol (const std::string& pack_name);

  void register_class (const cdef_class& cls)
  {
    m_all_classes[cls.get_name ()] = cls;
  }

  void unregister_class (const cdef_class& cls)
  {
    m_all_classes.erase(cls.get_name ());
  }

  void register_package (const cdef_package& pkg)
  {
    m_all_packages[pkg.get_name ()] = pkg;
  }

  void unregister_package (const cdef_package& pkg)
  {
    m_all_packages.erase (pkg.get_name ());
  }

  const cdef_class& meta_class (void) const { return m_meta_class; }
  const cdef_class& meta_property (void) const { return m_meta_property; }
  const cdef_class& meta_method (void) const { return m_meta_method; }
  const cdef_class& meta_package (void) const { return m_meta_package; }

  const cdef_package& meta (void) const { return m_meta; }

  OCTINTERP_API cdef_class
  make_class (const std::string& name,
              const std::list<cdef_class>& super_list = std::list<cdef_class> ());

  OCTINTERP_API cdef_class
  make_class (const std::string& name, const cdef_class& super);

  OCTINTERP_API cdef_class
  make_meta_class (const std::string& name, const cdef_class& super);

  OCTINTERP_API cdef_property
  make_property (const cdef_class& cls, const std::string& name,
                 const octave_value& get_method = Matrix (),
                 const std::string& get_access = "public",
                 const octave_value& set_method = Matrix (),
                 const std::string& set_access = "public");

  OCTINTERP_API cdef_property
  make_attribute (const cdef_class& cls, const std::string& name);

  OCTINTERP_API cdef_method
  make_method (const cdef_class& cls, const std::string& name,
               const octave_value& fcn,
               const std::string& m_access = "public",
               bool is_static = false);

  OCTINTERP_API cdef_method
  make_method (const cdef_class& cls, const std::string& name,
               octave_builtin::fcn ff,
               const std::string& m_access = "public",
               bool is_static = false);

  OCTINTERP_API cdef_method
  make_method (const cdef_class& cls, const std::string& name,
               octave_builtin::meth mm,
               const std::string& m_access = "public",
               bool is_static = false);

  OCTINTERP_API cdef_package
  make_package (const std::string& nm, const std::string& parent = "");

  OCTINTERP_API octave_value
  find_method (const std::string& class_name,
               const std::string& name) const;

private:

  interpreter& m_interpreter;

  // All registered/loaded classes
  std::map<std::string, cdef_class> m_all_classes;

  // All registered/loaded packages
  std::map<std::string, cdef_package> m_all_packages;

  cdef_class m_meta_class;
  cdef_class m_meta_property;
  cdef_class m_meta_method;
  cdef_class m_meta_package;

  cdef_package m_meta;
};

OCTAVE_END_NAMESPACE(octave)

#endif
