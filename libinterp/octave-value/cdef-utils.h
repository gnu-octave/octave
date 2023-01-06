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

#if ! defined (octave_cdef_utils_h)
#define octave_cdef_utils_h 1

#include "octave-config.h"

#include <list>
#include <string>

#include "cdef-fwd.h"

class octave_value;
class Cell;

OCTAVE_BEGIN_NAMESPACE(octave)

extern OCTINTERP_API std::string
get_base_name (const std::string& nm);

extern OCTINTERP_API void
make_function_of_class (const std::string& class_name,
                        const octave_value& fcn);

extern OCTINTERP_API void
make_function_of_class (const cdef_class& cls, const octave_value& fcn);

extern OCTINTERP_API cdef_class
lookup_class (const std::string& name, bool error_if_not_found = true,
              bool load_if_not_found = true);

extern OCTINTERP_API cdef_class
lookup_class (const cdef_class& cls);

extern OCTINTERP_API cdef_class
lookup_class (const octave_value& ov);

extern OCTINTERP_API std::list<cdef_class>
lookup_classes (const Cell& cls_list);

extern OCTINTERP_API octave_value
to_ov (const cdef_object& obj);

extern OCTINTERP_API octave_value
to_ov (const octave_value& ov);

extern OCTINTERP_API cdef_object
to_cdef (const octave_value& val);

extern OCTINTERP_API cdef_object&
to_cdef_ref (const octave_value& val);

extern OCTINTERP_API cdef_object
to_cdef (const cdef_object& obj);

extern OCTINTERP_API octave_value
to_ov (const std::list<cdef_class>& class_list);

extern OCTINTERP_API bool
is_dummy_method (const octave_value& fcn);

extern OCTINTERP_API bool
is_superclass (const cdef_class& clsa, const cdef_class& clsb,
               bool allow_equal = true, int max_depth = -1);
extern OCTINTERP_API bool
is_strict_superclass (const cdef_class& clsa, const cdef_class& clsb);

extern OCTINTERP_API bool
is_direct_superclass (const cdef_class& clsa, const cdef_class& clsb);

extern OCTINTERP_API cdef_package
lookup_package (const std::string& name, bool error_if_not_found = true,
                bool load_if_not_found = true);

extern OCTINTERP_API cdef_class
get_class_context (std::string& name, bool& in_constructor);

extern OCTINTERP_API cdef_class
get_class_context (void);

extern OCTINTERP_API bool
check_access (const cdef_class& cls, const octave_value& acc,
              const std::string& meth_name = "",
              const std::string& prop_name = "",
              bool is_prop_set = false);

OCTAVE_END_NAMESPACE(octave)

#endif
