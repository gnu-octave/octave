/*

Copyright (C) 2012-2017 John W. Eaton

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

#include "ov-classdef.h"
#include "pt-classdef.h"

namespace octave
{
  // Classdef attribute

  // Classdef attribute_list

  tree_classdef_attribute_list::~tree_classdef_attribute_list (void)
  {
    while (! empty ())
      {
        iterator p = begin ();
        delete *p;
        erase (p);
      }
  }

  // Classdef superclass

  // Classdef superclass_list

  tree_classdef_superclass_list::~tree_classdef_superclass_list (void)
  {
    while (! empty ())
      {
        iterator p = begin ();
        delete *p;
        erase (p);
      }
  }

  // Classdef property

  // Classdef property_list

  tree_classdef_property_list::~tree_classdef_property_list (void)
  {
    while (! empty ())
      {
        iterator p = begin ();
        delete *p;
        erase (p);
      }
  }

  // Classdef properties_block

  // Classdef methods_list

  // Classdef methods_block

  // Classdef event

  // Classdef events_list

  tree_classdef_events_list::~tree_classdef_events_list (void)
  {
    while (! empty ())
      {
        iterator p = begin ();
        delete *p;
        erase (p);
      }
  }

  // Classdef events_block

  // Classdef enum

  // Classdef enum_list

  tree_classdef_enum_list::~tree_classdef_enum_list (void)
  {
    while (! empty ())
      {
        iterator p = begin ();
        delete *p;
        erase (p);
      }
  }

  // Classdef enum_block

  // Classdef body

  tree_classdef_body::~tree_classdef_body (void)
  {
    while (! properties_lst.empty ())
      {
        properties_list_iterator p = properties_lst.begin ();
        delete *p;
        properties_lst.erase (p);
      }

    while (! methods_lst.empty ())
      {
        methods_list_iterator p = methods_lst.begin ();
        delete *p;
        methods_lst.erase (p);
      }

    while (! events_lst.empty ())
      {
        events_list_iterator p = events_lst.begin ();
        delete *p;
        events_lst.erase (p);
      }

    while (! enum_lst.empty ())
      {
        enum_list_iterator p = enum_lst.begin ();
        delete *p;
        enum_lst.erase (p);
      }
  }

  // Classdef

  octave_function*
  tree_classdef::make_meta_class (interpreter& interp, bool is_at_folder)
  {
    cdef_class cls = cdef_class::make_meta_class (interp, this, is_at_folder);

    if (cls.ok ())
      return cls.get_constructor_function ();

    return nullptr;
  }
}
