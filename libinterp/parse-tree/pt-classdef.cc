/*

Copyright (C) 2012-2019 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "ov.h"
#include "ov-classdef.h"
#include "pt-classdef.h"
#include "pt-eval.h"

namespace octave
{
  tree_superclass_ref *
  tree_superclass_ref::dup (symbol_scope&) const
  {
    tree_superclass_ref *new_scr
      = new tree_superclass_ref (m_method_name, m_class_name,
                                 line (), column ());

    new_scr->copy_base (*this);

    return new_scr;
  }

  octave_value_list
  tree_superclass_ref::evaluate_n (tree_evaluator& tw, int nargout)
  {
    octave_value tmp
      = octave_classdef::superclass_ref (m_method_name, m_class_name);

    if (! is_postfix_indexed ())
      {
        // There was no index, so this superclass_ref object is not
        // part of an index expression.  It is also not an identifier in
        // the syntax tree but we need to handle it as if it were.  So
        // call the function here.

        octave_function *f = tmp.function_value (true);

        assert (f);

        return f->call (tw, nargout);
      }

    // The superclass_ref function object will be indexed as part of the
    // enclosing index expression.

    return ovl (tmp);
  }

  tree_metaclass_query *
  tree_metaclass_query::dup (symbol_scope&) const
  {
    tree_metaclass_query *new_mcq
      = new tree_metaclass_query (m_class_name, line (), column ());

    new_mcq->copy_base (*this);

    return new_mcq;
  }

  octave_value
  tree_metaclass_query::evaluate (tree_evaluator&, int)
  {
    return octave_classdef::metaclass_query (m_class_name);
  }

  // Classdef attribute

  // Classdef attribute_list

  tree_classdef_attribute_list::~tree_classdef_attribute_list (void)
  {
    while (! empty ())
      {
        auto p = begin ();
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
        auto p = begin ();
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
        auto p = begin ();
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
        auto p = begin ();
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
        auto p = begin ();
        delete *p;
        erase (p);
      }
  }

  // Classdef enum_block

  // Classdef body

  tree_classdef_body::~tree_classdef_body (void)
  {
    while (! m_properties_lst.empty ())
      {
        auto p = m_properties_lst.begin ();
        delete *p;
        m_properties_lst.erase (p);
      }

    while (! m_methods_lst.empty ())
      {
        auto p = m_methods_lst.begin ();
        delete *p;
        m_methods_lst.erase (p);
      }

    while (! m_events_lst.empty ())
      {
        auto p = m_events_lst.begin ();
        delete *p;
        m_events_lst.erase (p);
      }

    while (! m_enum_lst.empty ())
      {
        auto p = m_enum_lst.begin ();
        delete *p;
        m_enum_lst.erase (p);
      }
  }

  // Classdef

  octave_value
  tree_classdef::make_meta_class (interpreter& interp, bool is_at_folder)
  {
    cdef_class cls = cdef_class::make_meta_class (interp, this, is_at_folder);

    if (cls.ok ())
      return cls.get_constructor_function ();

    return octave_value ();
  }
}
