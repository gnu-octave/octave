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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <iostream>

#include "ov.h"
#include "ov-classdef.h"
#include "pt-args-block.h"
#include "pt-classdef.h"
#include "pt-eval.h"

OCTAVE_BEGIN_NAMESPACE(octave)

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

      panic_unless (f);

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

std::string check_for_doc_string (comment_list *comments)
{
  // If the comment list ends in a block comment or full-line comment,
  // then it is the doc string for this property.

  if (comments)
    {
      comment_elt last_elt = comments->back ();

      if (last_elt.is_block () || last_elt.is_full_line ())
        return last_elt.text ();
    }

  return "";
}

tree_classdef_property::tree_classdef_property (tree_arg_validation *av,
    comment_list *comments)
  : m_av (av), m_comments (comments),
    m_doc_string (check_for_doc_string (m_comments))
{ }

tree_classdef_property::~tree_classdef_property (void)
{
  delete m_av;
}

tree_identifier *tree_classdef_property::ident (void)
{
  tree_expression *id_expr = m_av->identifier_expression ();

  return dynamic_cast<tree_identifier *> (id_expr);
}

tree_expression *tree_classdef_property::expression (void)
{
  return m_av->initializer_expression ();
}

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

tree_classdef_event::tree_classdef_event (tree_identifier *i,
    comment_list *comments)
  : m_id (i), m_comments (comments),
    m_doc_string (check_for_doc_string (m_comments))
{ }

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

tree_classdef_enum::tree_classdef_enum (tree_identifier *i,
                                        tree_expression *e,
                                        comment_list *comments)
  : m_id (i), m_expr (e), m_comments (comments),
    m_doc_string (check_for_doc_string (m_comments))
{ }

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

tree_classdef_body::tree_classdef_body (void)
  : m_properties_lst (), m_methods_lst (), m_events_lst (), m_enum_lst ()
{ }

tree_classdef_body::tree_classdef_body (tree_classdef_properties_block *pb)
  : m_properties_lst (), m_methods_lst (), m_events_lst (), m_enum_lst (),
    m_doc_string (pb ? get_doc_string (pb->leading_comment ()) : "")
{
  append (pb);
}

tree_classdef_body::tree_classdef_body (tree_classdef_methods_block *mb)
  : m_properties_lst (), m_methods_lst (), m_events_lst (), m_enum_lst (),
    m_doc_string (mb ? get_doc_string (mb->leading_comment ()) : "")
{
  append (mb);
}

tree_classdef_body::tree_classdef_body (tree_classdef_events_block *evb)
  : m_properties_lst (), m_methods_lst (), m_events_lst (), m_enum_lst (),
    m_doc_string (evb ? get_doc_string (evb->leading_comment ()) : "")
{
  append (evb);
}

tree_classdef_body::tree_classdef_body (tree_classdef_enum_block *enb)
  : m_properties_lst (), m_methods_lst (), m_events_lst (), m_enum_lst (),
    m_doc_string (enb ? get_doc_string (enb->leading_comment ()) : "")
{
  append (enb);
}

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

std::string
tree_classdef_body::get_doc_string (comment_list *comments) const
{
  // Grab the first comment from the list and use it as the doc string
  // for this classdef body.

  if (comments)
    {
      comment_elt first_elt = comments->front ();

      return first_elt.text ();
    }

  return "";
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

OCTAVE_END_NAMESPACE(octave)
