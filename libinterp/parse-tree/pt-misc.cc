/*

Copyright (C) 1994-2017 John W. Eaton

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

#include "Cell.h"

#include "defun.h"
#include "error.h"
#include "ov.h"
#include "oct-lvalue.h"
#include "pt-id.h"
#include "pt-idx.h"
#include "pt-misc.h"
#include "pt-walk.h"
#include "utils.h"

namespace octave
{
  // Parameter lists.

  tree_parameter_list::~tree_parameter_list (void)
  {
    while (! empty ())
      {
        iterator p = begin ();
        delete *p;
        erase (p);
      }
  }

  void
  tree_parameter_list::mark_as_formal_parameters (void)
  {
    for (tree_decl_elt *elt : *this)
      elt->mark_as_formal_parameter ();
  }

  bool
  tree_parameter_list::validate (in_or_out type)
  {
    bool retval = true;

    std::set<std::string> dict;

    for (tree_decl_elt *elt : *this)
      {
        tree_identifier *id = elt->ident ();

        if (id)
          {
            std::string name = id->name ();

            if (id->is_black_hole ())
              {
                if (type != in)
                  error ("invalid use of ~ in output list");
              }
            else if (dict.find (name) != dict.end ())
              error ("'%s' appears more than once in parameter list",
                     name.c_str ());
            else
              dict.insert (name);
          }
      }

    std::string va_type = (type == in ? "varargin" : "varargout");

    size_t len = length ();

    if (len > 0)
      {
        tree_decl_elt *elt = back ();

        tree_identifier *id = elt->ident ();

        if (id && id->name () == va_type)
          {
            if (len == 1)
              mark_varargs_only ();
            else
              mark_varargs ();

            iterator p = end ();
            --p;
            delete *p;
            erase (p);
          }
      }

    return retval;
  }

  std::list<std::string>
  tree_parameter_list::variable_names (void) const
  {
    std::list<std::string> retval;

    for (tree_decl_elt *elt : *this)
      retval.push_back (elt->name ());

    return retval;
  }

  bool
  tree_parameter_list::is_defined (void)
  {
    bool status = true;

    for (tree_decl_elt *elt : *this)
      {
        if (! elt->is_variable ())
          {
            status = false;
            break;
          }
      }

    return status;
  }

  tree_parameter_list *
  tree_parameter_list::dup (symbol_table::scope_id scope,
                            symbol_table::context_id context) const
  {
    tree_parameter_list *new_list = new tree_parameter_list ();

    if (takes_varargs ())
      new_list->mark_varargs ();

    for (const tree_decl_elt *elt : *this)
      new_list->append (elt->dup (scope, context));

    return new_list;
  }

  // Return lists.

  tree_return_list::~tree_return_list (void)
  {
    while (! empty ())
      {
        iterator p = begin ();
        delete *p;
        erase (p);
      }
  }
}
