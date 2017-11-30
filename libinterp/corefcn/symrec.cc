/*

Copyright (C) 1993-2017 John W. Eaton
Copyright (C) 2009 VZLU Prague, a.s.

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

#include <sstream>

#include "file-ops.h"
#include "file-stat.h"
#include "oct-env.h"
#include "oct-time.h"

#include "fcn-info.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "symrec.h"
#include "symscope.h"
#include "symtab.h"

namespace octave
{
  symbol_record::context_id
  symbol_record::symbol_record_rep::get_decl_scope_context (void) const
  {
    return m_decl_scope ? m_decl_scope->current_context () : 0;
  }

  void
  symbol_record::symbol_record_rep::init_persistent (void)
  {
    if (auto t_fwd_rep = m_fwd_rep.lock ())
      {
        t_fwd_rep->init_persistent ();
        return;
      }

    mark_persistent ();
  }

  symbol_record::symbol_record_rep *
  symbol_record::symbol_record_rep::dup (symbol_scope *new_scope) const
  {
    // FIXME: is this the right thing do to?
    if (auto t_fwd_rep = m_fwd_rep.lock ())
      return t_fwd_rep->dup (new_scope);

    return new symbol_record_rep (new_scope, m_name, varval (),
                                  m_storage_class);
  }

  octave_value
  symbol_record::symbol_record_rep::dump (void) const
  {
    if (auto t_fwd_rep = m_fwd_rep.lock ())
      return t_fwd_rep->dump ();

    std::map<std::string, octave_value> m
      = {{ "name", m_name },
         { "local", is_local () },
         { "automatic", is_automatic () },
         { "formal", is_formal () },
         { "hidden", is_hidden () },
         { "inherited", is_inherited () },
         { "global", is_global () },
         { "persistent", is_persistent () }};

    octave_value val = varval ();

    if (val.is_defined ())
      m["value"] = val;

    return octave_value (m);
  }

  symbol_record::symbol_record (void)
    : m_rep (new symbol_record_rep (__get_current_scope__ ("symbol_record"),
                                    "", octave_value (), local))

  { }

  octave_value
  symbol_record::find (const octave_value_list& args) const
  {
    octave_value retval;

    symbol_table& symtab
      = __get_symbol_table__ ("symbol_record::find");

    retval = varval ();

    if (retval.is_undefined ())
      {
        // FIXME
        retval = symtab.find_function (name (), args);

        if (retval.is_defined ())
          return retval;
      }

    return retval;
  }

  octave_value symbol_record::dummy_octave_value;
}
