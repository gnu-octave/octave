/*

Copyright (C) 2011-2012 Jacob Dawid

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <QString>

#include "ov.h"
#include "symtab.h"

#include "symbol-information.h"

bool
symbol_information::from_symbol_record
  (const symbol_table::symbol_record& symbol_record)
{
  if (symbol_record.is_local () && !symbol_record.is_global () && !symbol_record.is_hidden ())
    _scope = local;
  else if (symbol_record.is_global ())
    _scope = global;
  else if (symbol_record.is_persistent ())
    _scope = persistent;
  else
    _scope = unknown;

  _symbol = QString (symbol_record.name ().c_str ());
  _class  = QString (symbol_record.varval ().class_name ().c_str ());
  octave_value ov = symbol_record.varval ();

  // In case we have really large matrices or strings, cut them down
  // for performance reasons.
  QString short_value_string;
  bool use_short_value_string = false;
  if (ov.is_matrix_type () || ov.is_cell ())
    {
      if (ov.rows () * ov.columns () > 10)
        {
          use_short_value_string = true;
          short_value_string
            = QString ("%1x%2 items").arg (ov.rows ()).arg (ov.columns ());
        }
    }
  else if (ov.is_string ())
    {
      if (ov.string_value ().length () > 40)
        {
          use_short_value_string = true;
          short_value_string
            = QString::fromStdString (ov.string_value ().substr (0, 40));
        }
    }

  if (use_short_value_string)
    {
      _value = short_value_string;
    }
  else
    {
      std::stringstream buffer;
      ov.print (buffer, true);
      _value  = QString::fromStdString (buffer.str ());
    }
  _value.replace("\n", " ");

  if (ov.is_string ())
    _dimension = QString ("%1").arg (ov.string_value ().length ());
  else if (ov.is_range ())
    _dimension =  QString ("%1 : %2 : %3").arg (ov.range_value ().base ())
      .arg (ov.range_value ().inc ())
      .arg (ov.range_value ().limit ());
  else if (ov.is_matrix_type () || ov.is_cell ())
    _dimension = QString ("%1x%2").arg (ov.rows ())
      .arg (ov.columns ());
  else if (ov.is_function_handle ())
    // See code for func2str for a possible solution
    _dimension = QString ("func handle");
  else if (ov.is_inline_function ())
    // See code for formula for a possible solution
    _dimension = QString ("inline func");
  else
    _dimension = "1";

  return true;
}
