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

symbol_information::symbol_information (const symbol_table::symbol_record& sr)
{
  if (sr.is_local () && !sr.is_global () && !sr.is_hidden ())
    _scope = local;
  else if (sr.is_global ())
    _scope = global;
  else if (sr.is_persistent ())
    _scope = persistent;
  else
    _scope = unknown;

  _symbol = QString (sr.name ().c_str ());
  _class_name = QString (sr.varval ().class_name ().c_str ());
  octave_value ov = sr.varval ();
  dim_vector dv = ov.dims ();

  // In case we have really large matrices or strings, cut them down
  // for performance reasons.
  QString short_value_string;
  bool use_short_value_string = false;
  if (ov.is_range ())
    {
      use_short_value_string = true;

      Range r = ov.range_value ();

      double base = r.base ();
      double increment = r.base ();
      double limit = r.base ();

      std::stringstream buffer;

      buffer << base << ":";
      if (increment != 1 )
        buffer << increment << ":";
      buffer << limit;
      
      short_value_string = QString::fromStdString (buffer.str ());
    }
  else if (ov.is_matrix_type () || ov.is_cell ())
    {
      if (ov.numel () > 10)
        use_short_value_string = true;
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

  _dimension = QString::fromStdString (dv.str ());

  _hash = _scope + qHash (_symbol) + qHash (_class_name) + qHash (_value)
    + qHash (_dimension);
}
