/* OctaveGUI - A graphical user interface for Octave
 * Copyright (C) 2011 Jacob Dawid (jacob.dawid@googlemail.com)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef SYMBOLINFORMATION_H
#define SYMBOLINFORMATION_H

#include <QString>
#include <QHash>

// Octave includes
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#undef PACKAGE_URL
#include "octave/config.h"
#include "octave/cmd-edit.h"
#include "octave/error.h"
#include "octave/file-io.h"
#include "octave/input.h"
#include "octave/lex.h"
#include "octave/load-path.h"
#include "octave/octave.h"
#include "octave/oct-hist.h"
#include "octave/oct-map.h"
#include "octave/oct-obj.h"
#include "octave/ops.h"
#include "octave/ov.h"
#include "octave/ov-usr-fcn.h"
#include "octave/symtab.h"
#include "octave/pt.h"
#include "octave/pt-eval.h"
#include "octave/config.h"
#include "octave/Range.h"
#include "octave/toplev.h"
#include "octave/procstream.h"
#include "octave/sighandlers.h"
#include "octave/debug.h"
#include "octave/sysdep.h"
#include "octave/ov.h"
#include "octave/unwind-prot.h"
#include "octave/utils.h"
#include "octave/variables.h"

typedef struct SymbolInformation
{
  enum Scope
  {
    Local       = 0,
    Global      = 1,
    Persistent  = 2,
    Hidden      = 3
  };

  QString _symbol;
  QString _type;
  QString _value;
  Scope   _scope;

  int
  hash () const
  {
    return qHash (_symbol) + qHash (_type) + qHash (_value) + (int)_scope;
  }

  bool
  equals (const SymbolInformation& other) const
  {
    if (hash () == other.hash ())
      {
        return _symbol == other._symbol
            && _type   == other._type
            && _value  == other._value
            && _scope  == other._scope;
      }
  }

  bool
  fromSymbolRecord (const symbol_table::symbol_record& symbolRecord)
  {
    if (symbolRecord.is_local () && !symbolRecord.is_global () && !symbolRecord.is_hidden ())
      _scope = Local;
    else if (symbolRecord.is_global ())
      _scope = Global;
    else if (symbolRecord.is_persistent ())
      _scope = Persistent;
    else if (symbolRecord.is_hidden ())
      _scope = Hidden;

    _symbol = QString (symbolRecord.name ().c_str ());
    _type   = QString (symbolRecord.varval ().type_name ().c_str ());
    octave_value octaveValue = symbolRecord.varval ();

    // For every type, convert to a human readable string.
    if (octaveValue.is_sq_string ())
      _value = QString ("\'%1\'").arg (octaveValue.string_value ().c_str ());
    else if (octaveValue.is_dq_string ())
      _value = QString ("\"%1\"").arg (octaveValue.string_value ().c_str ());
    else if (octaveValue.is_real_scalar ())
      _value = QString ("%1").arg (octaveValue.scalar_value ());
    else if (octaveValue.is_complex_scalar ())
      _value = QString ("%1 + %2i").arg (octaveValue.scalar_value ())
                                   .arg (octaveValue.complex_value ().imag ());
    else if (octaveValue.is_range ())
      _value =  QString ("%1 : %2 : %3").arg (octaveValue.range_value ().base ())
                                        .arg (octaveValue.range_value ().inc ())
                                        .arg (octaveValue.range_value ().limit ());
    else if (octaveValue.is_real_matrix ())
      _value = QString ("%1x%2").arg (octaveValue.rows ())
                                .arg (octaveValue.columns ());
    else if (octaveValue.is_complex_matrix ())
      _value = QString ("%1x%2").arg (octaveValue.rows ())
                                .arg (octaveValue.columns ());
    else
      _value = QString ("<Type not recognized>");

    return true;
  }
} SymbolInformation;



#endif // SYMBOLINFORMATION_H
