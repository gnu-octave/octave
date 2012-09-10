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

#ifndef SYMBOLINFORMATION_H
#define SYMBOLINFORMATION_H

// FIXME -- we should not be including config.h in header files.

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <QString>
#include <QHash>

#include <sstream>

#include "symtab.h"

/**
  * \struct symbol_information
  * \brief Meta-information over a symbol-table entry.
  * \author Jacob Dawid
  * This struct is used to store meta information over a symbol entry.
  * It reduces memory consumption, since it only stores relevant data
  * about a symbol-table entry that will be used in the model for the
  * graphical user interface.
  */
struct symbol_information
{
  enum Scope
  {
    local       = 0,
    global      = 1,
    persistent  = 2,
    hidden      = 3
  };

  QString _symbol;
  QString _class;
  QString _value;
  QString _dimension;
  Scope   _scope;

  /** Hashes the symbol information for quickly comparing it. */
  int
  hash () const
  {
    return qHash (_symbol) + qHash (_class) + qHash (_value)
      + qHash (_dimension) + (int)_scope;
  }

  /** Compares two symbol information objects. */
  bool
  equals (const symbol_information& other) const
  {
    if (hash () == other.hash ())
      {
        return _symbol == other._symbol
            && _class  == other._class
            && _value  == other._value
            && _scope  == other._scope
            && _dimension == other._dimension;
      }
  }

  /** Extracts meta information from a given symbol record. */
  bool
  from_symbol_record (const symbol_table::symbol_record& symbol_record);
};

#endif // SYMBOLINFORMATION_H
