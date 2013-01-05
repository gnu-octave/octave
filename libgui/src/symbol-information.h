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
class symbol_information
{
public:

  enum Scope
    {
      unknown     = 0,
      local       = 1,
      global      = 2,
      persistent  = 3
    };

  symbol_information (const symbol_table::symbol_record& symbol_record);

  symbol_information (const symbol_information& x)
    : _scope (x._scope), _symbol (x._symbol), _class_name (x._class_name),
      _value (x._value), _dimension (x._dimension), _hash (x._hash)
  { }

  symbol_information operator = (const symbol_information& x)
  {
    if (this != &x)
      {
        _scope = x._scope;
        _symbol = x._symbol;
        _class_name = x._class_name;
        _value = x._value;
        _dimension = x._dimension;
        _hash = x._hash;
      }

    return *this;
  }

  ~symbol_information (void) { }

  QString symbol (void) const { return _symbol; }
  QString class_name (void) const { return _class_name; }
  QString value (void) const
    {
      return QString::fromUtf8 (_value.toStdString ().data (),
                                _value.toStdString ().size ());
    }
  QString dimension (void) const { return _dimension; }
  Scope scope (void) const { return _scope; }

  friend bool
  operator == (const symbol_information& a, const symbol_information& b)
  {
    return (a.hash () == b.hash ()
            && a.scope () == b.scope ()
            && a.symbol () == b.symbol ()
            && a.class_name () == b.class_name ()
            && a.value () == b.value ()
            && a.dimension () == b.dimension ());
  }

private:

  // FIXME -- this is not really the scope of the symbol.
  Scope _scope;

  QString _symbol;
  QString _class_name;
  QString _value;
  QString _dimension;

  int _hash;

  int hash (void) const { return _hash; }
};

#endif // SYMBOLINFORMATION_H
