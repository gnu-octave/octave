/*

Copyright (C) 2007,2008,2009 John P. Swensen
Copyright (C) 2010, 2011 Jacob Dawid

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

// Born July 13, 2007.

#include "OctaveLink.h"

OctaveLink
  OctaveLink::m_singleton;


OctaveLink::OctaveLink ():QObject (), m_previousHistoryLength (0)
{
  m_symbolTableSemaphore = new QSemaphore (1);
  m_historySemaphore = new QSemaphore (1);
}

OctaveLink::~OctaveLink ()
{
}

int
OctaveLink::readlineEventHook ()
{
  OctaveLink::instance ()->processOctaveServerData ();
  return 0;
}

QString
OctaveLink::octaveValueAsQString (OctaveValue octaveValue)
{
  // Convert single qouted string.
  if (octaveValue.is_sq_string ())
    {
      return QString ("\'%1\'").arg (octaveValue.string_value ().c_str ());

      // Convert double qouted string.
    }
  else if (octaveValue.is_dq_string ())
    {
      return QString ("\"%1\"").arg (octaveValue.string_value ().c_str ());

      // Convert real scalar.
    }
  else if (octaveValue.is_real_scalar ())
    {
      return QString ("%1").arg (octaveValue.scalar_value ());

      // Convert complex scalar.
    }
  else if (octaveValue.is_complex_scalar ())
    {
      return QString ("%1 + %2i").arg (octaveValue.scalar_value ()).
	arg (octaveValue.complex_value ().imag ());

      // Convert range.
    }
  else if (octaveValue.is_range ())
    {
      return QString ("%1 : %2 : %3").arg (octaveValue.range_value ().
					   base ()).arg (octaveValue.
							 range_value ().
							 inc ()).
	arg (octaveValue.range_value ().limit ());

      // Convert real matrix.
    }
  else if (octaveValue.is_real_matrix ())
    {
      // TODO: Convert real matrix into a string.
      return QString ("{matrix}");

      // Convert complex matrix.
    }
  else if (octaveValue.is_complex_matrix ())
    {
      // TODO: Convert complex matrix into a string.
      return QString ("{complex matrix}");

      // If everything else does not fit, we could not recognize the type.
    }
  else
    {
      return QString ("<Type not recognized>");
    }
}

void
OctaveLink::fetchSymbolTable ()
{
  m_symbolTableSemaphore->acquire ();
  m_symbolTableBuffer.clear ();
  std::list < SymbolRecord > allVariables = symbol_table::all_variables ();
  std::list < SymbolRecord >::iterator iterator;
  for (iterator = allVariables.begin (); iterator != allVariables.end ();
       iterator++)
    m_symbolTableBuffer.append (iterator->dup ());
  m_symbolTableSemaphore->release ();
  emit symbolTableChanged ();
}


void
OctaveLink::fetchHistory ()
{
  int currentLen = command_history::length ();
  if (currentLen != m_previousHistoryLength)
    {
      m_historySemaphore->acquire ();
      for (int i = m_previousHistoryLength; i < currentLen; i++)
	{
	  m_historyBuffer.append (command_history::get_entry (i));
	}
      m_historySemaphore->release ();
      m_previousHistoryLength = currentLen;
      emit historyChanged ();
    }
}

QList < SymbolRecord > OctaveLink::currentSymbolTable ()
{
  QList < SymbolRecord > m_symbolTableCopy;

  // Generate a deep copy of the current symbol table.
  m_symbolTableSemaphore->acquire ();
  foreach (SymbolRecord symbolRecord, m_symbolTableBuffer)
    m_symbolTableCopy.append (symbolRecord.dup ());
  m_symbolTableSemaphore->release ();

  return m_symbolTableCopy;
}

string_vector
OctaveLink::currentHistory ()
{
  m_historySemaphore->acquire ();
  string_vector retval (m_historyBuffer);
  m_historyBuffer = string_vector ();
  m_historySemaphore->release ();
  return retval;
}

void
OctaveLink::processOctaveServerData ()
{
  fetchSymbolTable ();
  fetchHistory ();
}
