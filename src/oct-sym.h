/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_symbol_h)
#define octave_symbol_h 1

#include <ctime>

#include <string>

class tree_walker;
class octave_value;
class octave_value_list;

class
octave_symbol
{
public:

  virtual ~octave_symbol (void) { }

  virtual octave_value eval (void) = 0;

  virtual octave_value_list eval (int, const octave_value_list&) = 0;

  virtual bool is_constant (void) const = 0;

  virtual bool is_system_fcn_file (void) { return false; }

  virtual string fcn_file_name (void) const { return string (); }

  virtual time_t time_parsed (void) const { return 0; }
};

#endif

/*
;; Local Variables: ***
;; mode: C++ ***
;; End: ***
*/
