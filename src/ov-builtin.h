/*

Copyright (C) 1996, 1997, 1998, 1999, 2000, 2002, 2003, 2004, 2005,
              2007, 2008 John W. Eaton

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

#if !defined (octave_builtin_h)
#define octave_builtin_h 1

#include <string>

#include "ov-fcn.h"
#include "ov-typeinfo.h"

class octave_value;
class octave_value_list;

// Builtin functions.

class
OCTINTERP_API
octave_builtin : public octave_function
{
public:

  octave_builtin (void) { }

  typedef octave_value_list (*fcn) (const octave_value_list&, int);

  octave_builtin (fcn ff, const std::string& nm = std::string (),
		  const std::string& ds = std::string ())
    : octave_function (nm, ds), f (ff) { }

  ~octave_builtin (void) { }

  octave_value subsref (const std::string& type,
			const std::list<octave_value_list>& idx)
    {
      octave_value_list tmp = subsref (type, idx, 1);
      return tmp.length () > 0 ? tmp(0) : octave_value ();
    }

  octave_value_list subsref (const std::string& type,
			     const std::list<octave_value_list>& idx,
			     int nargout);

  octave_function *function_value (bool = false) { return this; }

  const octave_function *function_value (bool = false) const { return this; }

  bool is_builtin_function (void) const { return true; }

  octave_value_list
  do_multi_index_op (int nargout, const octave_value_list& args);

protected:

  // A pointer to the actual function.
  fcn f;

private:

  // No copying!

  octave_builtin (const octave_builtin& ob);

  octave_builtin& operator = (const octave_builtin& ob);

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
