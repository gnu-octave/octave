/*

Copyright (C) 1996, 1997, 1999, 2000, 2002, 2003, 2004, 2005, 2007, 2008
              John W. Eaton

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

#if !defined (octave_dld_function_h)
#define octave_dld_function_h 1

#include <string>

#include "oct-shlib.h"

#include "ov-fcn.h"
#include "ov-builtin.h"
#include "ov-typeinfo.h"

class octave_shlib;

class octave_value;
class octave_value_list;

// Dynamically-linked functions.

class
OCTINTERP_API
octave_dld_function : public octave_builtin
{
public:

  octave_dld_function (void) { }

  octave_dld_function (octave_builtin::fcn ff, const octave_shlib& shl,
		       const std::string& nm = std::string (),
		       const std::string& ds = std::string ());

  ~octave_dld_function (void);

  void mark_fcn_file_up_to_date (const octave_time& t) { t_checked = t; }

  std::string fcn_file_name (void) const;

  octave_time time_parsed (void) const;

  octave_time time_checked (void) const { return t_checked; }

  bool is_system_fcn_file (void) const { return system_fcn_file; }

  bool is_builtin_function (void) const { return false; }

  bool is_dld_function (void) const { return true; }
  
  static octave_dld_function* create (octave_builtin::fcn ff,
      const octave_shlib& shl,
      const std::string& nm = std::string (),
      const std::string& ds = std::string ());

private:

  octave_shlib sh_lib;

  // The time the file was last checked to see if it needs to be
  // parsed again.
  mutable octave_time t_checked;

  // True if this function came from a file that is considered to be a
  // system function.  This affects whether we check the time stamp
  // on the file to see if it has changed.
  bool system_fcn_file;

  // No copying!

  octave_dld_function (const octave_dld_function& fn);

  octave_dld_function& operator = (const octave_dld_function& fn);

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
