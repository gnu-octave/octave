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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (octave_dynamic_ld_h)
#define octave_dynamic_ld_h 1

#include <string>

#include "oct-shlib.h"

class
octave_dynamic_loader
{
protected:

  octave_dynamic_loader (void) { }

public:

  virtual ~octave_dynamic_loader (void) { }

  static bool load (const std::string& fcn_name,
		    const std::string& file_name = std::string ());

  static bool remove (const std::string& fcn_name, octave_shlib& shl);

private:

  // No copying!

  octave_dynamic_loader (const octave_dynamic_loader&);

  octave_dynamic_loader& operator = (const octave_dynamic_loader&);

  static octave_dynamic_loader *instance;

  static bool instance_ok (void);

  bool do_load (const std::string& fcn_name,
		const std::string& file_name = std::string ());

  bool do_remove (const std::string& fcn_name, octave_shlib& shl);

  static bool doing_load;

protected:

  static std::string mangle_name (const std::string& name);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
