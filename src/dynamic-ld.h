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

#if !defined (octave_dynamic_ld_h)
#define octave_dynamic_ld_h 1

#include <string>

class octave_builtin;

class
octave_dynamic_loader
{
protected:

  octave_dynamic_loader (void);

public:

  typedef octave_builtin * (*builtin_fcn) (void);

  virtual ~octave_dynamic_loader (void);

  static int load_fcn_from_dot_oct_file (const string& fcn_name);

private:

  static octave_dynamic_loader *instance;

  virtual builtin_fcn
  resolve_reference (const string& mangled_name, const string& oct_file);

  string mangle_name (const string& name);

  // No copying!

  octave_dynamic_loader (const octave_dynamic_loader&);

  octave_dynamic_loader& operator = (const octave_dynamic_loader&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
