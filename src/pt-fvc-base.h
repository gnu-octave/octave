/*

Copyright (C) 1996 John W. Eaton

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

#if !defined (octave_tree_fvc_h)
#define octave_tree_fvc_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <ctime>

class ostream;

#include <string>

#include <SLList.h>

class octave_value;
class octave_value_list;

#include "pt-mvr-base.h"

// A base class for objects that can be evaluated with argument lists.

class
tree_fvc : public tree_multi_val_ret
{
public:
  tree_fvc (int l = -1, int c = -1) : tree_multi_val_ret (l, c) { }

  ~tree_fvc (void) { }

  virtual octave_value assign (octave_value& t,
				const octave_value_list& args);

  virtual string name (void) const;

  virtual void bump_value (tree_expression::type);

  virtual octave_value lookup_map_element (SLList<string>& list,
					    bool insert = false,
					    bool silent = false);

  virtual string fcn_file_name (void)
    { return string (); }

  virtual time_t time_parsed (void);

  virtual bool is_system_fcn_file (void) const
    { return false; }

  virtual int save (ostream& /* os */, bool /* mark_as_global */ = false,
		    int /* precision */ = 17);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
