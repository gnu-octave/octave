// pt-fvc-base.h                                      -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

class tree_constant;
class Octave_object;

#include "pt-mvr-base.h"

// A base class for objects that can be evaluated with argument lists.

class
tree_fvc : public tree_multi_val_ret
{
public:
  tree_fvc (int l = -1, int c = -1) : tree_multi_val_ret (l, c) { }

  ~tree_fvc (void) { }

  virtual tree_constant assign (tree_constant& t,
				const Octave_object& args);

  virtual string name (void) const;

  virtual void bump_value (tree_expression::type);

  virtual tree_constant lookup_map_element (SLList<string>& list,
					    int insert = 0, int silent = 0);

  virtual string fcn_file_name (void)
    { return string (); }

  virtual time_t time_parsed (void);

  virtual int is_system_fcn_file (void) const
    { return 0; }

  virtual int save (ostream& /* os */, int /* mark_as_global */ = 0,
		    int /* precision */ = 17);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
