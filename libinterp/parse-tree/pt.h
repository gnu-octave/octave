/*

Copyright (C) 1996-2016 John W. Eaton

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

#if ! defined (octave_pt_h)
#define octave_pt_h 1

#include "octave-config.h"

#include <string>

#include <iosfwd>

class octave_function;
class tree_walker;
class bp_table;
bool meets_condition (std::string *);

// Base class for the parse tree.

class
tree
{
public:

  tree (int l = -1, int c = -1)
    : line_num (l), column_num (c), bp (NULL) { }

  virtual ~tree (void) { }

  virtual int line (void) const { return line_num; }

  virtual int column (void) const { return column_num; }

  void line (int l) { line_num = l; }

  void column (int c) { column_num = c; }

  void set_location (int l, int c)
  {
    line_num = l;
    column_num = c;
  }

  virtual void set_breakpoint (const std::string& condition)
  {
    if (bp)
      *bp = condition;
    else
      bp = new std::string(condition);
  }

  virtual void delete_breakpoint (void) { if (bp) delete bp; bp = NULL; }

  bool meets_bp_condition (void) const;

  bool is_breakpoint (bool check_active = false) const
  { return bp && (! check_active || meets_bp_condition ()); }

  // breakpoint condition, or "0" (i.e., "false") if no breakpoint.
  // To distinguish "0" from a disabled breakpoint, test "is_breakpoint" too.
  const std::string bp_cond (void) const
  { return bp ? *bp : std::string("0"); }

  std::string str_print_code (void);

  virtual void accept (tree_walker& tw) = 0;

private:

  // The input line and column where we found the text that was
  // eventually converted to this tree node.
  int line_num;
  int column_num;

  // Breakpoint flag: NULL if no breakpoint, or the condition if there is one
  std::string *bp;

  // No copying!

  tree (const tree&);

  tree& operator = (const tree&);
};

#endif

