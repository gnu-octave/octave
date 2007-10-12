/*

Copyright (C) 1996, 1997 John W. Eaton

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

#if !defined (octave_tree_h)
#define octave_tree_h 1

#include <string>

#include <iostream>

class octave_function;
class tree_walker;

// Base class for the parse tree.

class
tree
{
public:

  tree (int l = -1, int c = -1) : break_point(false)
    {
      line_num = l;
      column_num = c;
    }

  virtual ~tree (void) { }

  virtual int line (void) const
    { return line_num; }

  virtual int column (void) const
    { return column_num; }

  virtual void accept (tree_walker& tw) = 0;

  std::string str_print_code (void);

  virtual void set_breakpoint (void)
    { break_point = true; }
  
  virtual void delete_breakpoint (void)
    { break_point = false; }

  virtual bool is_breakpoint (void) const 
    { return break_point; }

  // If true, stop executing at the next possible point.
  static bool break_next;
  
  // The line where dbnext was executed.
  static int last_line; 

  // The function where the last breakpoint occurred.
  static const octave_function *break_function;

  // The statement where the last breakpoint occurred.
  static const tree *break_statement;

private:

  // The input line and column where we found the text that was
  // eventually converted to this tree node.
  int line_num;
  int column_num;

  // Stop before executing this tree node
  bool break_point;

  // No copying!

  tree (const tree&);

  tree& operator = (const tree&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
