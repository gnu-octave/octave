// pt-base.h                                           -*- C++ -*-
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

#if !defined (octave_tree_base_h)
#define octave_tree_base_h 1

#if defined (__GNUG__)
#pragma interface
#endif

class ostream;

// How to print the code that the trees represent.

class
tree_print_code
{
public:
  virtual ~tree_print_code (void) { }

  virtual void print_code (ostream& os) = 0;

  void reset_indent_level (void)
    { curr_print_indent_level = 0; }

  void increment_indent_level (void)
    { curr_print_indent_level += 2; }

  void decrement_indent_level (void)
    { curr_print_indent_level -= 2; }

  void print_code_new_line (ostream& os);

  void print_code_indent (ostream& os);

  void print_code_reset (void);

private:
  static int curr_print_indent_level;
  static bool beginning_of_line;
};

// Base class for the parse tree.

class
tree : public tree_print_code
{
public:
  tree (int l = -1, int c = -1)
    {
      line_num = l;
      column_num = c;
    }

  virtual int line (void) const
    { return line_num; }

  virtual int column (void) const
    { return column_num; }

private:
  int line_num;
  int column_num;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
