// help.cc                                             -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <iostream.h>

#include "builtins.h"
#include "help.h"

static help_list operators[] =
{
  { "!",
    "Logical not operator.  See also `~'.\n", },

  { "!=",
    "Logical not equals operator.  See also `~' and `<>'.\n", },

  { "\"",
    "String delimiter.\n", },

  { "#",
    "Begin comment character.  See also `%'.\n", },

  { "%",
    "Begin comment charcter.  See also `#'.\n", },

  { "&",
    "Logical and operator.  See also `&&'.\n", },

  { "&&",
    "Logical and operator.  See also `&'.\n", },

  { "'",
    "Matrix transpose operator.  For complex matrices, computes the\n\
complex conjugate (Hermitian) transpose.  See also `.''\n\
\n\
The single quote character may also be used to delimit strings, but\n\
it is better to use the double quote character, since that is never\n\
ambiguous\n", },

  { "(",
    "Array index or function argument delimiter.\n", },

  { ")",
    "Array index or function argument delimiter.\n", },

  { "*",
    "Multiplication operator.  See also `.*'\n", },

  { "**",
    "Power operator.  See also `^', `.**', and `.^'\n", },

  { "+",
    "Addition operator.\n", },

  { "++",
    "Increment operator.  As in C, may be applied as a prefix or postfix operator.\n", },

  { ",",
    "Array index, function argument, or command separator.\n", },

  { "-",
    "Subtraction or unary negation operator.\n", },

  { "--",
    "Decrement operator.  As in C, may be applied as a prefix or postfix operator.\n", },

  { ".'",
    "Matrix transpose operator.  For complex matrices, computes the\n\
transpose, *not* the complex conjugate transpose.  See also `''.\n", },

  { ".*",
    "Element by element multiplication operator.  See also `*'.\n", },

  { ".**",
    "Element by element power operator.  See also `**', `^', and `.^'.\n", },

  { "./",
    "Element by element division operator.  See also `/' and `\\'.\n", },

  { ".^",
    "Element by element division operator.  See also `/' and `\\'.\n", },

  { "/",
    "Right division.  See also `\\' and `./'.\n", },

  { ":",
    "Select entire rows or columns of matrices.\n", },

  { ";",
    "Array row or command separator.  See also `,'.\n", },

  { "<",
    "Less than operator.\n", },

  { "<=",
    "Less than or equals operator.\n", },

  { "<>",
    "Logical not equals operator.  See also `!=' and `~='.\n", },

  { "=",
    "Assignment operator.\n", },

  { "==",
    "Equality test operator.\n", },

  { ">",
    "Greater than operator.\n", },

  { ">=",
    "Greater than or equals operator.\n", },

  { "[",
    "Return list delimiter.  See also `]'.\n", },

  { "\\",
    "Left division operator.  See also `/' and `./'.\n", },

  { "]",
    "Return list delimiter.  See also `['.\n", },

  { "^",
    "Power operator.  See also `**', `.^', and `.**.'\n", },

  { "|",
    "Logical or operator.  See also `||'.\n", },

  { "||",
    "Logical or operator.  See also `|'.\n", },

  { "~",
    "Logical not operator.  See also `!' and `~'.\n", },

  { "~=",
    "Logical not equals operator.  See also `<>' and `!='.\n", },

  { (char *) NULL, (char *) NULL, },
};

static help_list keywords[] =
{
  { "break",
    "Exit the innermost enclosing while or for loop.\n", },

  { "continue",
    "Jump to the end of the innermost enclosing while or for loop.\n", },

  { "else",
    "Alternate action for an if block.\n", },

  { "elseif",
    "Alternate conditional test for an if block.\n", },

  { "end",
    "Mark the end of any for, if, while, or function block.\n", },

  { "endfor",
    "Mark the end of a for loop.\n", },

  { "endfunction",
    "Mark the end of a function.\n", },

  { "endif",
    "Mark the end of an if block.\n", },

  { "endwhile",
    "Mark the end of a while loop.\n", },

  { "for",
    "Begin a for loop.\n", },

  { "function",
    "Begin a function body.\n", },

  { "global",
    "Declare variables to have global scope.\n", },

  { "gplot",
    "Produce 2-D plots using gnuplot-like command syntax.\n", },

  { "gsplot",
    "Produce 3-D plots using gnuplot-like command syntax.\n", },

  { "if",
    "Begin an if block.\n", },

  { "return",
    "Return from a function.\n", },

  { "while",
    "Begin a while loop.\n", },

  { (char *) NULL, (char *) NULL, },
};

char **
names (help_list *lst, int& count)
{
  count = 0;
  help_list *ptr = lst;
  while (ptr->name != (char *) NULL)
    {
      count++;
      ptr++;
    }

  if (count == 0)
    return (char **) NULL;
    
  char **name_list = new char * [count+1];

  ptr = lst;
  int i = 0;
  while (ptr->name != (char *) NULL)
    {
      name_list[i++] = ptr->name;
      ptr++;
    }

  name_list[count] = (char *) NULL;
  return name_list;
}

help_list *
operator_help (void)
{
  return operators;
}

help_list *
keyword_help (void)
{
  return keywords;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
