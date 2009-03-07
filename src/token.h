/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 2000, 2002, 2004, 2005,
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

#if !defined (octave_token_h)
#define octave_token_h 1

#include <string>

class
token
{
public:

  enum token_type
    {
      generic_token,
      string_token,
      double_token,
      ettype_token,
      pttype_token,
      sym_rec_token
    };

  enum end_tok_type
    {
      simple_end,
      for_end,
      function_end,
      if_end,
      switch_end,
      while_end,
      try_catch_end,
      unwind_protect_end
    };

  enum plot_tok_type
    {
      replot = 1,
      two_dee = 2,
      three_dee = 3
    };

  token (int l = -1, int c = -1);
  token (const std::string& s, int l = -1, int c = -1);
  token (double d, const std::string& s = std::string (),
	 int l = -1, int c = -1);
  token (end_tok_type t, int l = -1, int c = -1);
  token (plot_tok_type t, int l = -1, int c = -1);
  token (symbol_table::symbol_record *s, int l = -1, int c = -1);

  ~token (void);

  int line (void) { return line_num; }
  int column (void) { return column_num; }

  std::string text (void);
  double number (void);
  end_tok_type ettype (void);
  plot_tok_type pttype (void);
  symbol_table::symbol_record *sym_rec (void);

  std::string text_rep (void);

private:

  // No copying!

  token (const token& tok);

  token& operator = (const token& tok);

  int line_num;
  int column_num;
  token_type type_tag;
  union
    {
      std::string *str;
      double num;
      end_tok_type et;
      plot_tok_type pt;
      symbol_table::symbol_record *sr;
    };
  std::string orig_text;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
