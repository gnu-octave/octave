// token.h                                               -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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

#if !defined (octave_token_h)
#define octave_token_h 1

class symbol_record;

class token
{
public:
  enum token_type
    {
      generic_token,
      string_token,
      double_token,
      ettype_token,
      pttype_token,
      sym_rec_token,
    };

  enum end_tok_type
    {
      simple_end,
      for_end,
      function_end,
      if_end,
      while_end,
    };

  enum plot_tok_type
    {
      two_dee = 2,
      three_dee = 3,
    };

  token (int l = -1, int c = -1);
  token (char *s, int l = -1, int c = -1);
  token (double d, int l = -1, int c = -1);
  token (end_tok_type t, int l = -1, int c = -1);
  token (plot_tok_type t, int l = -1, int c = -1);
  token (symbol_record *s, int l = -1, int c = -1);

 ~token (void);

  int line (void);
  int column (void);

  char *string (void);
  double number (void);
  end_tok_type ettype (void);
  plot_tok_type pttype (void);
  symbol_record *sym_rec (void);

private:
  int line_num;
  int column_num;
  token_type type_tag;
  union
    {
      char *str;
      double num;
      end_tok_type et;
      plot_tok_type pt;
      symbol_record *sr;
    };
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
