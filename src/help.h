// help.h                                              -*- C++ -*-
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

#if !defined (octave_help_h)
#define octave_help_h 1

class ostrstream;

typedef struct help_list
{
  char *name;
  char *help;
};

extern char **names (help_list *l, int& count);
extern help_list *operator_help (void);
extern help_list *keyword_help (void);

extern int help_from_list (ostrstream& output_buf,
			   const help_list *list, const char *string,
			   int usage);

extern void print_usage (const char *s, int just_usage = 0);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
