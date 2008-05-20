/*

Copyright (C) 2008  David Bateman

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

#if !defined (octave_regex_match_h)
#define octave_regex_match_h 1

#include <string>

#if defined (HAVE_REGEX)
#if defined (__MINGW32__)
#define __restrict
#endif
#if defined (HAVE_SYS_TYPES_H)
#include <sys/types.h>
#endif
#include <regex.h>
#endif

#include "Array.h"
#include "str-vec.h"

class
OCTAVE_API
regex_match
{
public:

  regex_match (const std::string& p, bool insen = false) 
    : pat (p), case_insen (insen) { init (); }

  regex_match (const string_vector& p = string_vector (), bool insen = false) 
    : pat (p), case_insen (insen) { init (); }

  regex_match (const regex_match& gm) 
    : pat (gm.pat), case_insen (gm.case_insen) { init (); }

  regex_match& operator = (const regex_match& gm);

  ~regex_match (void);

  void set_pattern (const std::string& p);

  void set_pattern (const string_vector& p);

  bool match (const std::string&);

  Array<bool> match (const string_vector&);

private:

  void init (void);

  // Regex pattern(s).
  string_vector pat;

  // Should match be case insensitive
  bool case_insen;

#if HAVE_REGEX
  regex_t *compiled;
#endif

};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
