/*

Copyright (C) 2008-2011 David Bateman

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <vector>
#include <iostream>
#include <string>

#include "regex-match.h"
#include "str-vec.h"
#include "oct-locbuf.h"

regex_match& 
regex_match::operator = (const regex_match& gm)
{
  if (this != &gm)
    {
#if HAVE_REGEX
      for (int i = 0; i < pat.length (); i++)
        regfree (compiled +i);
      delete [] compiled;
#endif
      pat = gm.pat;
      case_insen = gm.case_insen;
      init ();
    }
  return *this;
}

regex_match::~regex_match (void)
{
#if HAVE_REGEX
  for (int i = 0; i < pat.length (); i++)
    regfree (compiled +i);
  delete [] compiled;
#endif
}


void 
regex_match::set_pattern (const std::string& p) 
{ 
#if HAVE_REGEX
  for (int i = 0; i < pat.length (); i++)
    regfree (compiled +i);
  delete [] compiled;
#endif
  pat = p; 
  init ();
}

void 
regex_match::set_pattern (const string_vector& p) 
{ 
#if HAVE_REGEX
  for (int i = 0; i < pat.length (); i++)
    regfree (compiled +i);
  delete [] compiled;
#endif
  pat = p; 
  init ();
}

void
regex_match::init (void)
{
#ifdef HAVE_REGEX
  int npat = pat.length ();
  int err = 0;
  int i;

  compiled = new regex_t [npat];

  for (i = 0; i < npat; i++)
    {
      err = regcomp (compiled + i, pat(i).c_str (), 
                     (REG_NOSUB | REG_EXTENDED |
                      (case_insen ? REG_ICASE : 0)));
      if (err)
        break;
    }
  
  if (err)
    {
      int len = regerror (err, compiled + i, 0, 0);
      OCTAVE_LOCAL_BUFFER (char, errmsg, len);
      regerror(err, compiled + i, errmsg, len);
      (*current_liboctave_error_handler) ("%s in pattern (%s)", errmsg, 
                                          pat(i).c_str());

      for (int j = 0; j < i + 1; j++)
        regfree (compiled + j);
    }
#else
  (*current_liboctave_error_handler) 
    ("regex not available in this version of Octave"); 
#endif
}

bool
regex_match::match (const std::string& s)
{
#if HAVE_REGEX
  int npat = pat.length ();

  const char *str = s.c_str ();

  for (int i = 0; i < npat; i++)
    if (regexec (compiled + i, str, 0, 0, 0) == 0) 
      return true;
#endif

  return false;
}

Array<bool>
regex_match::match (const string_vector& s)
{
  int n = s.length ();

  Array<bool> retval (dim_vector (n, 1));

  for (int i = 0; i < n; i++)
    retval(i) = match (s[i]);

  return retval;
}
