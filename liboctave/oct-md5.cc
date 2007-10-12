/*

Copyright (C) 2007 David Bateman

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

#include <string>
#include <vector>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "lo-error.h"
#include "oct-md5.h"
#include "md5.h"
 
std::string
oct_md5 (const std::string str)
{
  md5_state_t state;

  OCTAVE_LOCAL_BUFFER (md5_byte_t, digest, 16);
  md5_init (&state);
  md5_append (&state, reinterpret_cast<const md5_byte_t *>(str.c_str()),
	      str.length());
  md5_finish (&state, digest);

  OCTAVE_LOCAL_BUFFER (char, tmp, 33);
  for (octave_idx_type i = 0; i < 16; i++)
    sprintf (&tmp[2*i], "%02x", digest[i]);
  tmp[32] = 0;
  return std::string (tmp);
}
	  
std::string
oct_md5_file (const std::string file)
{
  FILE *ifile = fopen (file.c_str (), "rb");

  if (! ifile)
    {
      (*current_liboctave_error_handler) ("unable to open file `%s' for writing",
					  file.c_str());
      return std::string();
    }
  else
    {
      md5_state_t state;
      size_t nel;

      OCTAVE_LOCAL_BUFFER (md5_byte_t, digest, 16);
      OCTAVE_LOCAL_BUFFER (md5_byte_t, buf, 1024);

      md5_init (&state);

      while ((nel = fread (buf, 1, 1024, ifile)))
	md5_append (&state, buf, nel);

      fclose (ifile);

      md5_finish (&state, digest);

      OCTAVE_LOCAL_BUFFER (char, tmp, 33);
      for (octave_idx_type i = 0; i < 16; i++)
	sprintf (&tmp[2*i], "%02x", digest[i]);
      tmp[32] = 0;
      return std::string (tmp);
    }
}
	  
/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
