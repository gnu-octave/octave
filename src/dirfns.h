/*

Copyright (C) 1994, 1995, 1996, 1997, 2000, 2005, 2007 John W. Eaton

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

#if !defined (octave_dirfns_h)
#define octave_dirfns_h 1

#include <ctime>

#include <string>

#include "oct-time.h"

extern std::string polite_directory_format (const std::string&);
extern std::string base_pathname (const std::string&);
extern std::string make_absolute (const std::string&, const std::string&);
extern std::string get_working_directory (const std::string&);

// The time we last time we changed directories.
extern octave_time Vlast_chdir_time;

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
