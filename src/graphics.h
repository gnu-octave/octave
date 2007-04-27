/*

Copyright (C) 2007 John W. Eaton

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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (graphics_h)
#define graphics_h 1

#include <string>
#include "ov.h"

extern bool
set_property_in_handle (double handle, const std::string &property,
			const octave_value &arg,
			const std::string &func = std::string());

extern octave_value
get_property_from_handle (double handle, const std::string &property,
			  const std::string &func = std::string());
#endif

/*
;; Local Variables: ***
;; mode: C++ ***
;; End: ***
*/
