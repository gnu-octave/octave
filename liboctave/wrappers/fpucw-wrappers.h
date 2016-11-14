/*

Copyright (C) 2016 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_fpucw_wrappers_h)
#define octave_fpucw_wrappers_h 1

#if defined __cplusplus
extern "C" {
#endif

extern void octave_set_default_fpucw (void);

// unsigned int must match the actual type of fpucw_t.

extern unsigned int octave_begin_long_double_rounding (void);

extern void octave_end_long_double_rounding (unsigned int);

#if defined __cplusplus
}
#endif

#endif

