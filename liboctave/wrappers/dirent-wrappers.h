/*

Copyright (C) 2016-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if ! defined (octave_dirent_wrappers_h)
#define octave_dirent_wrappers_h 1

#if ! defined (__cplusplus)
#  include <stdbool.h>
#endif

#if defined __cplusplus
extern "C" {
#endif

extern void * octave_opendir_wrapper (const char *dname);

extern char * octave_readdir_wrapper (void *dir);

extern void octave_rewinddir_wrapper (void *dir);

extern int octave_closedir_wrapper (void *dir);

extern unsigned int octave_name_max_wrapper (void);

#if defined __cplusplus
}
#endif

#endif
