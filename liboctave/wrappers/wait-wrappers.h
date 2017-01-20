/*

Copyright (C) 2016 John W. Eaton

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

#if ! defined (octave_wait_wrappers_h)
#define octave_wait_wrappers_h 1

#include <sys/types.h>

#if ! defined (__cplusplus)
#  include <stdbool.h>
#endif

#if defined __cplusplus
extern "C" {
#endif

extern pid_t octave_waitpid_wrapper (pid_t pid, int *statusp, int options);

extern int octave_wcontinue_wrapper (void);

extern int octave_wcoredump_wrapper (int status);

extern bool octave_wifcontinued_wrapper (int status);

extern bool octave_wifexited_wrapper (int status);

extern bool octave_wifsignaled_wrapper (int status);

extern bool octave_wifstopped_wrapper (int status);

extern int octave_wexitstatus_wrapper (int status);

extern int octave_wnohang_wrapper (void);

extern int octave_wstopsig_wrapper (int status);

extern int octave_wtermsig_wrapper (int status);

extern int octave_wuntraced_wrapper (void);

#if defined __cplusplus
}
#endif

#endif
