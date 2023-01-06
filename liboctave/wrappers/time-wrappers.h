////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2016-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_time_wrappers_h)
#define octave_time_wrappers_h 1

#if defined __cplusplus
#  include <ctime>
#else
#  include <time.h>
#endif

#if defined __cplusplus
extern "C" {
#endif

extern OCTAVE_API int octave_gettimeofday_wrapper (time_t *sec, long *usec);

extern OCTAVE_API int
octave_cpu_time (time_t *usr_sec, time_t *sys_sec,
                 long *usr_usec, long *sys_usec);

extern OCTAVE_API int
octave_getrusage_wrapper (time_t *usr_sec, time_t *sys_sec,
                          long *usr_usec, long *sys_usec,
                          long *maxrss, long *ixrss, long *idrss,
                          long *isrss, long *minflt, long *majflt,
                          long *nswap, long *inblock, long *oublock,
                          long *msgsnd, long *msgrcv, long *nsignals,
                          long *nvcsw, long *nivcsw);

extern OCTAVE_API time_t
octave_mktime_wrapper (struct tm *tp);

#if defined __cplusplus
}
#endif

#endif
