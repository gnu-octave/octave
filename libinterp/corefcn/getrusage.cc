/*

Copyright (C) 1996-2015 John W. Eaton

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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "oct-time.h"

#include "defun.h"
#include "oct-map.h"
#include "ov.h"
#include "ovl.h"

DEFUN (getrusage, , ,
       "-*- texinfo -*-\n\
@deftypefn {} {} getrusage ()\n\
Return a structure containing a number of statistics about the current\n\
Octave process.\n\
\n\
Not all fields are available on all systems.  If it is not possible to get\n\
CPU time statistics, the CPU time slots are set to zero.  Other missing data\n\
are replaced by NaN@.  The list of possible fields is:\n\
\n\
@table @code\n\
@item idrss\n\
Unshared data size.\n\
\n\
@item inblock\n\
Number of block input operations.\n\
\n\
@item isrss\n\
Unshared stack size.\n\
\n\
@item ixrss\n\
Shared memory size.\n\
\n\
@item majflt\n\
Number of major page faults.\n\
\n\
@item maxrss\n\
Maximum data size.\n\
\n\
@item minflt\n\
Number of minor page faults.\n\
\n\
@item msgrcv\n\
Number of messages received.\n\
\n\
@item msgsnd\n\
Number of messages sent.\n\
\n\
@item nivcsw\n\
Number of involuntary context switches.\n\
\n\
@item nsignals\n\
Number of signals received.\n\
\n\
@item nswap\n\
Number of swaps.\n\
\n\
@item nvcsw\n\
Number of voluntary context switches.\n\
\n\
@item oublock\n\
Number of block output operations.\n\
\n\
@item stime\n\
A structure containing the system CPU time used.  The structure has the\n\
elements @code{sec} (seconds) @code{usec} (microseconds).\n\
\n\
@item utime\n\
A structure containing the user CPU time used.  The structure has the\n\
elements @code{sec} (seconds) @code{usec} (microseconds).\n\
@end table\n\
@end deftypefn")
{
  octave_scalar_map ru_map;
  octave_scalar_map tv_map;

  octave::sys::resource_usage rusage;

  octave::sys::cpu_time cpu = rusage.cpu ();

  tv_map.assign ("sec", cpu.user_sec ());
  tv_map.assign ("usec", cpu.user_usec ());
  ru_map.assign ("utime", octave_value (tv_map));

  tv_map.assign ("sec", cpu.system_sec ());
  tv_map.assign ("usec", cpu.system_usec ());
  ru_map.assign ("stime", octave_value (tv_map));

  ru_map.assign ("maxrss", static_cast<double> (rusage.maxrss ()));
  ru_map.assign ("ixrss", static_cast<double> (rusage.ixrss ()));
  ru_map.assign ("idrss", static_cast<double> (rusage.idrss ()));
  ru_map.assign ("isrss", static_cast<double> (rusage.isrss ()));
  ru_map.assign ("minflt", static_cast<double> (rusage.minflt ()));
  ru_map.assign ("majflt", static_cast<double> (rusage.majflt ()));
  ru_map.assign ("nswap", static_cast<double> (rusage.nswap ()));
  ru_map.assign ("inblock", static_cast<double> (rusage.inblock ()));
  ru_map.assign ("oublock", static_cast<double> (rusage.oublock ()));
  ru_map.assign ("msgsnd", static_cast<double> (rusage.msgsnd ()));
  ru_map.assign ("msgrcv", static_cast<double> (rusage.msgrcv ()));
  ru_map.assign ("nsignals", static_cast<double> (rusage.nsignals ()));
  ru_map.assign ("nvcsw", static_cast<double> (rusage.nvcsw ()));
  ru_map.assign ("nivcsw", static_cast<double> (rusage.nivcsw ()));

  return ovl (ru_map);
}

/*
%!test
%! r = getrusage ();
%! assert (isstruct (r));
%! assert (isfield (r, "idrss"));
%! assert (isfield (r, "inblock"));
%! assert (isfield (r, "isrss"));
%! assert (isfield (r, "ixrss"));
%! assert (isfield (r, "majflt"));
%! assert (isfield (r, "maxrss"));
%! assert (isfield (r, "minflt"));
%! assert (isfield (r, "msgrcv"));
%! assert (isfield (r, "msgsnd"));
%! assert (isfield (r, "nivcsw"));
%! assert (isfield (r, "nsignals"));
%! assert (isfield (r, "nswap"));
%! assert (isfield (r, "nvcsw"));
%! assert (isfield (r, "oublock"));
%! assert (isfield (r, "stime"));
%! assert (isfield (r, "utime"));
%! assert (isfield (r.stime, "sec"));
%! assert (isfield (r.stime, "usec"));
%! assert (isfield (r.utime, "sec"));
%! assert (isfield (r.utime, "usec"));
*/

