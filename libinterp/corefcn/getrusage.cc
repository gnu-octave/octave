////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "oct-time.h"

#include "defun.h"
#include "oct-map.h"
#include "ov.h"
#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (getrusage, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{procstats} =} getrusage ()
Return a structure containing a number of statistics about the current
Octave process.

Not all fields are available on all systems.  If it is not possible to get
CPU time statistics, the CPU time slots are set to zero.  Other missing data
are replaced by NaN@.  The list of possible fields is:

@table @code
@item idrss
Unshared data size.

@item inblock
Number of block input operations.

@item isrss
Unshared stack size.

@item ixrss
Shared memory size.

@item majflt
Number of major page faults.

@item maxrss
Maximum data size.

@item minflt
Number of minor page faults.

@item msgrcv
Number of messages received.

@item msgsnd
Number of messages sent.

@item nivcsw
Number of involuntary context switches.

@item nsignals
Number of signals received.

@item nswap
Number of swaps.

@item nvcsw
Number of voluntary context switches.

@item oublock
Number of block output operations.

@item stime
A structure containing the system CPU time used.  The structure has the
elements @code{sec} (seconds) @code{usec} (microseconds).

@item utime
A structure containing the user CPU time used.  The structure has the
elements @code{sec} (seconds) @code{usec} (microseconds).
@end table
@end deftypefn */)
{
  octave_scalar_map ru_map;
  octave_scalar_map tv_map;

  sys::resource_usage rusage;

  sys::cpu_time cpu = rusage.cpu ();

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

OCTAVE_END_NAMESPACE(octave)
