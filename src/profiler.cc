/*

Copyright (C) 2011 Daniel Kraft

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

#include <iostream>

#include "Cell.h"
#include "defun.h"
#include "oct-time.h"
#include "ov-fcn.h"
#include "ov-struct.h"
#include "pager.h"
#include "profiler.h"

profile_data_accumulator::enter::enter (profile_data_accumulator& a,
                                        const octave_function& f)
  : acc (a)
{
  if (acc.is_active ())
    {
      fcn = &f;
      acc.enter_function (*fcn);
    }
  else
    fcn = NULL;
}

profile_data_accumulator::enter::~enter ()
{
  if (fcn)
    acc.exit_function (*fcn);
}

profile_data_accumulator::profile_data_accumulator ()
  : enabled (false), call_stack (), times (), last_time (-1.0)
{}

void
profile_data_accumulator::set_active (bool value)
{
  // If we enable, clear the call-stack.  This ensures we freshly start
  // with collecting times now.
  if (value)
    {
      while (!call_stack.empty ())
        call_stack.pop ();
    }

  enabled = value;
}

void
profile_data_accumulator::enter_function (const octave_function& fcn)
{
  // The enter class will check and only call us if the profiler is active.
  assert (is_active ());

  // If there is already an active function, add to its time before
  // pushing the new one.
  if (!call_stack.empty ())
    add_current_time ();

  call_stack.push (&fcn);
  last_time = query_time ();
}

void
profile_data_accumulator::exit_function (const octave_function& fcn)
{
  assert (!call_stack.empty ());
  assert (&fcn == call_stack.top ());

  // Usually, if we are disabled this function is not even called.  But the
  // call disabling the profiler is an exception.  So also check here
  // and only record the time if enabled.
  if (is_active ())
    add_current_time ();

  call_stack.pop ();

  // If this was an "inner call", we resume executing the parent function
  // up the stack.  So note the start-time for this!
  last_time = query_time ();
}

void
profile_data_accumulator::reset (void)
{
  if (is_active ())
    {
      error ("Can't reset active profiler.");
      return;
    }

  times.clear ();
  last_time = -1.0;
}

Cell
profile_data_accumulator::get_data (void) const
{
  const int n = times.size ();

  Cell result (1, n);
  int i = 0;
  for (timing_map::const_iterator p = times.begin (); p != times.end (); ++p)
    {
      octave_scalar_map entry;

      entry.contents ("name") = octave_value (p->first);
      entry.contents ("time") = octave_value (p->second);

      result (i++) = entry;
    }
  assert (i == n);

  return result;
}

double
profile_data_accumulator::query_time (void) const
{
  octave_time now;
  return now.double_value ();
}

void
profile_data_accumulator::add_current_time (void)
{
  const double t = query_time ();
  assert (last_time >= 0.0 && last_time <= t);

  assert (!call_stack.empty ());
  const std::string name = call_stack.top ()->profiler_name ();

  // If the key is not yet present in the map, it is constructed
  // with default value 0.
  times[name] += t - last_time;
}

profile_data_accumulator profiler;

DEFUN (profiler_enable, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} profiler_enable (enabled)\n\
Enable or disable the profiler data collection.\n\
@end deftypefn")
{
  // If there is an output argument, return current (if we change the old)
  // status of the profiler.
  octave_value_list result;
  if (nargout > 0)
    {
      if (nargout > 1)
        error ("profiler_enable: too many output arguments requested");

      result(0) = profiler.is_active ();
    }

  // If there is an input argument, set the status.
  const int nargin = args.length ();
  if (nargin > 0)
    {
      if (nargin > 1)
        error ("profiler_enable: too many arguments specified");

      profiler.set_active (args(0).bool_value ());
    }

  return result;
}

DEFUN (profiler_reset, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} profiler_reset ()\n\
Clear all collected profiling data.\n\
@end deftypefn")
{
  octave_value_list result;
  const int nargin = args.length ();

  if (nargin > 0)
    error ("profiler_reset: no arguments expected");
  if (nargout > 0)
    error ("profiler_reset: no output argument possible");

  profiler.reset ();

  return result;
}

DEFUN (profiler_data, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} data = profiler_data ()\n\
Query the timings collected by the profiler.\n\
@end deftypefn")
{
  octave_value_list result;
  const int nargin = args.length ();

  if (nargin > 0)
    error ("profiler_data: no arguments expected");
  if (nargout != 1)
    error ("profiler_reset: expected exactly one output argument");

  result(0) = profiler.get_data ();

  return result;
}
