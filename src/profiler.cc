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

#include "defun.h"
#include "oct-time.h"
#include "ov-struct.h"
#include "pager.h"
#include "profiler.h"

profile_data_accumulator::enter::enter (profile_data_accumulator& a,
                                        const std::string& f)
  : acc (a)
{
  if (acc.is_active ())
    {
      fcn = f;
      acc.enter_function (fcn);
    }
  else
    fcn = "";
}

profile_data_accumulator::enter::~enter ()
{
  if (fcn != "")
    acc.exit_function (fcn);
}

profile_data_accumulator::stats::stats ()
  : time (0.0), calls (0), recursive (false),
    parents (), children ()
{}

// With the help of a mapping name -> index, convert a function_set list
// to an Octave array of indices.
octave_value
profile_data_accumulator::stats::function_set_value (const function_set& list,
                                                     const fcn_index_map& idx)
{
  const octave_idx_type n = list.size ();

  RowVector retval (n);
  octave_idx_type i = 0;
  for (function_set::const_iterator p = list.begin (); p != list.end (); ++p)
    {
      fcn_index_map::const_iterator q = idx.find (*p);
      assert (q != idx.end ());
      retval (i) = q->second;
      ++i;
    }
  assert (i == n);

  return retval;
}

profile_data_accumulator::profile_data_accumulator ()
  : enabled (false), call_stack (), data (), last_time (-1.0)
{}

void
profile_data_accumulator::set_active (bool value)
{
  // If we enable, clear the call-stack.  This ensures we freshly start
  // with collecting times now.
  if (value)
    {
      while (!call_stack.empty ())
        call_stack.pop_back ();
    }

  enabled = value;
}

void
profile_data_accumulator::enter_function (const std::string& fcn)
{
  // The enter class will check and only call us if the profiler is active.
  assert (is_active ());

  // If there is already an active function, add to its time before
  // pushing the new one.
  if (!call_stack.empty ())
    add_current_time ();

  // Update non-timing related data for the function entered.
  stats& entry = data[fcn];
  ++entry.calls;
  if (!call_stack.empty ())
    {
      const std::string parent_name = call_stack.back ();
      entry.parents.insert (parent_name);
      data[parent_name].children.insert (fcn);
    }
  if (!entry.recursive)
    for (call_stack_type::iterator i = call_stack.begin ();
         i != call_stack.end (); ++i)
      if (*i == fcn)
        {
          entry.recursive = true;
          break;
        }

  call_stack.push_back (fcn);
  last_time = query_time ();
}

void
profile_data_accumulator::exit_function (const std::string& fcn)
{
  assert (!call_stack.empty ());
  assert (fcn == call_stack.back ());

  // Usually, if we are disabled this function is not even called.  But the
  // call disabling the profiler is an exception.  So also check here
  // and only record the time if enabled.
  if (is_active ())
    add_current_time ();

  call_stack.pop_back ();

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

  data.clear ();
  last_time = -1.0;
}

octave_value
profile_data_accumulator::get_data (void) const
{
  const octave_idx_type n = data.size ();

  // For the parent/child data, we need to map function key-names
  // to the indices they correspond to in the output array.  Find them out
  // in a preparation step.
  fcn_index_map fcn_indices;
  octave_idx_type i = 0;
  for (stats_map::const_iterator p = data.begin (); p != data.end (); ++p)
    {
      fcn_indices[p->first] = i + 1;
      ++i;
    }
  assert (i == n);

  Cell rv_names (n, 1);
  Cell rv_times (n, 1);
  Cell rv_calls (n, 1);
  Cell rv_recursive (n, 1);
  Cell rv_parents (n, 1);
  Cell rv_children (n, 1);

  i = 0;
  for (stats_map::const_iterator p = data.begin (); p != data.end (); ++p)
    {
      const stats& entry = p->second;

      rv_names (i) = octave_value (p->first);
      rv_times (i) = octave_value (entry.time);
      rv_calls (i) = octave_value (entry.calls);
      rv_recursive (i) = octave_value (entry.recursive);
      rv_parents (i) = stats::function_set_value (entry.parents, fcn_indices);
      rv_children (i) = stats::function_set_value (entry.children, fcn_indices);

      ++i;
    }
  assert (i == n);

  Octave_map retval;

  retval.assign ("FunctionName", rv_names);
  retval.assign ("TotalTime", rv_times);
  retval.assign ("NumCalls", rv_calls);
  retval.assign ("IsRecursive", rv_recursive);
  retval.assign ("Parents", rv_parents);
  retval.assign ("Children", rv_children);

  return retval;
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
  const std::string name = call_stack.back ();

  // The entry for this function should already be created; namely
  // when entering the function via the non-timing data collection!
  stats_map::iterator pos = data.find (name);
  assert (pos != data.end ());
  pos->second.time += t - last_time;
}

profile_data_accumulator profiler;

// Enable or disable the profiler data collection.
DEFUN (__profiler_enable, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Function File} __profiler_enable ()\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value_list retval;

  const int nargin = args.length ();
  if (nargin > 0)
    {
      if (nargin > 1)
        {
          print_usage ();
          return retval;
        }

      profiler.set_active (args(0).bool_value ());
    }

  retval(0) = profiler.is_active ();

  return retval;
}

// Clear all collected profiling data.
DEFUN (__profiler_reset, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Function File} __profiler_reset ()\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value_list retval;
  const int nargin = args.length ();

  if (nargin > 0)
    warning ("profiler_reset: ignoring extra arguments");

  profiler.reset ();

  return retval;
}

// Query the timings collected by the profiler.
DEFUN (__profiler_data, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Function File} __profiler_data ()\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value_list retval;
  const int nargin = args.length ();

  if (nargin > 0)
    warning ("profiler_data: ignoring extra arguments");

  retval(0) = profiler.get_data ();

  return retval;
}
