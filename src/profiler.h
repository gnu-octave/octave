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

#if !defined (octave_profiler_h)
#define octave_profiler_h 1

#include <stack>
#include <map>

class octave_function;
class Cell;

class
OCTAVE_API
profile_data_accumulator
{
public:

  // This is a utility class that can be used to call the enter/exit
  // functions in a manner protected from stack unwinding.
  class enter
  {
    private:

      profile_data_accumulator& acc;

      const octave_function* fcn;

    public:

      enter (profile_data_accumulator&, const octave_function& fcn);

      virtual ~enter (void);

    private:

      // No copying!

      enter (const enter&);

      enter& operator = (const enter&);
  };

  profile_data_accumulator (void);

  bool is_active (void) const { return enabled; }

  void set_active (bool);

  void reset (void);

  Cell get_data (void) const;

private:

  bool enabled;

  std::stack<const octave_function*> call_stack;

  typedef std::map<std::string, double> timing_map;
  timing_map times;

  // Store last timestamp we had, when the currently active function was called.
  double last_time;

  // These are private as only the unwind-protecting inner class enter
  // should be allowed to call them.
  void enter_function (const octave_function&);
  void exit_function (const octave_function&);

  // Query a timestamp, used for timing calls (obviously).
  // This is not static because in the future, maybe we want a flag
  // in the profiler or something to choose between cputime, wall-time
  // user-time, system-time, ...
  double query_time () const;

  // Add the time elapsed since last_time to the function on the top
  // of our call-stack.  This is called from two different positions,
  // thus it is useful to have it as a seperate function.
  void add_current_time (void);

  // No copying!

  profile_data_accumulator (const profile_data_accumulator&);

  profile_data_accumulator& operator = (const profile_data_accumulator&);
};

// The instance used.
extern profile_data_accumulator profiler;

#endif
