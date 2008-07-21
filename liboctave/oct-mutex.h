/*

Copyright (C) 2008 Michael Goffioul

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

#if !defined (octave_octave_mutex_h)
#define octave_octave_mutex_h 1

class octave_mutex;

class
octave_base_mutex
{
public:
  friend class octave_mutex;

  octave_base_mutex (void) : count (1) { }

  virtual ~octave_base_mutex (void) { }

  virtual void lock (void);

  virtual void unlock (void);

private:
  int count;
};

class
OCTAVE_API
octave_mutex
{
public:
  octave_mutex (void);

  octave_mutex (const octave_mutex& m)
  {
    rep = m.rep;
    rep->count++;
  }

  ~octave_mutex (void)
  {
    if (--rep->count == 0)
      delete rep;
  }

  octave_mutex& operator = (const octave_mutex& m)
  {
    if (rep != m.rep)
      {
	if (--rep->count == 0)
	  delete rep;

	rep = m.rep;
	rep->count++;
      }

    return *this;
  }

  void lock (void)
  {
    rep->lock ();
  }

  void unlock (void)
  {
    rep->unlock ();
  }

protected:
  octave_base_mutex *rep;
};

class
octave_autolock
{
public:
  octave_autolock (const octave_mutex& m)
    : mutex (m)
  {
    mutex.lock ();
  }

  ~octave_autolock (void)
  {
    mutex.unlock ();
  }

private:

  // No copying or default constructor!
  octave_autolock (void);
  octave_autolock (const octave_autolock&);
  octave_autolock& operator = (const octave_autolock&);

private:
  octave_mutex mutex;
};

#endif
