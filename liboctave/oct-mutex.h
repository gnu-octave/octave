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

    virtual ~octave_mutex (void)
      {
	if (rep && --rep->count == 0)
	  {
	    delete rep;
	    rep = 0;
	  }
      }

    octave_mutex& operator = (const octave_mutex& m)
      {
	if (rep != m.rep)
	  {
	    if (rep && --rep->count == 0)
	      delete rep;

	    rep = m.rep;
	    rep->count++;
	  }

	return *this;
      }

    virtual void lock (void)
      { rep->lock (); }

    virtual void unlock (void)
      { rep->unlock (); }

protected:
    explicit octave_mutex (int /* dummy */)
	: rep (0) { }

protected:
    union
      {
	octave_mutex *rep;
	int count;
      };
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
