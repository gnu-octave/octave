/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_pager_h)
#define octave_pager_h 1

#include <iostream>
#include <strstream>
#include <string>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

class
octave_pager_buf : public strstreambuf
{
public:

  octave_pager_buf (int size = 0) : strstreambuf (size) { }

  void flush_current_contents_to_diary (void);

protected:

  int sync (void);
};

class
octave_pager_stream : public ostream
{
protected:

  octave_pager_stream (void);

public:

  ~octave_pager_stream (void);

  void flush_current_contents_to_diary (void);

  static octave_pager_stream& stream (void);

private:

  static octave_pager_stream *instance;

  octave_pager_buf *pb;

  // No copying!

  octave_pager_stream (const octave_pager_stream&);

  octave_pager_stream& operator = (const octave_pager_stream&);
};

class
octave_diary_buf : public strstreambuf
{
public:

  octave_diary_buf (int size = 0) : strstreambuf (size) { }

protected:

  int sync (void);
};

class
octave_diary_stream : public ostream
{
protected:

  octave_diary_stream (void);

public:

  ~octave_diary_stream (void);

  static octave_diary_stream& stream (void);

private:

  static octave_diary_stream *instance;

  octave_diary_buf *db;

  // No copying!

  octave_diary_stream (const octave_diary_stream&);

  octave_diary_stream& operator = (const octave_diary_stream&);
};

#define octave_stdout (octave_pager_stream::stream ())

#define octave_diary (octave_diary_stream::stream ())

extern void flush_octave_stdout (void);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
