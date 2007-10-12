/*

Copyright (C) 1996, 1997, 2005, 2006, 2007 John W. Eaton

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

#if !defined (octave_prog_args_h)
#define octave_prog_args_h 1

struct
long_options
{
  const char *name;
  int has_arg;
  int *flag;
  int val;
};

class
OCTAVE_API
prog_args
{
public:

  // These values must match the corresponding defines in getopt.h.
  enum option_argument
    {
      no_arg = 0,
      required_arg = 1,
      optional_arg = 2
    };

  prog_args (int argc, char *const *argv, const char *s_opts, const
	     long_options* l_opts = 0)
    : xargc (argc), xargv (argv), short_opts (s_opts), long_opts (l_opts)
      {
	init ();
      }

  ~prog_args (void) { }

  int getopt (void);

  const char *optarg (void);

  int optind (void);

private:

  // Number of args.
  int xargc;

  // Program args.
  char *const *xargv;

  // Single character options.
  const char *short_opts;

  // Long options.
  const long_options *long_opts;

  void init (void);

  prog_args (const prog_args&);

  prog_args& operator = (const prog_args&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
