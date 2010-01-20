/*

Copyright (C) 2001, 2005, 2006, 2007, 2008, 2009
              John W. Eaton and Paul Kienzle

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

#include <list>
#include <map>
#include <string>

#include "Cell.h"
#include "oct-map.h"
#include "defun-dld.h"
#include "ov.h"
#include "ov-fcn.h"
#include "ov-typeinfo.h"
#include "pager.h"
#include "parse.h"
#include "symtab.h"
#include "variables.h"

DEFUN_DLD (builtin, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@dots{}]} builtin (@var{f}, @dots{})\n\
Call the base function @var{f} even if @var{f} is overloaded to\n\
some other function for the given type signature.\n\
@seealso{dispatch}\n\
@end deftypefn")
{
  octave_value_list retval; 

  int nargin = args.length ();

  if (nargin > 0)
    {
      const std::string name (args(0).string_value ());
 
      if (! error_state)
        {
          octave_value fcn = symbol_table::builtin_find (name);

          if (fcn.is_defined ())
            retval = feval (fcn.function_value (), args.splice (0, 1),
                            nargout);
          else
            error ("builtin: lookup for symbol `%s' failed", name.c_str ());
        }
      else
        error ("builtin: expecting function name as first argument");
    }
  else
    print_usage ();

  return retval;
}

DEFUN_DLD (dispatch, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} dispatch (@var{f}, @var{r}, @var{type})\n\
\n\
Replace the function @var{f} with a dispatch so that function @var{r}\n\
is called when @var{f} is called with the first argument of the named\n\
@var{type}.  If the type is @var{any} then call @var{r} if no other type\n\
matches.  The original function @var{f} is accessible using\n\
@code{builtin (@var{f}, @dots{})}.\n\
\n\
If @var{r} is omitted, clear dispatch function associated with @var{type}.\n\
\n\
If both @var{r} and @var{type} are omitted, list dispatch functions\n\
for @var{f}.\n\
@seealso{builtin}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  std::string f, r, t;

  if (nargin > 0 && nargin < 4)
    {
      if (nargin > 0)
        {
          f = args(0).string_value ();

          if (error_state)
            {
              error ("dispatch: expecting first argument to be function name");
              return retval;
            }
        }

      if (nargin > 1)
        {
          r = args(1).string_value ();

          if (error_state)
            {
              error ("dispatch: expecting second argument to be function name");
              return retval;
            }
        }

      if (nargin > 2)
        {
          t = args(2).string_value ();

          if (error_state)
            {
              error ("dispatch: expecting third argument to be type name");
              return retval;
            }
        }

      if (nargin == 1)
        {
          if (nargout > 0)
            {
              symbol_table::fcn_info::dispatch_map_type dm
                = symbol_table::get_dispatch (f);

              size_t len = dm.size ();

              Cell type_field (len, 1);
              Cell name_field (len, 1);

              symbol_table::fcn_info::dispatch_map_type::const_iterator p
                = dm.begin ();

              for (size_t i = 0; i < len; i++)
                {
                  type_field(i) = p->first;
                  name_field(i) = p->second;

                  p++;
                }

              Octave_map m;

              m.assign ("type", type_field);
              m.assign ("name", name_field);

              retval = m;
            }
          else
            symbol_table::print_dispatch (octave_stdout, f);
        }
      else if (nargin == 2)
        {
          t = r;
          symbol_table::clear_dispatch (f, t);
        }
      else
        symbol_table::add_dispatch (f, t, r);
    }
  else
    print_usage ();

  return retval;
}

/*

%!test # builtin function replacement
%! dispatch('sin','length','string')
%! assert(sin("abc"),3)
%! assert(sin(0),0,10*eps); 
%!test # 'any' function
%! dispatch('sin','exp','any')
%! assert(sin(0),1,eps);
%! assert(sin("abc"),3);
%!test # 'builtin' function
%! assert(builtin('sin',0),0,eps);
%! builtin('eval','x=1;');
%! assert(x,1);
%!test # clear function mapping
%! dispatch('sin','string')
%! dispatch('sin','any')
%! assert(sin(0),0,10*eps);
%!test # oct-file replacement
%! dispatch('fft','length','string')
%! assert(fft([1,1]),[2,0]);
%! assert(fft("abc"),3)
%! dispatch('fft','string');
%!test # m-file replacement
%! dispatch('hamming','length','string')
%! assert(hamming(1),1)
%! assert(hamming("abc"),3)
%! dispatch('hamming','string')

%!test # override preloaded builtin
%! evalin('base','cos(1);');
%! dispatch('cos','length','string')
%! evalin('base','assert(cos("abc"),3)');
%! evalin('base','assert(cos(0),1,eps)');
%! dispatch('cos','string')
%!test # override pre-loaded oct-file
%! evalin('base','qr(1);');
%! dispatch('qr','length','string')
%! evalin('base','assert(qr("abc"),3)');
%! evalin('base','assert(qr(1),1)');
%! dispatch('qr','string');
%!test # override pre-loaded m-file
%! evalin('base','hanning(1);');
%! dispatch('hanning','length','string')
%! evalin('base','assert(hanning("abc"),3)');
%! evalin('base','assert(hanning(1),1)');
%! dispatch('hanning','string');

## The following tests have been disabled because creating functions
## on the fly causes trouble (filesystem timestamp resolution?) and so
## leads people to complain about the failed tests when the dispatch
## mechanism is working fine, but it is really the creation of the
## functions that is failing.  And anyway, this method of function
## dispatch should be considered obsolete and probably removed from
## Octave now that we have classes.
##
## FIXME I would rather not create dispatch_x/dispatch_y
## in the current directory!  I don't want them installed accidentally.
## 
## %!function echo_to_file (str, name)
## %!  fid = fopen (name, 'w');
## %!  if (fid != -1)
## %!    fprintf (fid, str);
## %!    fprintf (fid, '\n');
## %!    fclose (fid);
## %!  endif
## 
## %!test # replace base m-file
## %! echo_to_file ('function a=dispatch_x(a)', "dispatch_x.m");
## %! dispatch('dispatch_x','length','string')
## %! assert(dispatch_x(3),3)
## %! assert(dispatch_x("a"),1)
## %! sleep (2);
## %! echo_to_file ('function a=dispatch_x(a),++a;', "dispatch_x.m");
## %! rehash();
## %! assert(dispatch_x(3),4)
## %! assert(dispatch_x("a"),1)
## %!test 
## %! unlink("dispatch_x.m");
## 
## %!test # replace dispatch m-file
## %! echo_to_file ('function a=dispatch_y(a)', "dispatch_y.m");
## %! dispatch('hello','dispatch_y','complex scalar')
## %! assert(hello(3i),3i)
## %! sleep (2);
## %! echo_to_file ('function a=dispatch_y(a),++a;', "dispatch_y.m");
## %! rehash();
## %! assert(hello(3i),1+3i)
## %!test 
## %! unlink("dispatch_y.m");

FIXME add tests for preservation of mark_as_command status.

*/
