## Copyright (C) 2007 Michael Goffioul
## Copyright (C) 2010 Martin Hepperle
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function file} {} javaaddpath (@var{path})
## Add @var{path} to the dynamic class path of the Java virtual
## machine.  @var{path} may be either a directory where @file{.class}
## files are found, or a @file{.jar} file containing Java classes.
## @seealso{javaclasspath}
## @end deftypefn

function javaaddpath (class_path)

  if (nargin != 1)
    print_usage ();
  else
    new_path = canonicalize_file_name (tilde_expand (class_path));
    if (exist (new_path, "dir"))
      if (! strcmp (new_path (end), filesep))
        new_path = [new_path, filesep];
      endif
    elseif (! exist (new_path, "file"))
      error ("invalid Java classpath: %s", class_path);
    endif
    success = java_invoke ("org.octave.ClassHelper", "addClassPath", new_path);

    if (! success)
      warning ("javaaddpath: failed to add '%s' to Java classpath", new_path);
    endif
  endif 
   
endfunction
