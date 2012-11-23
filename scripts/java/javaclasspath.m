## Copyright (C) 2007 Michael Goffioul, 2010 Martin Hepperle
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; If not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function file}  {}          javaclasspath
## @deftypefnx {Function file} {@var{STATIC} =} javaclasspath
## @deftypefnx {Function file} {[@var{STATIC}, @var{DYNAMIC}] =} javaclasspath
## @deftypefnx {Function file} {@var{PATH} =} javaclasspath (@var{WHAT})
##
## Returns the class path of the Java virtual machine in
## the form of a cell array of strings. 
##
## If called without input parameter:@*
## @itemize
## @item If no output variable is given, the result is simply printed 
## on the standard output.
## @item If one output variable @var{STATIC} is given, the result is
## the static classpath.
## @item If two output variables @var{STATIC} and @var{DYNAMIC} are 
## given, the first variable will contain the static classpath,
## the second will be filled with the dynamic claspath.
## @end itemize
## 
## If called with a single input parameter @var{WHAT}:@*
## @itemize
## @item If @var{WHAT} is '-static' the static classpath is returned.
## @item If @var{WHAT} is '-dynamic' the dynamic  classpath is returned.
## @item If @var{WHAT} is '-all' the static and the dynamic classpath 
## are returned in a single cell array
## @end itemize
## @end deftypefn
## @seealso{javaaddpath,javarmpath}

function varargout = javaclasspath (which)

  % dynamic classpath
  dynamic_path = java_invoke ("org.octave.ClassHelper", "getClassPath");
  dynamic_path_list = strsplit (dynamic_path, pathsep ());

  % static classpath
  static_path = java_invoke ('java.lang.System', 'getProperty', 'java.class.path');
  static_path_list = strsplit (static_path, pathsep ());
  if (length (static_path_list) > 1)
    % remove first element (which is .../octave.jar)
    static_path_list = static_path_list(2:length (static_path_list));
  else
    static_path_list = {};
  end

  switch nargin
    case 0
      switch nargout
        case 0
          disp_path_list ( 'STATIC', static_path_list )
          disp('');
          disp_path_list ( 'DYNAMIC', dynamic_path_list )
        case 1
          varargout{1} = cellstr (dynamic_path_list);
        case 2
          varargout{1} = cellstr (dynamic_path_list);
          varargout{2} = cellstr (static_path_list);
      endswitch
        
    case 1
      switch nargout
        case 0
          if (strcmp (which, '-static') == 1)
            disp_path_list ( 'STATIC', static_path_list )
          elseif (strcmp (which, '-dynamic') == 1)
            disp_path_list ( 'DYNAMIC', dynamic_path_list )
          end
        case 1
          if (strcmp (which, '-static') == 1)
            varargout{1} = cellstr (static_path_list);
          elseif (strcmp (which, '-dynamic') == 1)
            varargout{1} = cellstr (dynamic_path_list);
          elseif (strcmp (which, '-all') == 1)
            varargout{1} = cellstr ([static_path_list, dynamic_path_list]);
          end
      endswitch
  endswitch
  
end
#
# Display cell array of paths
#
function disp_path_list ( which, path_list )
  printf ('   %s JAVA PATH\n\n', which);
  if (length (path_list) > 0)
    printf ('      %s\n', path_list{:});
  else
    printf ('      - empty -\n');
  endif
end
