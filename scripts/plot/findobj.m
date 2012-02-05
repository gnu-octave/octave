## Copyright (C) 2007-2012 Ben Abbott
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
## @deftypefn  {Function File} {@var{h} =} findobj ()
## @deftypefnx {Function File} {@var{h} =} findobj (@var{prop_name}, @var{prop_value})
## @deftypefnx {Function File} {@var{h} =} findobj ("-property", @var{prop_name})
## @deftypefnx {Function File} {@var{h} =} findobj ("-regexp", @var{prop_name}, @var{pattern})
## @deftypefnx {Function File} {@var{h} =} findobj ("flat", @dots{})
## @deftypefnx {Function File} {@var{h} =} findobj (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} =} findobj (@var{h}, "-depth", @var{d}, @dots{})
## Find graphics object with specified property values.  The simplest form is
##
## @example
## findobj (@var{prop_name}, @var{prop_value})
## @end example
##
## @noindent
## which returns all of the handles to the objects with the name
## @var{prop_name} and the name @var{prop_value}.  The search can be limited
## to a particular object or set of objects and their descendants by
## passing a handle or set of handles @var{h} as the first argument to
## @code{findobj}.
##
## The depth of hierarchy of objects to which to search to can be limited
## with the "-depth" argument.  To limit the number depth of the hierarchy
## to search to @var{d} generations of children, and example is
##
## @example
## findobj (@var{h}, "-depth", @var{d}, @var{prop_name}, @var{prop_value})
## @end example
##
## Specifying a depth @var{d} of 0, limits the search to the set of object
## passed in @var{h}.  A depth @var{d} of 0 is equivalent to the "-flat"
## argument.
##
## A specified logical operator may be applied to the pairs of @var{prop_name}
## and @var{prop_value}.  The supported logical operators are "-and", "-or",
## "-xor", "-not".
##
## The objects may also be matched by comparing a regular expression to the
## property values, where property values that match @code{regexp
## (@var{prop_value}, @var{pattern})} are returned.  Finally, objects may be
## matched by property name only, using the "-property" option.
## @seealso{get, set}
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>

function h = findobj (varargin)

  depth = NaN;
  if (nargin == 0)
    handles = 0;
    n1 = 0;
  else
    if (! isempty (varargin{1}))
      if (ishandle (varargin{1}(1)))
        handles = varargin{1};
        n1 = 2;
      else
        handles = 0;
        n1 = 1;
      endif
    else
      ## Return [](0x1) for compatibility.
      h = zeros (0, 1);
      return;
    endif
    if (n1 <= nargin)
      if (ischar (varargin{n1}))
        if (strcmpi (varargin{n1}, "flat"))
          depth = 0;
          n1 = n1 + 1;
        elseif (strcmpi (varargin{n1}, "-depth"))
          depth = varargin{n1+1};
          n1 = n1 + 2;
        endif
      else
        error ("findobj: properties and options must be strings");
      endif
    endif
  endif

  if (n1 <= nargin && nargin > 0)
    args = varargin(n1 : nargin);
  else
    args = {};
  endif

  regularexpression = [];
  property          = [];
  logicaloperator   = {};
  pname             = {};
  pvalue            = {};
  np = 1;
  na = 1;

  while (na <= numel (args))
    regularexpression(np) = 0;
    property(np) = 0;
    logicaloperator{np} = "and";
    if (ischar (args{na}))
      if (strcmpi (args{na}, "-regexp"))
        if (na + 2 <= numel (args))
          regularexpression(np) = 1;
          na = na + 1;
          pname{np} = args{na};
          na = na + 1;
          pvalue{np} = args{na};
          na = na + 1;
          np = np + 1;
        else
          error ("findobj: inconsistent number of arguments");
        endif
      elseif (strcmpi (args{na}, "-property"))
        if (na + 1 <= numel (args))
          na = na + 1;
          property(np) = 1;
          pname{np} = args{na};
          na = na + 1;
          pvalue{np} = [];
          np = np + 1;
        else
          error ("findobj: inconsistent number of arguments");
        endif
      elseif (! strcmp (args{na}(1), "-"))
        ## Parameter/value pairs.
        if (na + 1 <= numel (args))
          pname{np} = args{na};
          na = na + 1;
          pvalue{np} = args{na};
          na = na + 1;
          if (na <= numel(args))
            if (ischar (args{na}))
              if strcmpi(args{na}, "-and")
                logicaloperator{np} = "and";
                na = na+1;
              elseif strcmpi(args{na}, "-or")
                logicaloperator{np} = "or";
                na = na+1;
              elseif strcmpi(args{na}, "-xor")
                logicaloperator{np} = "xor";
                na = na+1;
              elseif strcmpi(args{na}, "-not")
                logicaloperator{np} = "not";
                na = na+1;
              endif
            else
              error ("findobj: properties and options must be strings");
            endif
          else
            logicaloperator{np} = "and";
          endif
          np = np + 1;
        else
          error ("findobj: inconsistent number of arguments");
        endif
      else
        ## This is sloppy ... but works like Matlab.
        if strcmpi(args{na}, "-not")
          h = [];
          return
        endif
        na = na + 1;
      endif
    else
      error ("findobj: properties and options must be strings");
    endif
  endwhile

  numpairs = np - 1;

  ## Load all objects which qualify for being searched.
  idepth = 0;
  h = handles;
  while (numel (handles) && ! (idepth >= depth))
    children = [];
    for n = 1 : numel (handles)
      children = union (children, get(handles(n), "children"));
    endfor
    handles = children;
    h = union (h, children);
    idepth = idepth + 1;
  endwhile

  keepers = ones (size (h));
  if (numpairs > 0)
    for nh = 1 : numel(h)
      p = get (h (nh));
      for np = 1 : numpairs
        fields = fieldnames (p);
        fieldindex = find (strcmpi (fields, pname{np}), 1);
        if (numel (fieldindex))
          pname{np} = fields{fieldindex};
          if (property(np))
            match = 1;
          else
            if (regularexpression(np))
              match = regexp (p.(pname{np}), pvalue{np});
              if isempty (match)
                match = 0;
              endif
            elseif (numel (p.(pname{np})) == numel (pvalue{np}))
              if (ischar (pvalue{np}))
                match = strcmpi (pvalue{np}, p.(pname{np}));
              else
                match = (pvalue{np} == p.(pname{np}));
              endif
            else
              match = 0;
            endif
            match = all (match);
          endif
          if (strcmpi (logicaloperator{np}, "not"))
            keepers(nh) = ! keepers(nh) & ! match;
          else
            keepers(nh) = feval (logicaloperator{np}, keepers(nh), match);
          endif
        else
          keepers(nh) = 0;
        endif
      endfor
    endfor
  endif

  h = h (keepers != 0);
  h = reshape (h, [numel(h), 1]);
endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   l = line;
%!   obj = findobj (hf, "type", "line");
%!   assert (l, obj);
%!   assert (gca, findobj (hf, "type", "axes"));
%!   assert (hf, findobj (hf, "type", "figure"));
%!   assert (isempty (findobj (hf, "type", "xyzxyz")));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

