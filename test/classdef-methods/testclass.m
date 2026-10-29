########################################################################
##
## Copyright (C) 2013-2026 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

classdef testclass

  properties (Constant)
    # functions
    n = 'nested_function';
    m = 'method_function';
    s = 'static_function';
    so = 'obj.static_function';
    l = 'local_function';
    p = 'private_function';
    # function handles
    hn = '@nested_function';
    hm = '@method_function';
    hmo = '@obj.method_function';
    hmvo = '@(varargin) obj.method_function (varargin{:})';
    hs = '@testclass.static_function';
    hsv = '@(varargin) testclass.static_function (varargin{:})';
    hso = '@obj.static_function';
    hsvo = '@(varargin) obj.static_function (varargin{:})';
    hl = '@local_function';
    hp = '@private_function';
  endproperties

  methods
    function ret = call (obj, func, varargin)
      switch (func)
        case testclass.n
          ret = nested_function (varargin{:});
        case testclass.m
          ret = obj.method_function (varargin{:});
        case testclass.s
          ret = testclass.static_function (varargin{:});
        case testclass.so
          ret = obj.static_function (varargin{:});
        case testclass.l
          ret = local_function (varargin{:});
        case testclass.p
          ret = private_function (varargin{:});
        case testclass.hn
          ret = @nested_function;
        case testclass.hm
          ret = @method_function;
        case testclass.hmo
          ret = @obj.method_function;
        case testclass.hmvo
          ret = @(varargin) obj.method_function (varargin{:});
        case testclass.hs
          ret = @testclass.static_function;
        case testclass.hsv
          ret = @(varargin) testclass.static_function (varargin{:});
        case testclass.hso
          ret = @obj.static_function;
        case testclass.hsvo
          ret = @(varargin) obj.static_function (varargin{:});
        case testclass.hl
          ret = @local_function;
        case testclass.hp
          ret = @private_function;
        otherwise
          ret = '';
      endswitch

      function ret = nested_function (varargin)
        ret = 'nested_function';
        if (nargin > 0)
          switch (varargin{1})
            case testclass.m
              ret = obj.method_function (varargin{2:end});
            case testclass.s
              ret = testclass.static_function (varargin{2:end});
            case testclass.so
              ret = obj.static_function (varargin{2:end});
            case testclass.l
              ret = local_function (varargin{2:end});
            case testclass.p
              ret = private_function (varargin{2:end});
            case testclass.hm
              ret = @method_function;
            case testclass.hmo
              ret = @obj.method_function;
            case testclass.hmvo
              ret = @(varargin) obj.method_function (varargin{:});
            case testclass.hs
              ret = @testclass.static_function;
            case testclass.hsv
              ret = @(varargin) testclass.static_function (varargin{:});
            case testclass.hso
              ret = @obj.static_function;
            case testclass.hsvo
              ret = @(varargin) obj.static_function (varargin{:});
            case testclass.hl
              ret = @local_function;
            case testclass.hp
              ret = @private_function;
          endswitch
        endif
      endfunction
    endfunction

    function ret = method_function (obj, varargin)
      ret = 'method_function';
      if (nargin > 1)
        switch (varargin{1})
          case testclass.s
            ret = testclass.static_function (varargin{2:end});
          case testclass.so
            ret = obj.static_function (varargin{2:end});
          case testclass.l
            ret = local_function (varargin{2:end});
          case testclass.p
            ret = private_function (varargin{2:end});
          case testclass.hs
            ret = @testclass.static_function;
          case testclass.hsv
            ret = @(varargin) testclass.static_function (varargin{:});
          case testclass.hso
            ret = @obj.static_function;
          case testclass.hsvo
            ret = @(varargin) obj.static_function (varargin{:});
          case testclass.hl
            ret = @local_function;
          case testclass.hp
            ret = @private_function;
        endswitch
      endif
    endfunction
  endmethods

  methods (Static)
    function ret = static_function (varargin)
      ret = 'static_function';
      if (nargin > 0)
        switch (varargin{1})
          case testclass.l
            ret = local_function (varargin{2:end});
          case testclass.p
            ret = private_function (varargin{2:end});
          case testclass.hl
            ret = @local_function;
          case testclass.hp
            ret = @private_function;
        endswitch
      endif
    endfunction
  endmethods

endclassdef

function ret = local_function (varargin)
  ret = 'local_function';
  if (nargin > 0)
    switch (varargin{1})
      case testclass.s
        ret = testclass.static_function (varargin{2:end});
      case testclass.p
        ret = private_function (varargin{2:end});
      case testclass.hs
        ret = @testclass.static_function;
      case testclass.hsv
        ret = @(varargin) testclass.static_function (varargin{:});
      case testclass.hp
        ret = @private_function;
    endswitch
  endif
endfunction
