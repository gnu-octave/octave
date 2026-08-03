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
    hs = '@testpkg.testclass.static_function';
    hsv = '@(varargin) testpkg.testclass.static_function (varargin{:})';
    hso = '@obj.static_function';
    hsvo = '@(varargin) obj.static_function (varargin{:})';
    hl = '@local_function';
    hp = '@private_function';
  endproperties

  methods

    function ret = call (obj, func, varargin)
      switch (func)
        case testpkg.testclass.n
          ret = nested_function (varargin{:});
        case testpkg.testclass.m
          ret = obj.method_function (varargin{:});
        case testpkg.testclass.s
          ret = testpkg.testclass.static_function (varargin{:});
        case testpkg.testclass.so
          ret = obj.static_function (varargin{:});
        case testpkg.testclass.l
          ret = local_function (varargin{:});
        case testpkg.testclass.p
          ret = private_function (varargin{:});
        case testpkg.testclass.hn
          ret = @nested_function;
        case testpkg.testclass.hm
          ret = @method_function;
        case testpkg.testclass.hmo
          ret = @obj.method_function;
        case testpkg.testclass.hmvo
          ret = @(varargin) obj.method_function (varargin{:});
        case testpkg.testclass.hs
          ret = @testpkg.testclass.static_function;
        case testpkg.testclass.hsv
          ret = @(varargin) testpkg.testclass.static_function (varargin{:});
        case testpkg.testclass.hso
          ret = @obj.static_function;
        case testpkg.testclass.hsvo
          ret = @(varargin) obj.static_function (varargin{:});
        case testpkg.testclass.hl
          ret = @local_function;
        case testpkg.testclass.hp
          ret = @private_function;
        otherwise
          ret = '';
      endswitch

      function ret = nested_function (varargin)
        ret = 'nested_function';
        if (nargin > 0)
          switch (varargin{1})
            case testpkg.testclass.m
              ret = obj.method_function (varargin{2:end});
            case testpkg.testclass.s
              ret = testpkg.testclass.static_function (varargin{2:end});
            case testpkg.testclass.so
              ret = obj.static_function (varargin{2:end});
            case testpkg.testclass.l
              ret = local_function (varargin{2:end});
            case testpkg.testclass.p
              ret = private_function (varargin{2:end});
            case testpkg.testclass.hm
              ret = @method_function;
            case testpkg.testclass.hmo
              ret = @obj.method_function;
            case testpkg.testclass.hmvo
              ret = @(varargin) obj.method_function (varargin{:});
            case testpkg.testclass.hs
              ret = @testpkg.testclass.static_function;
            case testpkg.testclass.hsv
              ret = @(varargin) testpkg.testclass.static_function (varargin{:});
            case testpkg.testclass.hso
              ret = @obj.static_function;
            case testpkg.testclass.hsvo
              ret = @(varargin) obj.static_function (varargin{:});
            case testpkg.testclass.hl
              ret = @local_function;
            case testpkg.testclass.hp
              ret = @private_function;
          endswitch
        endif
      endfunction
    endfunction

    function ret = method_function (obj, varargin)
      ret = 'method_function';
      if (nargin > 1)
        switch (varargin{1})
          case testpkg.testclass.s
            ret = testpkg.testclass.static_function (varargin{2:end});
          case testpkg.testclass.so
            ret = obj.static_function (varargin{2:end});
          case testpkg.testclass.l
            ret = local_function (varargin{2:end});
          case testpkg.testclass.p
            ret = private_function (varargin{2:end});
          case testpkg.testclass.hs
            ret = @testpkg.testclass.static_function;
          case testpkg.testclass.hsv
            ret = @(varargin) testpkg.testclass.static_function (varargin{:});
          case testpkg.testclass.hso
            ret = @obj.static_function;
          case testpkg.testclass.hsvo
            ret = @(varargin) obj.static_function (varargin{:});
          case testpkg.testclass.hl
            ret = @local_function;
          case testpkg.testclass.hp
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
          case testpkg.testclass.l
            ret = local_function (varargin{2:end});
          case testpkg.testclass.p
            ret = private_function (varargin{2:end});
          case testpkg.testclass.hl
            ret = @local_function;
          case testpkg.testclass.hp
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
      case testpkg.testclass.s
        ret = testpkg.testclass.static_function (varargin{2:end});
      case testpkg.testclass.p
        ret = private_function (varargin{2:end});
      case testpkg.testclass.hs
        ret = @testpkg.testclass.static_function;
      case testpkg.testclass.hsv
        ret = @(varargin) testpkg.testclass.static_function (varargin{:});
      case testpkg.testclass.hp
        ret = @private_function;
    endswitch
  endif
endfunction
