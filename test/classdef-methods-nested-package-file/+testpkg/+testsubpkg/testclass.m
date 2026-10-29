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
    hs = '@testpkg.testsubpkg.testclass.static_function';
    hsv = '@(varargin) testpkg.testsubpkg.testclass.static_function (varargin{:})';
    hso = '@obj.static_function';
    hsvo = '@(varargin) obj.static_function (varargin{:})';
    hl = '@local_function';
    hp = '@private_function';
  endproperties

  methods

    function ret = call (obj, func, varargin)
      switch (func)
        case testpkg.testsubpkg.testclass.n
          ret = nested_function (varargin{:});
        case testpkg.testsubpkg.testclass.m
          ret = obj.method_function (varargin{:});
        case testpkg.testsubpkg.testclass.s
          ret = testpkg.testsubpkg.testclass.static_function (varargin{:});
        case testpkg.testsubpkg.testclass.so
          ret = obj.static_function (varargin{:});
        case testpkg.testsubpkg.testclass.l
          ret = local_function (varargin{:});
        case testpkg.testsubpkg.testclass.p
          ret = private_function (varargin{:});
        case testpkg.testsubpkg.testclass.hn
          ret = @nested_function;
        case testpkg.testsubpkg.testclass.hm
          ret = @method_function;
        case testpkg.testsubpkg.testclass.hmo
          ret = @obj.method_function;
        case testpkg.testsubpkg.testclass.hmvo
          ret = @(varargin) obj.method_function (varargin{:});
        case testpkg.testsubpkg.testclass.hs
          ret = @testpkg.testsubpkg.testclass.static_function;
        case testpkg.testsubpkg.testclass.hsv
          ret = @(varargin) testpkg.testsubpkg.testclass.static_function (varargin{:});
        case testpkg.testsubpkg.testclass.hso
          ret = @obj.static_function;
        case testpkg.testsubpkg.testclass.hsvo
          ret = @(varargin) obj.static_function (varargin{:});
        case testpkg.testsubpkg.testclass.hl
          ret = @local_function;
        case testpkg.testsubpkg.testclass.hp
          ret = @private_function;
        otherwise
          ret = '';
      endswitch

      function ret = nested_function (varargin)
        ret = 'nested_function';
        if (nargin > 0)
          switch (varargin{1})
            case testpkg.testsubpkg.testclass.m
              ret = obj.method_function (varargin{2:end});
            case testpkg.testsubpkg.testclass.s
              ret = testpkg.testsubpkg.testclass.static_function (varargin{2:end});
            case testpkg.testsubpkg.testclass.so
              ret = obj.static_function (varargin{2:end});
            case testpkg.testsubpkg.testclass.l
              ret = local_function (varargin{2:end});
            case testpkg.testsubpkg.testclass.p
              ret = private_function (varargin{2:end});
            case testpkg.testsubpkg.testclass.hm
              ret = @method_function;
            case testpkg.testsubpkg.testclass.hmo
              ret = @obj.method_function;
            case testpkg.testsubpkg.testclass.hmvo
              ret = @(varargin) obj.method_function (varargin{:});
            case testpkg.testsubpkg.testclass.hs
              ret = @testpkg.testsubpkg.testclass.static_function;
            case testpkg.testsubpkg.testclass.hsv
              ret = @(varargin) testpkg.testsubpkg.testclass.static_function (varargin{:});
            case testpkg.testsubpkg.testclass.hso
              ret = @obj.static_function;
            case testpkg.testsubpkg.testclass.hsvo
              ret = @(varargin) obj.static_function (varargin{:});
            case testpkg.testsubpkg.testclass.hl
              ret = @local_function;
            case testpkg.testsubpkg.testclass.hp
              ret = @private_function;
          endswitch
        endif
      endfunction
    endfunction

    function ret = method_function (obj, varargin)
      ret = 'method_function';
      if (nargin > 1)
        switch (varargin{1})
          case testpkg.testsubpkg.testclass.s
            ret = testpkg.testsubpkg.testclass.static_function (varargin{2:end});
          case testpkg.testsubpkg.testclass.so
            ret = obj.static_function (varargin{2:end});
          case testpkg.testsubpkg.testclass.l
            ret = local_function (varargin{2:end});
          case testpkg.testsubpkg.testclass.p
            ret = private_function (varargin{2:end});
          case testpkg.testsubpkg.testclass.hs
            ret = @testpkg.testsubpkg.testclass.static_function;
          case testpkg.testsubpkg.testclass.hsv
            ret = @(varargin) testpkg.testsubpkg.testclass.static_function (varargin{:});
          case testpkg.testsubpkg.testclass.hso
            ret = @obj.static_function;
          case testpkg.testsubpkg.testclass.hsvo
            ret = @(varargin) obj.static_function (varargin{:});
          case testpkg.testsubpkg.testclass.hl
            ret = @local_function;
          case testpkg.testsubpkg.testclass.hp
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
          case testpkg.testsubpkg.testclass.l
            ret = local_function (varargin{2:end});
          case testpkg.testsubpkg.testclass.p
            ret = private_function (varargin{2:end});
          case testpkg.testsubpkg.testclass.hl
            ret = @local_function;
          case testpkg.testsubpkg.testclass.hp
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
      case testpkg.testsubpkg.testclass.s
        ret = testpkg.testsubpkg.testclass.static_function (varargin{2:end});
      case testpkg.testsubpkg.testclass.p
        ret = private_function (varargin{2:end});
      case testpkg.testsubpkg.testclass.hs
        ret = @testpkg.testsubpkg.testclass.static_function;
      case testpkg.testsubpkg.testclass.hsv
        ret = @(varargin) testpkg.testsubpkg.testclass.static_function (varargin{:});
      case testpkg.testsubpkg.testclass.hp
        ret = @private_function;
    endswitch
  endif
endfunction
