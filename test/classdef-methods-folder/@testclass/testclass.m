classdef testclass

  properties (Constant)
    # functions
    n = 'nested_function';
    m = 'method_function';
    m2 = 'method_external_function';
    s = 'static_function';
    so = 'obj.static_function';
    s2 = 'static_external_function';
    s2o = 'obj.static_external_function';
    l = 'local_function';
    p = 'private_function';
    # function handles
    hn = '@nested_function';
    hm = '@method_function';
    hmo = '@obj.method_function';
    hmvo = '@(varargin) obj.method_function (varargin{:})';
    hm2 = '@method_external_function';
    hm2o = '@obj.method_external_function';
    hm2vo = '@(varargin) obj.method_external_function (varargin{:})';
    hs = '@testclass.static_function';
    hsv = '@(varargin) testclass.static_function (varargin{:})';
    hso = '@obj.static_function';
    hsvo = '@(varargin) obj.static_function (varargin{:})';
    hs2 = '@testclass.static_external_function';
    hs2v = '@(varargin) testclass.static_external_function (varargin{:})';
    hs2o = '@obj.static_external_function';
    hs2vo = '@(varargin) obj.static_external_function (varargin{:})';
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
        case testclass.m2
          ret = obj.method_external_function (varargin{:});
        case testclass.s
          ret = testclass.static_function (varargin{:});
        case testclass.so
          ret = obj.static_function (varargin{:});
        case testclass.s2
          ret = testclass.static_external_function (varargin{:});
        case testclass.s2o
          ret = obj.static_external_function (varargin{:});
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
        case testclass.hm2
          ret = @method_external_function;
        case testclass.hm2o
          ret = @obj.method_external_function;
        case testclass.hm2vo
          ret = @(varargin) obj.method_external_function (varargin{:});
        case testclass.hs
          ret = @testclass.static_function;
        case testclass.hsv
          ret = @(varargin) testclass.static_function (varargin{:});
        case testclass.hso
          ret = @obj.static_function;
        case testclass.hsvo
          ret = @(varargin) obj.static_function (varargin{:});
        case testclass.hs2
          ret = @testclass.static_external_function;
        case testclass.hs2v
          ret = @(varargin) testclass.static_external_function (varargin{:});
        case testclass.hs2o
          ret = @obj.static_external_function;
        case testclass.hs2vo
          ret = @(varargin) obj.static_external_function (varargin{:});
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
            case testclass.m2
              ret = obj.method_external_function (varargin{2:end});
            case testclass.s
              ret = testclass.static_function (varargin{2:end});
            case testclass.so
              ret = obj.static_function (varargin{2:end});
            case testclass.s2
              ret = testclass.static_external_function (varargin{2:end});
            case testclass.s2o
              ret = obj.static_external_function (varargin{2:end});
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
            case testclass.hm2
              ret = @method_external_function;
            case testclass.hm2o
              ret = @obj.method_external_function;
            case testclass.hm2vo
              ret = @(varargin) obj.method_external_function (varargin{:});
            case testclass.hs
              ret = @testclass.static_function;
            case testclass.hsv
              ret = @(varargin) testclass.static_function (varargin{:});
            case testclass.hso
              ret = @obj.static_function;
            case testclass.hsvo
              ret = @(varargin) obj.static_function (varargin{:});
            case testclass.hs2
              ret = @testclass.static_external_function;
            case testclass.hs2v
              ret = @(varargin) testclass.static_external_function (varargin{:});
            case testclass.hs2o
              ret = @obj.static_external_function;
            case testclass.hs2vo
              ret = @(varargin) obj.static_external_function (varargin{:});
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
          case testclass.m2
            ret = obj.method_external_function (varargin{2:end});
          case testclass.s
            ret = testclass.static_function (varargin{2:end});
          case testclass.so
            ret = obj.static_function (varargin{2:end});
          case testclass.s2
            ret = testclass.static_external_function (varargin{2:end});
          case testclass.s2o
            ret = obj.static_external_function (varargin{2:end});
          case testclass.l
            ret = local_function (varargin{2:end});
          case testclass.p
            ret = private_function (varargin{2:end});
          case testclass.hm2
            ret = @method_external_function;
          case testclass.hm2o
            ret = @obj.method_external_function;
          case testclass.hm2vo
            ret = @(varargin) obj.method_external_function (varargin{:});
          case testclass.hs
            ret = @testclass.static_function;
          case testclass.hsv
            ret = @(varargin) testclass.static_function (varargin{:});
          case testclass.hso
            ret = @obj.static_function;
          case testclass.hsvo
            ret = @(varargin) obj.static_function (varargin{:});
          case testclass.hs2
            ret = @testclass.static_external_function;
          case testclass.hs2v
            ret = @(varargin) testclass.static_external_function (varargin{:});
          case testclass.hs2o
            ret = @obj.static_external_function;
          case testclass.hs2vo
            ret = @(varargin) obj.static_external_function (varargin{:});
          case testclass.hl
            ret = @local_function;
          case testclass.hp
            ret = @private_function;
        endswitch
      endif
    endfunction

    ret = method_external_function (obj, varargin)
  endmethods

  methods (Static)
    function ret = static_function (varargin)
      ret = 'static_function';
      if (nargin > 0)
        switch (varargin{1})
          case testclass.s2
            ret = testclass.static_external_function (varargin{2:end});
          case testclass.l
            ret = local_function (varargin{2:end});
          case testclass.p
            ret = private_function (varargin{2:end});
          case testclass.hs2
            ret = @testclass.static_external_function;
          case testclass.hs2v
            ret = @(varargin) testclass.static_external_function (varargin{:});
          case testclass.hl
            ret = @local_function;
          case testclass.hp
            ret = @private_function;
        endswitch
      endif
    endfunction

    ret = static_external_function (varargin)
  endmethods

endclassdef

function ret = local_function (varargin)
  ret = 'local_function';
  if (nargin > 0)
    switch (varargin{1})
      case testclass.s
        ret = testclass.static_function (varargin{2:end});
      case testclass.s2
        ret = testclass.static_external_function (varargin{2:end});
      case testclass.p
        ret = private_function (varargin{2:end});
      case testclass.hs
        ret = @testclass.static_function;
      case testclass.hsv
        ret = @(varargin) testclass.static_function (varargin{:});
      case testclass.hs2
        ret = @testclass.static_external_function;
      case testclass.hs2v
        ret = @(varargin) testclass.static_external_function (varargin{:});
      case testclass.hp
        ret = @private_function;
    endswitch
  endif
endfunction
