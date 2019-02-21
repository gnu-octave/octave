classdef struct_wrapper
  properties
    s;
  end
  methods
    function o = struct_wrapper ()
      if (nargin == 0)
        o.s = struct ();
      else
        error ('struct_wrapper:SyntaxError', ...
               'struct_wrapper: Invalid syntax');
      end
    end
    function n = numel (o, varargin)
      n = 1;
    end
    function varargout = subsref (o, p)
      if (isequal (p(1).type, '{}'))
        r = [];
        for i = 1:numel (p(1).subs)
          r = [r, getfield(o.s, p(1).subs{i})];
        end
        varargout = {r};
      else
        error ('struct_wrapper:SyntaxError', ...
               'struct_wrapper: Invalid syntax');
      end
    end
    function o = subsasgn (o, p, varargin)
      if (isequal (p(1).type, '{}'))
        for i = 1:numel (p(1).subs)
          o.s = setfield (o.s, p(1).subs{i}, varargin{1}(i));
        end
      else
        error ('struct_wrapper:SyntaxError', ...
               'struct_wrapper: Invalid syntax');
      end
    end
  end
end
