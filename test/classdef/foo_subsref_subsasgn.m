classdef foo_subsref_subsasgn < handle
  properties
    x
  end

  methods
    function obj = foo_subsref_subsasgn (dims)
      switch (dims)
        case 1
          obj.x = 1:4;
        case 2
          obj.x = eye (4);
        otherwise
          error ('foo_subsref_subsasgn:SyntaxError', ...
            'foo_subsref_subsasgn: Invalid syntax');
      end
    end

    function ind = end (obj, k, n)
      sz = size (obj.x);
      if (k < n)
        ind = sz(k);
      else
        ind = prod (sz(k:end));
      end
    end

    function varargout = subsref (obj, S)
      switch (length (S))
        case 1
          if (S(1).type == "()")
            varargout = {obj.x(S(1).subs{1})};
          elseif (S(1).type == "{}")
            %% Note in ML R2018b "x{1:3}" expects "nargout == 3".
            varargout = num2cell (obj.x(S(1).subs{1}));
          elseif (S(1).type == "." && S(1).subs == 'x')
            varargout = {obj.x};
          else
            error ('foo_subsref_subsasgn:SyntaxError', ...
              'foo_subsref_subsasgn: Invalid syntax');
          end
        case 2
          %% Note in ML R2018b "x(1)(1)" is not allowed.
          if (S(1).type == "{}" && (S(2).type == "{}" || S(2).type == "()"))
            varargout = {obj.x(S(1).subs{1}, S(2).subs{1})};
          elseif (S(1).type == "." && S(1).subs == 'x' ...
              && (S(2).type == "{}" || S(2).type == "()"))
            varargout = {obj.x(S(2).subs{1})};
          else
            error ('foo_subsref_subsasgn:SyntaxError', ...
              'foo_subsref_subsasgn: Invalid syntax');
          end
        otherwise
          error ('foo_subsref_subsasgn:SyntaxError', ...
            'foo_subsref_subsasgn: Invalid syntax');
      end
    end

    function obj = subsasgn (obj, S, varargin)
      switch (length (S))
        case 1
          if (S(1).type == "{}" || S(1).type == "()")
            obj.x(S(1).subs{1}) = varargin{1};
          elseif (S(1).type == "." && S(1).subs == 'x')
            obj.x = varargin{1};
          else
            error ('foo_subsref_subsasgn:SyntaxError', ...
              'foo_subsref_subsasgn: Invalid syntax');
          end
        case 2
          %% Note in ML R2018b "x(1)(1)" is not allowed.
          if (S(1).type == "{}" && (S(2).type == "{}" || S(2).type == "()"))
            obj.x(S(1).subs{1}, S(2).subs{1}) = varargin{1};
          elseif (S(1).type == "." && S(1).subs == 'x' ...
              && (S(2).type == "{}" || S(2).type == "()"))
            obj.x(S(2).subs{1}) = varargin{1};
          else
            error ('foo_subsref_subsasgn:SyntaxError', ...
              'foo_subsref_subsasgn: Invalid syntax');
          end
        otherwise
          error ('foo_subsref_subsasgn:SyntaxError', ...
            'foo_subsref_subsasgn: Invalid syntax');
      end
    end

  end
end
