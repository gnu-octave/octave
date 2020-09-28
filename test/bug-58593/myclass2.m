classdef myclass2 < handle
  properties
    data_
  end

  methods
    function obj = myclass2(data)
      obj.data_ = 1001:1005;
    end

    function r = subsref(obj,S)
      switch S(1).type
      case '.'
        switch S(1).subs
        case 'data'
          % Transform: obj.data --> obj.data_
          r = subsref (obj.data_, S(2:end));
        case 'alldata'
          % Transform: obj.data --> obj.data_(1:end)
          % This expression should trigger *builtin* subsref *twice* (one
          % for the evaluation of 'end', and the other for the whole rvalue).
          % 'end' here is also builtin 'end'
          r = obj.data_(1:end);
        otherwise
          error('Incorrect usage')
        end
      case '()'
        % Transform: obj(index) --> obj.data_(1:end)
        r = subsref(obj.data_,S);
      otherwise
        error('Incorrect usage')
      end
    end

    function r = end(obj,k,n)
      % We subtract 1 from the "real" end of obj.data_
      r = builtin('end',obj.data_,k,n)-1;
    end
  end
end
