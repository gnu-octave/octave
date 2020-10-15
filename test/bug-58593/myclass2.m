classdef myclass2 < handle
  properties
    data_
  end

  methods
    function obj = myclass2 ()
      obj.data_ = 1001:1005;
    end

    function r = subsref (obj, S)
      switch (S(1).type)
      case '.'
        switch (S(1).subs)
        case 'data'
          %% Transform: obj.data --> obj.data_
          r = obj.data_;
          if (length (S) > 1)
            r = subsref (r, S(2:end));
          end
        case 'alldata'
          %% Transform: obj.data --> obj.data_(1:end)
          %% This statement should trigger *builtin* subsref *twice* (one
          %% for the evaluation of 'end', and the other for the whole rvalue).
          %% 'end' here is also builtin 'end'
          r = obj.data_(1:end);

          if (length (S) > 1)
            r = subsref (r, S(2:end));
          end
        otherwise
          error ('Incorrect usage');
        end
      case '()'
        %% Transform: obj(index) --> obj.data_(index)
        r = subsref (obj.data_, S);
      otherwise
        error ('Incorrect usage');
      end
    end

    function obj = subsasgn (obj, S, B)
      switch (S(1).type)
      case '.'
        switch (S(1).subs)
        case 'data'
          %% Transform: obj.data --> obj.data_
          if (length (S)>1)
            B = subsasgn (obj.data_, S(2:end), B);
          end
          obj.data_ = B;
        case 'alldata'
          %% Transform: obj.data --> obj.data_(1:end)
          if (length (S)>1)
            B = subsasgn (obj.data_(1:end), S(2:end), B);
          end
          %% This statement should trigger *builtin* subsref to evaluate 'end',
          %% then *builtin* subsasgn for the whole assignment expression
          %% 'end' here is also builtin 'end'
          obj.data_(1:end) = B;
        otherwise
          error ('Incorrect usage');
        end
      case '()'
        %% Transform: obj(index) --> obj.data_(index)
        obj.data_ = subsasgn (obj.data_, S, B);
      otherwise
        error ('Incorrect usage');
      end
    end

    function r = end (obj, k, n)
      %% We subtract 1 from the "real" end of obj.data_
      r = builtin ('end', obj.data_, k, n) - 1;
    end
  end
end
