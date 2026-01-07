classdef overloaded_subsref

  properties
    a
  end

  methods
    function B = subsref (A, s)
      if (strcmp (s(1).type, '()') && numel (s(1).subs) == 1 && s(1).subs{1} == 1)
        error ('unsupported syntax');
      else
        B = builtin ('subsref', A, s);
      end
    end
  end

end
