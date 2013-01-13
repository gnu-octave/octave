classdef foo_payment
  properties
    rate;
    term;
    principle;
  end
  methods
    function obj = foo_payment (r, t, p)
      if (nargin == 0)
        obj = struct ('rate', 0, 'term', 0, 'principle', 0);
      elseif (nargin == 3)
        obj.rate = r;
        obj.term = t;
        obj.principle = p;
      else
        error ('foo_payment:SyntaxError', ...
               'foo_payment: Invalid syntax')
      end
    end
    function amt = amount (obj)
      i = obj.rate / (12 * 100);
      if (i == 0 && obj.term == 0)
        amt = obj.principle;
      else
        amt = (obj.principle * i) / (1 - (1 + i)^(-obj.term));
      end
    end
  end
end

