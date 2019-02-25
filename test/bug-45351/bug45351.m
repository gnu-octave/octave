classdef bug45351

  properties (Hidden, SetAccess = protected)
    h1 = [];% handle to function
    h2 = [];% handle to function
    h3 = [];% handle to function
  end

  methods
    function self = bug45351 ()
      self.h1 = @foo;
      self.h2 = @(self, n) self.foo (n);
      self.h3 = @(~, n) self.foo (n);
    end
    function [h1, h2, h3] = get_handles (self)
      h1 = self.h1;
      h2 = self.h2;
      h3 = self.h3;
    end
    function r = bar (self, hnum)
      switch (hnum)
        case 1
          r = self.h1 (self, hnum);
        case 2
          r = self.h2 (self, hnum);
        case 3
          r = self.h3 (self, hnum);
      end
    end
  end

  methods (Hidden, Access = protected)
    function r = foo (self, n)
      r = sprintf ('bug45351.foo: %d', n);
    end
  end
end
