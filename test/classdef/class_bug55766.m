classdef class_bug55766

  properties
    testprop = 0;
  end

  properties (Access = public)
    publictestprop = 0;
  end

  properties (Access = protected)
    protectedtestprop = 0;
  end

  properties (Hidden)
    hiddentestprop = 0;
  end

  properties (Hidden = true)
    anotherhiddentestprop = 0;
  end

  properties (Hidden = false)
    notahiddentestprop = 0;
  end

end
