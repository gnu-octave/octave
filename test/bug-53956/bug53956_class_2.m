classdef bug53956_class_2 < handle
  properties
  end
  methods
    function delete (self)
      global dtor2_called
      dtor2_called++;
    end
  end
end
