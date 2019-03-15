classdef bug53956_class_4 < bug53956_class_3
  properties
  end
  methods
    function delete (self)
      global dtor4_called
      dtor4_called++;
    end
  end
end
