classdef cdef_counts_self < handle
  methods
    function obj = cdef_counts_self ()
      global cdef_alive_objects_cntr = 0;
      cdef_alive_objects_cntr++;
    end
    function delete (self)
      global cdef_alive_objects_cntr = 0;
      cdef_alive_objects_cntr--;
    end
  end
end
