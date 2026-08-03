classdef atfolder_class
  properties
    a
  endproperties

  methods
    retval = external_method (obj);
  endmethods

  methods (Static = true)
    retval = external_static_method ();
  endmethods

  methods (Hidden = true, Access = private)
    retval = external_method_2 (obj);
  endmethods
endclassdef
