classdef set_method_error_class
  properties
    b = 0
    c = 0
  endproperties
  ## LOADING keeps its default value on load, which does not call the
  ## constructor and does not restore Transient properties.
  properties (Transient)
    loading = true
  endproperties
  methods
    function obj = set_method_error_class ()
      obj.loading = false;
    endfunction
    ## An error on load only, so that a test can save a value and see how
    ## load handles the error.
    function obj = set.b (obj, val)
      if (obj.loading)
        error ("set_method_error_class: no value for b on load");
      endif
      obj.b = val;
    endfunction
  endmethods
endclassdef
