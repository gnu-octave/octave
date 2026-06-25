classdef classdef_breakpoints
  properties
    m_prop = [];
  endproperties
  methods
    function this = classdef_breakpoints (prop)
      this.m_prop = prop;
    endfunction
    function this = foo (this, prop)
      this.m_prop = prop;
    endfunction
    function this = bar (this)
      printf ("prop: %f\n", this.m_prop);
    endfunction
  endmethods
endclassdef

function my_subfunction (n)
  ## my_subfunction hello
  printf ("subfunction: %f\n", n);
endfunction
