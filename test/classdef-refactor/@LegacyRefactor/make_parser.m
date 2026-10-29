function obj = make_parser (self)
  obj = inputParser ();
  obj.addRequired ('value');
  obj.parse (42);
endfunction
