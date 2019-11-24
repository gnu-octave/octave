function retval = fcn ()
  ## Call private function.
  assert (pfcn1 (), "pfcn1");
  ## Execute private script.
  a = "";
  ascript;
  assert (a, "ascript");
  retval = true;
endfunction
