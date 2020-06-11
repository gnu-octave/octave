function [add, sub, mul, div] = shared_ctx (val)
  add = @add_fun;
  sub = @sub_fun;
  mul = @mul_fun;
  div = @div_fun;
  function r = add_fun (x)
    val += x;
    r = val;
  endfunction
  function r = sub_fun (x)
    val -= x;
    r = val;
  endfunction
  function r = mul_fun (x)
    val *= x;
    r = val;
  endfunction
  function r = div_fun (x)
    val /= x;
    r = val;
  endfunction
endfunction
