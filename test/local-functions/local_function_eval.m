function status = local_function_eval ()
  eval ("y = 5;");
  status = local_fcn ("y = 6;");
endfunction

function status = local_fcn (expr)
  eval (expr);
  status = exist ("y", "var");
endfunction
