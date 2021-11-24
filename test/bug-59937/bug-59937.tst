%!test <*59973>
%! eval_global_script
%! assert (isglobal (new_var_name));
%! assert (__varval__ (new_var_name), []);
%! eval (sprintf ("clear -global %s", new_var_name));
