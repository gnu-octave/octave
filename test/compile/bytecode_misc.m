function bytecode_misc ()
  ## Assure "set_internal_variable" are reset properly
  max_stack = max_stack_depth;
  set_max_stack_depth_1p (max_stack + 1);
  assert (max_stack_depth == max_stack);
end

function set_max_stack_depth_1p (x)
  max_stack_depth (x + 1, "local");
  assert (max_stack_depth == x + 1);
end