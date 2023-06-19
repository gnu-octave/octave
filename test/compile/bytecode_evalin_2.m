function bytecode_evalin_2 ()
  __printf_assert__ ("%f ", evalin ("caller", "caller_c"));

  evalin ("caller", "caller_c = 33;");

  %% %Can't create local in caller in the treewalker
  %% evalin ("caller", "caller_d = 22;");
end
