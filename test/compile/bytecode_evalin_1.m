function bytecode_evalin_1 ()
  b = evalin ("caller", "caller_a");
  __printf_assert__ ("%f ", b);

  evalin ("caller", "__printf_assert__('yoyo ')");

  evalin ("base", "__printf_assert__('yobase ')");

  caller_b = 3;
  sub1();
  __printf_assert__ ("%f ", caller_e);
  __printf_assert__ ("%f ", eval ("caller_f")); % No slot for caller_f

  caller_c = 11;
  bytecode_evalin_2 ();
  __printf_assert__ ("%f ", caller_c); % Changes to 33

  %%% TODO: Can't create a variable with evalin in the treewalker
  %%%       need to verify it aint working with the VM too.
  %% __printf_assert__ ("%f ", caller_d); % Is initialized to 22


end

function sub1()
  b = evalin ("caller", "caller_b");
  __printf_assert__ ("%f ", b);

  evalin ("caller", "__printf_assert__('yoyo2 ')");

  evalin ("base", "__printf_assert__('yobase2 ')");

  assignin ("caller", "caller_e", 123);
  assignin ("caller", "caller_f", 124);
end
