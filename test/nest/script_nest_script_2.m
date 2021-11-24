## script_nest_script.m
if (x > 0)
  r = x * 2;
else
  ## Expect error since nested function should not be
  ## visible in this context.
  r = nest_fun ();
endif
