## persistent_nest
function y = persistent_nest ()
  persistent x = 0;
  g;
  y = x;

  function g
    x = x + 1;
  end
end
