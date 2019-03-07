function fcnHandle = counter
  value = 0;
  function currentValue = increment
    value = value+1;
    currentValue = value;
  end
  fcnHandle = @increment;
end
