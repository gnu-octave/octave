try
  clear a
  a;
catch
  try
    clear b
    b;
  catch
    lasterr
  end_try_catch
  lasterr
end_try_catch
