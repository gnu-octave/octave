try
  clear a
  a;
catch
  try
    clear b
    b;
  catch
    __error_text__
  end_try_catch
  __error_text__
end_try_catch
