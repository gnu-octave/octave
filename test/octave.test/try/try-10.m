try
  try
    clear a
    a;
  catch
    error (strcat ("rethrow: ", lasterr));
  end_try_catch
catch
  lasterr
end_try_catch
