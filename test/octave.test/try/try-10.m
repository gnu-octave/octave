try
  try
    clear a
    a;
  catch
    error (strcat ("rethrow: ", __error_text__));
  end_try_catch
catch
  __error_text__
end_try_catch
