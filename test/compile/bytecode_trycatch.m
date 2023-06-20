function bytecode_trycatch ()
  % TODO: Check identifier in error object too

  try
    __printf_assert__ ("yay ");
  catch
    __printf_assert__ ("boo ");
  end

  try
    __printf_assert__ ("yay2 ");
    error ("ooo");
    __printf_assert__ ("boo ");
  catch
    __printf_assert__ ("yay3 ");

    __printf_assert__ ("%s ", getfield (lasterror (), 'message'));
  end

  % Empty body/catch
  try
  catch
    __printf_assert__ ("boo ");
  end

  try
  catch
  end

  try
  catch err
  end

  try
    error("foo");
  catch
  end

  % Error object
  try
    __printf_assert__ ("yay2 ");
    error ("ooo2");
    __printf_assert__ ("boo ");
  catch err
    __printf_assert__ ("yay3 ");

    __printf_assert__ ("%s ", getfield (lasterror (), 'message'));
    __printf_assert__ ("%s ", getfield (err, 'message'));
  end

  % Nested
  try
    __printf_assert__ ("yay3 ");
    try
      __printf_assert__ ("yay4 ");
      error ("Nested error");
      __printf_assert__ ("boo ");
    catch
      __printf_assert__ ("%s ", getfield (lasterror (), 'message'));
      try
        __printf_assert__ ("yay5 ");
        error ("In catch");
        __printf_assert__ ("boo ");
      catch
        __printf_assert__ ("yay6 ");
        __printf_assert__ ("%s ", getfield (lasterror (), 'message'));
      end
    end

    error ("qwe");
    __printf_assert__ ("boo ");
  catch
    __printf_assert__ ("yay7 ");
    __printf_assert__ ("%s ", getfield (lasterror (), 'message'));
  end

  % Unwind subfunction
  try
    suby ();
  catch
    __printf_assert__ ("yay8 ");
    __printf_assert__ ("%s ", getfield (lasterror (), 'message'));
  end

  % Catch undefined id
  try
    qwe = asd;
    __printf_assert__ ("boo ");
  catch
    __printf_assert__ ("yay9 ");
    __printf_assert__ ("%s ", getfield (lasterror (), 'message'));
  end
  % Catch unconformant arguments
  try
    a = [1 2];
    b = [1 2 3];
    c = a * b;
  catch
    __printf_assert__ ("yay10 ");
    __printf_assert__ ("%s ", getfield (lasterror (), 'message'));
  end

  % Rethrow
  try
    try
      error ("yoyo");
    catch err
      rethrow (err);
      __printf_assert__ ("boo ");
    end
  catch
    __printf_assert__ ("yay11 ");
    __printf_assert__ ("%s ", getfield (lasterror (), 'message'));
  end

  % There are some shenanigans going on poping native ints belonging
  % to for-loops' iteration counters going on, so test that.
  for i = 1:3
    try
      error ("foo");
    catch
      __printf_assert__ ("yay12 ");
    __printf_assert__ ("%s ", getfield (lasterror (), 'message'));
    end
  end

  % switch statements save the value on the stack so add some switches
  % to check that they are unwound properly nested in fors etc

  zxc = '1';
  switch zxc
    case '1'
      for m = 1:3
      end
      switch m
        case 2
      end

      for i = 1:3
        try
          qwe = '1';
          switch qwe
            case '1'
              for j = 1:3
                asd = '1';
                switch asd
                  case '1'
                    error ("foo");
                end

                for k = 1:3
                end
              end
          end
        catch
          __printf_assert__ ("yay13 ");
          __printf_assert__ ("%s ", getfield (lasterror (), 'message'));
        end
      end

      for l = 1:2
      end
  end
  % TODO: Test more types of errors ...
end

function suby ()
  for j = 1:2
    for i = 1:3
      error ("Error in subfunction");
    end
  end
end
