function bytecode_unwind ()
  unwind_protect
    __printf_assert__ ("yay1 ");
  unwind_protect_cleanup
    __printf_assert__ ("yay2 ");
  end

  try
    unwind_protect
      error ("e1");
      __printf_assert__ ("boo ");
    unwind_protect_cleanup
      __printf_assert__ ("yay3 ");
    end
  catch e
    __printf_assert__ ("%s ", getfield (e, 'message'));
  end

  suby (1);
  suby (2);

  try
    suby (3);
  catch
  end
  
  suby (4);
  suby (5);
  suby (6);

  % Break and returns that are not executed
  %
  % The combination "break and return", "break" and "return"
  % takes different codepaths in the walker so we test all 
  % combinations here.

  unwind_protect
    for i = 1:3
      if i == 4
        break
      elseif i == 5
        return
      end
    end
  unwind_protect_cleanup
    __printf_assert__ ("yay4 ");
  end

  % Break only
  unwind_protect
    for i = 1:3
      if i == 4
        break
      end
    end
  unwind_protect_cleanup
    __printf_assert__ ("yay5 ");
  end

  % Return only
  unwind_protect
    for i = 1:3
      if i == 4
        return
      end
    end
  unwind_protect_cleanup
    __printf_assert__ ("yay6 ");
  end
end

function suby (a)
  if a == 1
    unwind_protect
      __printf_assert__ ("subyyay1 ");
      return
      __printf_assert__ ("boo ");
    unwind_protect_cleanup
      __printf_assert__ ("subyyay2 "); 
    end
    __printf_assert__ ("boo ");
  elseif a == 2
    % Nested unwind protect with return in body
    unwind_protect
      unwind_protect
        __printf_assert__ ("subyyay3 ");
        return
        __printf_assert__ ("boo ");
      unwind_protect_cleanup
        __printf_assert__ ("subyyay4 "); 
      end
      __printf_assert__ ("boo ");
    unwind_protect_cleanup
      __printf_assert__ ("subyyay5 "); 
    end
    __printf_assert__ ("boo ");
  elseif a == 3
    % Nested unwind protect with error in body
    unwind_protect
      unwind_protect
        __printf_assert__ ("subyyay6 ");
        error foooo
        __printf_assert__ ("boo ");
      unwind_protect_cleanup
        __printf_assert__ ("subyyay7 "); 
      end
      __printf_assert__ ("boo ");
    unwind_protect_cleanup
      __printf_assert__ ("subyyay8 "); 
    end
    __printf_assert__ ("boo ");
  elseif a == 4
    for i = 1:3
      unwind_protect
        __printf_assert__ ("subyyay9 ");
        break;
        __printf_assert__ ("boo ");
      unwind_protect_cleanup
        __printf_assert__ ("subyyay10 "); 
      end
      __printf_assert__ ("boo ");
    end
  elseif a == 5
    for i = 1:3
      unwind_protect
        __printf_assert__ ("subyyay11 ");
        for j = 1:3
          unwind_protect
            __printf_assert__ ("subyyay12 ");
            break;
            __printf_assert__ ("boo ");
          unwind_protect_cleanup
            __printf_assert__ ("subyyay13 "); 
          end
          __printf_assert__ ("boo ");
        end
        break;
        __printf_assert__ ("boo ");
      unwind_protect_cleanup
        __printf_assert__ ("subyyay14 "); 
      end
      __printf_assert__ ("boo ");
    end
  elseif a == 6
    % Mixing unwind protect with for loops. error and break
    for i = 1:3
      unwind_protect
        __printf_assert__ ("subyyay15 ");
        try
          for j = 1:3
            unwind_protect
              __printf_assert__ ("subyyay16 ");
              error ('qwe');
              __printf_assert__ ("boo ");
            unwind_protect_cleanup
              __printf_assert__ ("subyyay17 "); 
            end
            __printf_assert__ ("boo ");
          end
        catch
          break;
        end
        __printf_assert__ ("boo ");
      unwind_protect_cleanup
        __printf_assert__ ("subyyay18 "); 
      end
      __printf_assert__ ("boo ");
    end
  end
end
