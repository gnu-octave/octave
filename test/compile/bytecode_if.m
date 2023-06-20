function bytecode_if()
  ctr = 0;
  a = 1;
  b = 0;

  if a
    __printf_assert__ ("%d ", ctr++);
  end

  if a
    __printf_assert__ ("%d ", ctr++);
  else
    __printf_assert__ ("booo ");
  end

  if a
    __printf_assert__ ("%d ", ctr++);
  elseif a
    __printf_assert__ ("booo ");
  else
    __printf_assert__ ("booo ");
  end

  if b
    __printf_assert__ ("booo ", ctr++);
  end

  if b
    __printf_assert__ ("booo ");
  else
    __printf_assert__ ("%d ", ctr++);
  end

  if b
    __printf_assert__ ("booo ");
  elseif b
    __printf_assert__ ("booo ");
  else
    __printf_assert__ ("%d ", ctr++);
  end

  if b
    __printf_assert__ ("booo ");
  elseif a
    __printf_assert__ ("%d ", ctr++);
  else
    __printf_assert__ ("booo ");
  end

  if a
    if a
      if a
        if a
          if a
            if b
              __printf_assert__ ("booo ");
            else
              if a
                if a
                  if b
                    __printf_assert__ ("booo ");
                  elseif a
                    if b
                      __printf_assert__ ("booo ");
                    else
                      __printf_assert__ ("%d ", ctr++);
                    end
                  else
                    __printf_assert__ ("booo ");
                  end
                end
              end
            end
          end
        else
          __printf_assert__ ("booo ");
        end
      end
    end
  end

  if 3 > 2
    __printf_assert__ ("%d ", ctr++);
  end

  if []
    __printf_assert__ ("booo ");
  end

  if ~b
    __printf_assert__ ("%d ", ctr++);
  end

  if b
  end

  % "Braindead" short circuit
  %
  % We also check that there is a proper short circuit
  if truthy (1) & truthy (2)
    __printf_assert__ ("yay1 ");
  end

  if falsy (3) & truthy (4)
    __printf_assert__ ("booo ");
  end

  if falsy (5) & falsy (6)
    __printf_assert__ ("booo ");
  end

  if truthy (7) & falsy (8)
    __printf_assert__ ("booo ");
  end

  if truthy (1)| truthy (2)
    __printf_assert__ ("yay1 ");
  end

  if falsy (3) | truthy (4)
    __printf_assert__ ("yay2 ");
  end

  if falsy (5) | falsy (6)
    __printf_assert__ ("booo ");
  end

  if truthy (7) | falsy (8)
    __printf_assert__ ("yay3 ");
  end
end

function a = truthy (b)
  __printf_assert__ ("%d ", b);
  a = 1;
end

function a = falsy (b)
  __printf_assert__ ("%d ", b);
  a = 0;
end

