function for_loop_ifs (n)
  for i = 1:n
    if i == 100
      continue;
    elseif i == 300
      continue;
    end

    if i * 2 == 3002
      continue;
    end

    if i < 0
      break;
    end

    if i == -1024
      disp ("foooo");
    end

    if i == n + 1
      break;
    end

    if ~i
      break;
    end
  end
end

