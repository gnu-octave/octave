function A = qsort_iterative (A)
  len = length (A);
  stack = zeros (64, 1);

  top = 0;

  % Push initial indices to the stack
  stack (++top) = 1;
  stack (++top) = len;

  while top > 0
    % Pop indices
    high = stack (top--);
    low = stack (top--);

    % Partion part of the algorithm
    p = low - 1; % pivot index
    x = A (high);% pivot value

    % Swap so that there are two parts. One less than or equal to the pivot
    % value and one higher
    for j = low:high - 1
      if A(j) <= x
        p++;
        tmp = A(j);
        A(j) = A(p);
        A(p) = tmp;
      end
    end
    % Swap the pivot value with the first value bigger than the pivot
    p++;
    tmp = A(high);
    A(high) = A(p);
    A(p) = tmp;
    % End partion

    % Push left and right indices (if there are any value to the left or right)
    if p - 1 > low
      stack(++top) = low;
      stack(++top) = p - 1;
    end

    if p + 1 < high
      stack(++top) = p + 1;
      stack(++top) = high;
    end
  end
end
