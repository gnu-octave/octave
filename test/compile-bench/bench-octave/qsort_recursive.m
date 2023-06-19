% Most time is spent copying vectors anyway so terrible test really.

function A = qsort_recursive (A)
  if isempty (A) || length(A) == 1
    return;
  end

  [p, A] = partion (A);

  left = qsort_recursive (A(1:p - 1));
  right = qsort_recursive (A(p + 1:end));

  A = [left A(p) right];
end

function [p, A] = partion (A)
  lo = 1;
  hi = length (A);

  pivot = A(hi);

  p = lo - 1;

  for j = lo:1:hi-1
    if A(j) <= pivot
      p++;
      tmp = A(j);
      A (j) = A (p);
      A (p) = tmp;
    end
  end
  p++;
  tmp = A(hi);
  A(hi) = A(p);
  A(p) = tmp;
end
