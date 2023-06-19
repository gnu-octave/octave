function bench_cov (v)
  for i = 1:10000
    cov (v, v);
  end
end
