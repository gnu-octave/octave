function r = getsize_cellfun (x)
  ## Must use @numel so that function overloading can occur, rather than
  ## 'numel'  which is built in to cellfun().
  r = cellfun (@numel, {x.d});
end
