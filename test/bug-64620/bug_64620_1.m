function bug_64620_1
  h = subby;
end

function h = subby % The h return is needed for the bug to show
  function nested_fn
    % This framed stored shared pointer to subby's frame when put in handle
  end

  h = @nested_fn;

  obj = cdef_counts_self;
  % obj destructor was not triggered
end
