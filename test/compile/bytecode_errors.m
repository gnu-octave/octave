function bytecode_errors (idx)
  % We put test dispatch last, so that we don't have
  % to update all columns and rows each time we 
  % add a test ...
  run_test (idx);
end

function if_undefined_value ()
  if qweqwe
    a = 2;
  end
end

function assign_undef ()
  a = b;
end

function subsref_undef_id ()
  a = b(1,2,3);
end

function subsref_cell_undef_id ()
  a = b{1,2,3};
end

function wordcmd_undef_id ()
  b 1 2 3;
end

function binary_undef ()
  b * a;
end

function id_index_oob_error_1 ()
  a = [1, 2];
  b = a (3);
end

function id_index_oob_error_2 ()
  a = [1, 2];
  b = a (-1);
end

function binary_wrong_size_1 ()
  a = [1 2 3] + [1 2];
end

function run_test (idx)
  if idx == 0
    if_undefined_value ();
  elseif idx == 1
    assign_undef ();
  elseif idx == 2
    subsref_undef_id ();
  elseif idx == 3
    subsref_cell_undef_id ();
  elseif idx == 4
    wordcmd_undef_id ();
  elseif idx == 5
    binary_undef ();
  elseif idx == 6
    id_index_oob_error_1 ();
  elseif idx == 7
    id_index_oob_error_2 ();
  elseif idx == 8
    binary_wrong_size_1 ();
  end
end
