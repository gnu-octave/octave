
s_whos = whos;

if length (s_whos)
  s_whos = s_whos(end);

  % Only in the top scope nargin is equal to the size of argv
  if strcmp (s_whos.nesting.function, "top scope")
    assert (length (argv ()) == nargin ())
  end
end

assert (bytecode_script_nargin_expected_value == nargin)

if exist ("bytecode_script_nargin_call_recursive") && bytecode_script_nargin_call_recursive
  bytecode_script_nargin_call_recursive = false;
  bytecode_script_nargin;
  bytecode_script_nargin_call_recursive = true;
end
