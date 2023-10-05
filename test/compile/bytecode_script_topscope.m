% (bug #64705)
%
% Test the interaction between a script called from top scope and 
% global and local variables

% If this global is true, we do a recursive call to this script
% to add another frame.
global bytecode_script_topscope_call_self
if bytecode_script_topscope_call_self
  bytecode_script_topscope_call_self = false;
  bytecode_script_topscope;
  bytecode_script_topscope_call_self = true;
  return
end

global bytecode_script_topscope_place % "caller" or "base", set in the test script

assert (ans == 1234); % Last calculation in bytecode_script_topscope_setup_script
4321; % Change ans to 4321
evalin (bytecode_script_topscope_place, "assert (ans == 4321)"); % Check that ans changed in "caller" or "base" frame
evalin ("caller", "assert (ans == 4321)"); % Should always change in caller

% These should allready be global
assert (isglobal ("glb_a"))
assert (isglobal ("glb_b"))
assert (isglobal ("glb_c"))
assert (isglobal ("glb_d"))
assert (isglobal ("glb_f"))
assert (isglobal ("glb_g"))

global glb_a % "re-global"
global glb_b % "re-global"
global glb_c % "re-global"
global glb_d = 1234; % re-global, init should not run due to being defined in top scope allready
global glb_e = 6; % Not defined in top scope
eval ("global glb_f"); % "re-global", dynamic frame
global glb_g % "re-global"

assert (glb_a == 2)
assert (glb_b == 3)
assert (glb_c == 4)
assert (glb_d == 5)
assert (glb_e == 6)
eval ("assert (glb_f == 7)")
assert (glb_g == 8)

assert (local_b == 103)

glb_b = 33;

clear global glb_c;

glb_d = 55;

%% Locals

local_a = 102; % Will be added to top scope

local_b = 113; % Added in top scope, change it

clear local_c % Added in top scope, clear it here

% Added in top scope. Clear after using
local_d = 123;
clear local_d;

% Not added in top scope. Clear after using
local_e = 123;
clear local_e;

% Not added in top scope. Clear from top scope ...
local_f = 123;
evalin (bytecode_script_topscope_place, "clear local_f");
assert (!exist ("local_f"))

% Added in top scope. Clear from top scope ...
local_g = 123;
evalin (bytecode_script_topscope_place, "clear local_g");
assert (!exist ("local_g"))
