global glb_aa = 1;
global glb_bb
eval ("global glb_cc;")
eval ("global glb_dd = 4;")
global glb_ee;
glb_ee = 5;

local_aa = 1;
eval ("local_bb = 2;")
local_cc = 3;

global bytecode_load_script_file
save (bytecode_load_script_file, "glb_aa", "glb_bb", "glb_cc", "glb_dd", "glb_ee", "local_aa", "local_bb", "local_cc")

clear global glb_aa glb_bb glb_cc glb_dd glb_ee
clear local_aa local_bb local_cc