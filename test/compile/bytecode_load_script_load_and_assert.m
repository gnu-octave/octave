local_cc = 33; %Defined before the load
glb_ee = 123;

assert (!isglobal ("glb_aa"))
assert (!isglobal ("glb_bb"))
assert (!isglobal ("glb_cc"))
assert (!isglobal ("glb_dd"))
assert (!isglobal ("glb_ee"))

global bytecode_load_script_file
load (bytecode_load_script_file)

assert (isglobal ("glb_aa"))
assert (isglobal ("glb_bb"))
assert (isglobal ("glb_cc"))
assert (isglobal ("glb_dd"))
assert (isglobal ("glb_ee"))

assert (local_aa == 1)
assert (local_bb == 2)
assert (local_cc == 3)
assert (glb_aa == 1);
assert (isempty (glb_bb));
assert (isempty (glb_cc));
assert (glb_dd == 4);
assert (glb_ee == 5);