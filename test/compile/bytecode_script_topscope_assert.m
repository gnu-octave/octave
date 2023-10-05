function bytecode_script_topscope_assert (place = "base")
 evalin (place, "assert (isglobal ('glb_a'))");
 evalin (place, "assert (isglobal ('glb_b'))");
 evalin (place, "assert (!isglobal ('glb_c'))"); % Unglobalized in script
 evalin (place, "assert (isglobal ('glb_d'))");
 evalin (place, "assert (isglobal ('glb_e'))");
 evalin (place, "assert (isglobal ('glb_g'))");
 evalin (place, "assert (glb_a == 2)");
 evalin (place, "assert (glb_b == 33)");
 evalin (place, "assert (!exist ('glb_c'))");
 evalin (place, "assert (glb_d == 55)");
 evalin (place, "assert (glb_e == 6)"); % Added in the script
 evalin (place, "assert (glb_g == 8)");
 evalin (place, "assert (local_a == 102)"); % Local added in script
 evalin (place, "assert (local_b == 113)");
 evalin (place, "assert (!exist ('local_c'))"); % Cleared in script
 evalin (place, "assert (!exist ('local_d'))"); % Cleared in script
 evalin (place, "assert (!exist ('local_e'))"); % Cleared in script
 evalin (place, "assert (!exist ('local_f'))"); % Cleared in script
 evalin (place, "assert (!exist ('local_g'))"); % Cleared in script
 evalin (place, "assert (local_h == 456)");

 evalin (place, "clear global glb_a glb_b glb_c glb_d glb_e glb_f glb_g");
 evalin (place, "clear local_a local_b local_c local_d local_e local_f local_g local_h");
endfunction