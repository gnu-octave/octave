function bytecode_script_topscope_setup (place = "base")
 evalin (place, "global glb_a = 2");
 evalin (place, "global glb_b = 3");
 evalin (place, "global glb_c = 4");
 evalin (place, "global glb_d = 5");
 evalin (place, "global glb_f = 7");
 evalin (place, "local_b = 103;");
 evalin (place, "local_c = 104;");
 evalin (place, "local_d = 105;");
 evalin (place, "local_g = 108;");
endfunction