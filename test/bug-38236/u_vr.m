# u_vr.m

cmd = "\
function __demo__ () \
  df_vr; \
  v = vr * 2; \
endfunction \
";

for ii = 1:2
  unwind_protect
    eval (cmd);
    __demo__;
  unwind_protect_cleanup
    clear __demo__
  end_unwind_protect
endfor