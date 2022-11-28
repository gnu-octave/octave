%!function [raa, rbb, igaa, igbb] = sub1 (aa, bb)
%!  global aa bb;
%!  aa = 1;
%!  raa = aa;
%!  rbb = bb;
%!  igaa = isglobal ("aa");
%!  igbb = isglobal ("bb");
%!endfunction

%!test <*53579>
%! global aa bb;
%! assert (isglobal ("aa"));
%! assert (isglobal ("bb"));
%! aa = 3;
%! xx = 5;
%! [raa, rbb, igaa, igbb] = sub1 (aa, xx);
%! assert (raa, 1);
%! assert (rbb, []);
%! assert (igaa);
%! assert (igbb);
%! assert (xx, 5);
%! clear -global aa bb;  # cleanup after test
