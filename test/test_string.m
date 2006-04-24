%% Automatically generated from DejaGNU files

%% test/octave.test/string/str-esc-1.m
%!test
%! x = 7;
%! if (strcmp ("\a", setstr (x)))
%! printf_assert ("ok\n");
%! endif
%! assert(prog_output_assert("ok"));

%% test/octave.test/string/str-esc-2.m
%!test
%! x = 8;
%! if (strcmp ("\b", setstr (x)))
%! printf_assert ("ok\n");
%! endif
%! assert(prog_output_assert("ok"));

%% test/octave.test/string/str-esc-3.m
%!test
%! x = 12;
%! if (strcmp ("\f", setstr (x)))
%! printf_assert ("ok\n");
%! endif
%! assert(prog_output_assert("ok"));

%% test/octave.test/string/str-esc-4.m
%!test
%! x = 10;
%! if (strcmp ("\n", setstr (x)))
%! printf_assert ("ok\n");
%! endif
%! assert(prog_output_assert("ok"));

%% test/octave.test/string/str-esc-5.m
%!test
%! x = 13;
%! if (strcmp ("\r", setstr (x)))
%! printf_assert ("ok\n");
%! endif
%! assert(prog_output_assert("ok"));

%% test/octave.test/string/str-esc-6.m
%!test
%! x = 9;
%! if (strcmp ("\t", setstr (x)))
%! printf_assert ("ok\n");
%! endif
%! assert(prog_output_assert("ok"));

%% test/octave.test/string/str-esc-7.m
%!test
%! x = 11;
%! if (strcmp ("\v", setstr (x)))
%! printf_assert ("ok\n");
%! endif
%! assert(prog_output_assert("ok"));

%% test/octave.test/string/str-esc-8.m
%!test
%! x = 92;
%! if (strcmp ("\\", setstr (x)))
%! printf_assert ("ok\n");
%! endif
%! assert(prog_output_assert("ok"));

%% test/octave.test/string/str-esc-9.m
%!test
%! x = 39;
%! if (strcmp ("\'", setstr (x)))
%! printf_assert ("ok\n");
%! endif
%! assert(prog_output_assert("ok"));

%% test/octave.test/string/str-esc-10.m
%!test
%! x = 34;
%! if (strcmp ("\"", setstr (x)))
%! printf_assert ("ok\n");
%! endif
%! assert(prog_output_assert("ok"));

%% test/octave.test/string/str-esc-11.m
%!test
%! x = 120;
%! fail('strcmp ("\x", setstr (x))',"warning",".*unrecognized escape sequence.*");

%% test/octave.test/string/str-esc-12.m
%!test
%! x = [7, 8, 12, 10, 13, 9, 11, 92, 39, 34];
%! if (strcmp ("\a\b\f\n\r\t\v\\\'\"", setstr (x)))
%! printf_assert ("ok\n");
%! endif
%! assert(prog_output_assert("ok"));

%% FIXME
%% Why do the next two tests fail?
%% test/octave.test/string/string_fill_char-1.m
%!#test
%! sfc = string_fill_char;
%! string_fill_char = "X";
%! str = ["these"; "are"; "strings"];
%! assert(str,["theseXX"; "areXXXX"; "strings"]);
%! string_fill_char = sfc;

%% test/octave.test/string/string_fill_char-2.m
%!#test
%! sfc = string_fill_char;
%! string_fill_char = " ";
%! str = ["these"; "are"; "strings"];
%! assert(str,["these  "; "are    "; "strings"]);
%! string_fill_char = sfc;

%% test/octave.test/string/ischar-1.m
%!assert(!(ischar (1)));

%% test/octave.test/string/ischar-2.m
%!assert(!(ischar ([1, 2])));

%% test/octave.test/string/ischar-3.m
%!assert(!(ischar ([])));

%% test/octave.test/string/ischar-4.m
%!assert(!(ischar ([1, 2; 3, 4])));

%% test/octave.test/string/ischar-5.m
%!assert(ischar (""));

%% test/octave.test/string/ischar-6.m
%!assert(ischar ("t"));

%% test/octave.test/string/ischar-7.m
%!assert(ischar ("test"));

%% test/octave.test/string/ischar-8.m
%!assert(ischar (["test"; "ing"]));

%% test/octave.test/string/ischar-9.m
%!test
%! s.a = "test";
%! assert(!(ischar (s)));

%% test/octave.test/string/ischar-10.m
%!error <Invalid call to ischar.*> ischar ();

%% test/octave.test/string/ischar-11.m
%!error <Invalid call to ischar.*> ischar ("test", 1);

%% test/octave.test/string/blanks-1.m
%!assert(strcmp (blanks (3), "   "));

%% test/octave.test/string/blanks-2.m
%!error blanks ();

%% test/octave.test/string/blanks-3.m
%!error blanks (1, 2);

%% test/octave.test/string/char-1.m
%!assert(strcmp (char ([65, 83, 67, 73, 73]), "ASCII"));

%% test/octave.test/string/char-2.m
%!error <Invalid call to char.*> char ();

%% test/octave.test/string/char-3.m
%!test
%! x = char ("foo", "bar", "foobar");
%! assert((strcmp (x(1,:), "foo   ")
%! && strcmp (x(2,:), "bar   ")
%! && strcmp (x(3,:), "foobar")));

%% test/octave.test/string/int2str-1.m
%!assert(strcmp (int2str (-123), "-123") && strcmp (int2str (1.2), "1"));

%% test/octave.test/string/int2str-2.m
%!error int2str ();

%% test/octave.test/string/int2str-3.m
%!error int2str (1, 2);

%% test/octave.test/string/num2str-1.m
%!assert((strcmp (num2str (123), "123") && strcmp (num2str (1.23), "1.23")));

%% test/octave.test/string/num2str-2.m
%!error num2str ();

%% test/octave.test/string/num2str-3.m
%!error num2str (1, 2, 3);

%% test/octave.test/string/strcat-1.m
%!assert((strcmp (strcat ("foo", "bar"), "foobar")
%! && strcmp (strcat (["a"; "bb"], ["foo"; "bar"]), ["a foo"; "bbbar"])));

%% test/octave.test/string/strcat-2.m
%!error strcat ();

%% test/octave.test/string/strcat-3.m
%!error strcat (1, 2);

%% test/octave.test/string/str2mat-1.m
%!assert(strcmp (str2mat ("a", "bb", "ccc"), ["a  "; "bb "; "ccc"]));

%% test/octave.test/string/str2mat-2.m
%!error str2mat ();

%% test/octave.test/string/str2mat-3.m
%!assert(all (str2mat (1, 2, 3) == setstr ([1; 2; 3])));

%% test/octave.test/string/deblank-1.m
%!assert(strcmp (deblank (" f o o  "), " f o o"));

%% test/octave.test/string/deblank-2.m
%!error deblank ();

%% test/octave.test/string/deblank-3.m
%!error deblank ("foo", "bar");

%% test/octave.test/string/deblank-4.m
%!error deblank (1);

%% test/octave.test/string/findstr-1.m
%!assert((findstr ("abababa", "a") == [1, 3, 5, 7]
%! && findstr ("abababa", "aba") == [1, 3, 5]
%! && findstr ("abababa", "aba", 0) == [1, 5]));

%% test/octave.test/string/findstr-2.m
%!error findstr ();

%% test/octave.test/string/findstr-3.m
%!error findstr ("foo", "bar", 3, 4);

%% test/octave.test/string/index-1.m
%!assert(index ("foobarbaz", "b") == 4 && index ("foobarbaz", "z") == 9);

%% test/octave.test/string/index-2.m
%!error index ();

%% test/octave.test/string/index-3.m
%!error index ("foo", "bar", 3);

%% test/octave.test/string/rindex-1.m
%!assert(rindex ("foobarbaz", "b") == 7 && rindex ("foobarbaz", "o") == 3);

%% test/octave.test/string/rindex-2.m
%!error rindex ();

%% test/octave.test/string/rindex-3.m
%!error rindex ("foo", "bar", 3);

%% test/octave.test/string/split-1.m
%!assert(all (all (split ("Test string", "t") == ["Tes "; " s  "; "ring"])));

%% test/octave.test/string/split-2.m
%!error split ();

%% test/octave.test/string/split-3.m
%!assert(all (strcmp (split ("foo bar baz", " ", 2), ["foo"; "bar baz"])));

%% test/octave.test/string/split-4.m
%!error split ("foo", "bar", 3, 4);

%% test/octave.test/string/strrep-1.m
%!assert(strcmp (strrep ("This is a test string", "is", "&%$"),
%! "Th&%$ &%$ a test string"));

%% test/octave.test/string/strrep-2.m
%!error strrep ();

%% test/octave.test/string/strrep-3.m
%!error strrep ("foo", "bar", 3, 4);

%% test/octave.test/string/substr-1.m
%!assert(strcmp (substr ("This is a test string", 6, 9), "is a test"));

%% test/octave.test/string/substr-2.m
%!error substr ();

%% test/octave.test/string/substr-3.m
%!error substr ("foo", 2, 3, 4);

%% test/octave.test/string/strcmp-1.m
%!assert(strcmp ("foobar", "foobar") && strcmp ("fooba", "foobar") == 0);

%% test/octave.test/string/strcmp-2.m
%!error <Invalid call to strcmp.*> strcmp ();

%% test/octave.test/string/strcmp-3.m
%!error <Invalid call to strcmp.*> strcmp ("foo", "bar", 3);

%% test/octave.test/string/bin2dec-1.m
%!assert(bin2dec ("1110") == 14);

%% test/octave.test/string/bin2dec-2.m
%!error bin2dec ();

%% test/octave.test/string/bin2dec-3.m
%!error bin2dec ("str", 1);

%% test/octave.test/string/dec2bin-1.m
%!assert(strcmp (dec2bin (14), "1110"));

%% test/octave.test/string/dec2bin-2.m
%!error dec2bin ();

%% test/octave.test/string/dec2bin-3.m
%!assert(strcmp (dec2bin (14, 6), "001110")
%! );

%% test/octave.test/string/dec2bin-4.m
%!error dec2bin (1, 2, 3);

%% test/octave.test/string/dec2hex-1.m
%!assert(strcmp (tolower (dec2hex (2748)), "abc"));

%% test/octave.test/string/dec2hex-2.m
%!error dec2hex ();

%% test/octave.test/string/dec2hex-3.m
%!assert(strcmp (tolower (dec2hex (2748, 5)), "00abc"));

%% test/octave.test/string/dec2hex-4.m
%!error dec2hex (1, 2, 3);

%% test/octave.test/string/hex2dec-1.m
%!assert(hex2dec ("12b") == 299 && hex2dec ("12B") == 299);

%% test/octave.test/string/hex2dec-2.m
%!error hex2dec ();

%% test/octave.test/string/hex2dec-3.m
%!error hex2dec ("str", 1);

%% test/octave.test/string/str2num-1.m
%!assert(str2num ("-1.3e2") == -130 && str2num ("[1, 2; 3, 4]") == [1, 2; 3, 4]);

%% test/octave.test/string/str2num-2.m
%!error str2num ();

%% test/octave.test/string/str2num-3.m
%!error str2num ("string", 1);

%% test/octave.test/string/undo_string_escapes-1.m
%!assert(strcmp (undo_string_escapes ("abc\a\b\n\r\t\v\f123"),
%! "abc\\a\\b\\n\\r\\t\\v\\f123"));

%% test/octave.test/string/undo_string_escapes-2.m
%!error <Invalid call to undo_string_escapes.*> undo_string_escapes ();

%% test/octave.test/string/undo_string_escapes-3.m
%!error <Invalid call to undo_string_escapes.*> undo_string_escapes ("string", 2);

%% test/octave.test/string/toascii-1.m
%!test
%! charset = setstr (0:127);
%! 
%! result = 0:127;
%! 
%! assert(all (toascii (charset) == result));

%% test/octave.test/string/toascii-3.m
%!error toascii (1, 2);

%% test/octave.test/string/toascii-3.m
%!error toascii (1, 2);

%% test/octave.test/string/tolower-1.m
%!test
%! charset = setstr (0:127);
%! 
%! result = charset;
%! 
%! result ((toascii("A"):toascii("Z"))+1) \
%! = result ((toascii("a"):toascii("z"))+1);
%! 
%! assert(all (tolower (charset) == result));

%% test/octave.test/string/tolower-3.m
%!error tolower (1, 2);

%% test/octave.test/string/tolower-3.m
%!error tolower (1, 2);

%% test/octave.test/string/toupper-1.m
%!test
%! charset = setstr (0:127);
%! 
%! result = charset;
%! 
%! result ((toascii("a"):toascii("z"))+1) \
%! = result ((toascii("A"):toascii("Z"))+1);
%! 
%! assert(all (toupper (charset) == result));

%% test/octave.test/string/toupper-3.m
%!error toupper (1, 2);

%% test/octave.test/string/toupper-3.m
%!error toupper (1, 2);

%% test/octave.test/string/isalnum-1.m
%!test
%! charset = setstr (0:127);
%! 
%! result = zeros (1, 128);
%! 
%! result ((toascii("A"):toascii("Z"))+1) = 1;
%! result ((toascii("0"):toascii("9"))+1) = 1;
%! result ((toascii("a"):toascii("z"))+1) = 1;
%! 
%! assert(all (isalnum (charset) == result));

%% test/octave.test/string/isalnum-2.m
%!error isalnum (1, 2);

%% test/octave.test/string/isalnum-3.m
%!error isalnum ();

%% test/octave.test/string/isalpha-1.m
%!test
%! charset = setstr (0:127);
%! 
%! result = zeros (1, 128);
%! 
%! result ((toascii("A"):toascii("Z"))+1) = 1;
%! result ((toascii("a"):toascii("z"))+1) = 1;
%! 
%! assert(all (isalpha (charset) == result));

%% test/octave.test/string/isalpha-2.m
%!error isalpha (1, 2);

%% test/octave.test/string/isalpha-3.m
%!error isalpha ();

%% test/octave.test/string/isascii-1.m
%!test
%! charset = setstr (0:127);
%! 
%! result = ones (1, 128);
%! 
%! assert(all (isascii (charset) == result));

%% test/octave.test/string/isascii-2.m
%!error isascii (1, 2);

%% test/octave.test/string/isascii-3.m
%!error isascii ();

%% test/octave.test/string/iscntrl-1.m
%!test
%! charset = setstr (0:127);
%! 
%! result = zeros (1, 128);
%! 
%! result (1:32) = 1;
%! result (128) = 1;
%! 
%! assert(all (iscntrl (charset) == result));

%% test/octave.test/string/iscntrl-2.m
%!error iscntrl (1, 2);

%% test/octave.test/string/iscntrl-3.m
%!error iscntrl ();

%% test/octave.test/string/isdigit-1.m
%!test
%! charset = setstr (0:127);
%! 
%! result = zeros (1, 128);
%! 
%! result ((toascii("0"):toascii("9"))+1) = 1;
%! 
%! assert(all (isdigit (charset) == result));

%% test/octave.test/string/isdigit-2.m
%!error isdigit (1, 2);

%% test/octave.test/string/isdigit-3.m
%!error isdigit ();

%% test/octave.test/string/isgraph-1.m
%!test
%! charset = setstr (0:127);
%! 
%! result = zeros (1, 128);
%! 
%! result (34:127) = 1;
%! 
%! assert(all (isgraph (charset) == result));

%% test/octave.test/string/isgraph-2.m
%!error isgraph (1, 2);

%% test/octave.test/string/isgraph-3.m
%!error isgraph ();

%% test/octave.test/string/islower-1.m
%!test
%! charset = setstr (0:127);
%! 
%! result = zeros (1, 128);
%! 
%! result ((toascii("a"):toascii("z"))+1) = 1;
%! 
%! assert(all (islower (charset) == result));

%% test/octave.test/string/islower-2.m
%!error islower (1, 2);

%% test/octave.test/string/islower-3.m
%!error islower ();

%% test/octave.test/string/isprint-1.m
%!test
%! charset = setstr (0:127);
%! 
%! result = zeros (1, 128);
%! 
%! result (33:127) = 1;
%! 
%! assert(all (isprint (charset) == result));

%% test/octave.test/string/isprint-2.m
%!error isprint (1, 2);

%% test/octave.test/string/isprint-3.m
%!error isprint ();

%% test/octave.test/string/ispunct-1.m
%!test
%! charset = setstr (0:127);
%! 
%! result = zeros (1, 128);
%! 
%! result (34:48) = 1;
%! result (59:65) = 1;
%! result (92:97) = 1;
%! result (124:127) = 1;
%! 
%! assert(all (ispunct (charset) == result));

%% test/octave.test/string/ispunct-2.m
%!error ispunct (1, 2);

%% test/octave.test/string/ispunct-3.m
%!error ispunct ();

%% test/octave.test/string/isspace-1.m
%!test
%! charset = setstr (0:127);
%! 
%! result = zeros (1, 128);
%! 
%! result (toascii (" \f\n\r\t\v")+1) = 1;
%! 
%! assert(all (isspace (charset) == result));

%% test/octave.test/string/isspace-2.m
%!error isspace (1, 2);

%% test/octave.test/string/isspace-3.m
%!error isspace ();

%% test/octave.test/string/isupper-1.m
%!test
%! charset = setstr (0:127);
%! 
%! result = zeros (1, 128);
%! 
%! result ((toascii("A"):toascii("Z"))+1) = 1;
%! 
%! assert(all (isupper (charset) == result));

%% test/octave.test/string/isupper-2.m
%!error isupper (1, 2);

%% test/octave.test/string/isupper-3.m
%!error isupper ();

%% test/octave.test/string/isxdigit-1.m
%!test
%! charset = setstr (0:127);
%! 
%! result = zeros (1, 128);
%! 
%! result ((toascii("A"):toascii("F"))+1) = 1;
%! result ((toascii("0"):toascii("9"))+1) = 1;
%! result ((toascii("a"):toascii("f"))+1) = 1;
%! 
%! assert(all (isxdigit (charset) == result));

%% test/octave.test/string/isxdigit-2.m
%!error isxdigit (1, 2);

%% test/octave.test/string/isxdigit-3.m
%!error isxdigit ();

