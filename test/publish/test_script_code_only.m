
% Code only with a very very very very very very very very very very looong line
%
x = 5;

for i = 1:5
  x += i;  # Might be useful "perhaps"
endfor

%{
Multiline comment with keyword if "if" and 'if'
%}

if (x == 'a')
  y = sin (x);
endif

#{
Multiline comment with keyword if "if" and 'if'
#}

str = "some % string \" ' with %{";
str2 = 'another % string '' " with %{';
%
