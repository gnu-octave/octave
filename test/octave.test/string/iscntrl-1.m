charset = setstr (0:127);

result = zeros (1, 128);

result (1:32) = 1;
result (128) = 1;

all (iscntrl (charset) == result)
