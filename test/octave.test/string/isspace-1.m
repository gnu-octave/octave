charset = setstr (0:127);

result = zeros (1, 128);

result (toascii (" \f\n\r\t\v")+1) = 1;

all (isspace (charset) == result)
