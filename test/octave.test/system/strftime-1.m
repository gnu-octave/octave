fmt = "%%%n%t%H%I%k%l%M%p%r%R%s%S%T%X%Z%a%A%b%B%c%C%d%e%D%h%j%m%U%w%W%x%y%Y";
isstr (strftime (fmt, localtime (time ())))