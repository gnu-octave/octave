# run_cmd: short script used in demos
# prints string cmd to the screen, then executes after a pause

# $Revision: 1.1.1.1 $
# $Log: run_cmd.m,v $
# Revision 1.1.1.1  1998/05/19 20:24:05  jwe
#
# Revision 1.4  1997/02/20 16:07:26  hodel
# added "fflush" after disp("executing")
#
# Revision 1.3  1997/02/12 15:38:14  hodel
# added separator after command execution
#
# added blank line after eval(cmd)
#
# Revision 1.1  1997/02/12 11:35:08  hodel
# Initial revision
#
# Revision 1.3  1997/02/07 15:44:13  scotte
# Added "executing" string so that users would know that the command was
# being processed
#

disp(["Command: ",cmd])
puts("Press a key to execute command");
fflush(stdout);
kbhit();
disp("  executing");
fflush(stdout);
eval(cmd);
disp("---")
disp(" ")
