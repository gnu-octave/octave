# run_cmd: short script used in demos
# prints string cmd to the screen, then executes after a pause

# $Revision: 2.0.0.2 $

disp(["Command: ",cmd])
puts("Press a key to execute command");
fflush(stdout);
kbhit();
disp("  executing");
fflush(stdout);
eval(cmd);
disp("---")
disp(" ")
