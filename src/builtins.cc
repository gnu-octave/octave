// DO NOT EDIT!  Generated automatically by mkbuiltins.

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "mappers.h"
#include "variables.h"
#include "builtins.h"
#include "oct-obj.h"

static void
install_builtin_functions (void)
{
 extern Octave_object      Fall    (const Octave_object& args, int nargout) ; static builtin_function     Sall   = {   "all"  ,     2  ,     1  ,   0 ,     Fall  ,    
  "all (X): are all elements of X nonzero?"   }; install_builtin_function (&    Sall  );   
 extern Octave_object      Fany    (const Octave_object& args, int nargout) ; static builtin_function     Sany   = {   "any"  ,     2  ,     1  ,   0 ,     Fany  ,    
  "any (X): are any elements of X nonzero?"   }; install_builtin_function (&    Sany  );   
 extern Octave_object      Fcumprod    (const Octave_object& args, int nargout) ; static builtin_function     Scumprod   = {   "cumprod"  ,     2  ,     1  ,   0 ,     Fcumprod  ,    
  "cumprod (X): cumulative products"   }; install_builtin_function (&    Scumprod  );   
 extern Octave_object      Fcumsum    (const Octave_object& args, int nargout) ; static builtin_function     Scumsum   = {   "cumsum"  ,     2  ,     1  ,   0 ,     Fcumsum  ,    
  "cumsum (X): cumulative sums"   }; install_builtin_function (&    Scumsum  );   
 extern Octave_object      Fdiag    (const Octave_object& args, int nargout) ; static builtin_function     Sdiag   = {   "diag"  ,     3  ,     1  ,   0 ,     Fdiag  ,    
  "diag (X [,k]): form/extract diagonals"   }; install_builtin_function (&    Sdiag  );   
 extern Octave_object      Fisstr    (const Octave_object& args, int nargout) ; static builtin_function     Sisstr   = {   "isstr"  ,     2  ,     1  ,   0 ,     Fisstr  ,    
  "isstr (X): return 1 if X is a string, 0 otherwise"   }; install_builtin_function (&    Sisstr  );   
 extern Octave_object      Fprod    (const Octave_object& args, int nargout) ; static builtin_function     Sprod   = {   "prod"  ,     2  ,     1  ,   0 ,     Fprod  ,    
  "prod (X): products"   }; install_builtin_function (&    Sprod  );   
 extern Octave_object      Fsetstr    (const Octave_object& args, int nargout) ; static builtin_function     Ssetstr   = {   "setstr"  ,     2  ,     1  ,   0 ,     Fsetstr  ,    
  "setstr (V): convert a vector to a string"   }; install_builtin_function (&    Ssetstr  );   
 extern Octave_object      Fsize    (const Octave_object& args, int nargout) ; static builtin_function     Ssize   = {   "size"  ,     2  ,     1  ,   0 ,     Fsize  ,    
  "[m, n] = size (x): return rows and columns of X"   }; install_builtin_function (&    Ssize  );   
 extern Octave_object      Fsum    (const Octave_object& args, int nargout) ; static builtin_function     Ssum   = {   "sum"  ,     2  ,     1  ,   0 ,     Fsum  ,    
  "sum (X): sum of elements"   }; install_builtin_function (&    Ssum  );   
 extern Octave_object      Fsumsq    (const Octave_object& args, int nargout) ; static builtin_function     Ssumsq   = {   "sumsq"  ,     2  ,     1  ,   0 ,     Fsumsq  ,    
  "sumsq (X): sum of squares of elements"   }; install_builtin_function (&    Ssumsq  );   
 extern Octave_object      Fones    (const Octave_object& args, int nargout) ; static builtin_function     Sones   = {   "ones"  ,     3  ,     1  ,   0 ,     Fones  ,    
  "ones (N), ones (N, M), ones (X): create a matrix of all ones"   }; install_builtin_function (&    Sones  );   
 extern Octave_object      Fzeros    (const Octave_object& args, int nargout) ; static builtin_function     Szeros   = {   "zeros"  ,     3  ,     1  ,   0 ,     Fzeros  ,    
  "zeros (N), zeros (N, M), zeros (X): create a matrix of all zeros"   }; install_builtin_function (&    Szeros  );   
 extern Octave_object      Feye    (const Octave_object& args, int nargout) ; static builtin_function     Seye   = {   "eye"  ,     3  ,     1  ,   0 ,     Feye  ,    
  "eye (N), eye (N, M), eye (X): create an identity matrix"   }; install_builtin_function (&    Seye  );   
 extern Octave_object      Fcd    (const Octave_object& args, int nargout) ; static builtin_function     Scd   = {   "cd"  ,     2  ,     1  ,   1 ,     Fcd  ,    
  "cd [dir]\n\nchange current working directory\nif no arguments are given, the current directory is changed to the\nusers home directory"   }; install_builtin_function (&    Scd  );   
 extern Octave_object      Fls    (const Octave_object& args, int nargout) ; static builtin_function     Sls   = {   "ls"  ,     -1  ,     1  ,   1 ,     Fls  ,    
  "ls [options]\n\nprint a directory listing"   }; install_builtin_function (&    Sls  );   
 alias_builtin ("dir", "ls");   ;

 extern Octave_object      Fpwd    (const Octave_object& args, int nargout) ; static builtin_function     Spwd   = {   "pwd"  ,     1  ,     0  ,   0 ,     Fpwd  ,    
  "pwd (): print current working directory"   }; install_builtin_function (&    Spwd  );   
 extern Octave_object      Ferror    (const Octave_object& args, int nargout) ; static builtin_function     Serror   = {   "error"  ,     2  ,     1  ,   0 ,     Ferror  ,    
  "error (MESSAGE): print MESSAGE and set the error state.\nThis should eventually take us up to the top level, possibly\nprinting traceback messages as we go.\n\nIf MESSAGE ends in a newline character, traceback messages are not\nprinted."   }; install_builtin_function (&    Serror  );    
 extern Octave_object      Ffclose    (const Octave_object& args, int nargout) ; static builtin_function     Sfclose   = {   "fclose"  ,     2  ,     1  ,   0 ,     Ffclose  ,    
  "fclose (FILENAME or FILENUM): close a file"   }; install_builtin_function (&    Sfclose  );   
 extern Octave_object      Ffflush    (const Octave_object& args, int nargout) ; static builtin_function     Sfflush   = {   "fflush"  ,     2  ,     1  ,   0 ,     Ffflush  ,    
  "fflush (FILENAME or FILENUM): flush buffered data to output file"   }; install_builtin_function (&    Sfflush  );   
 extern Octave_object      Ffgets    (const Octave_object& args, int nargout) ; static builtin_function     Sfgets   = {   "fgets"  ,     3  ,     2  ,   0 ,     Ffgets  ,    
  "[STRING, LENGTH] = fgets (FILENAME or FILENUM, LENGTH)\n\nread a string from a file"   }; install_builtin_function (&    Sfgets  );   
 extern Octave_object      Ffopen    (const Octave_object& args, int nargout) ; static builtin_function     Sfopen   = {   "fopen"  ,     3  ,     1  ,   0 ,     Ffopen  ,    
  "FILENUM = fopen (FILENAME, MODE): open a file\n\n  Valid values for mode include:\n\n   r  : open text file for reading\n   w  : open text file for writing; discard previous contents if any\n   a  : append; open or create text file for writing at end of file\n   r+ : open text file for update (i.e., reading and writing)\n   w+ : create text file for update; discard previous contents if any\n   a+ : append; open or create text file for update, writing at end\n\n Update mode permits reading from and writing to the same file."   }; install_builtin_function (&    Sfopen  );   
 extern Octave_object      Ffreport    (const Octave_object& args, int nargout) ; static builtin_function     Sfreport   = {   "freport"  ,     1  ,     1  ,   0 ,     Ffreport  ,    
  "freport (): list open files and their status"   }; install_builtin_function (&    Sfreport  );   
 extern Octave_object      Ffrewind    (const Octave_object& args, int nargout) ; static builtin_function     Sfrewind   = {   "frewind"  ,     2  ,     1  ,   0 ,     Ffrewind  ,    
  "frewind (FILENAME or FILENUM): set file position at beginning of file"   }; install_builtin_function (&    Sfrewind  );   
 extern Octave_object      Ffseek    (const Octave_object& args, int nargout) ; static builtin_function     Sfseek   = {   "fseek"  ,     4  ,     1  ,   0 ,     Ffseek  ,    
  "fseek (FILENAME or FILENUM, OFFSET [, ORIGIN])\n\nset file position for reading or writing"   }; install_builtin_function (&    Sfseek  );   
 extern Octave_object      Fftell    (const Octave_object& args, int nargout) ; static builtin_function     Sftell   = {   "ftell"  ,     2  ,     1  ,   0 ,     Fftell  ,    
  "POSITION = ftell (FILENAME or FILENUM): returns the current file position"   }; install_builtin_function (&    Sftell  );   
 extern Octave_object      Ffprintf    (const Octave_object& args, int nargout) ; static builtin_function     Sfprintf   = {   "fprintf"  ,     -1  ,     1  ,   0 ,     Ffprintf  ,    
  "fprintf (FILENAME or FILENUM, FORMAT, ...)"   }; install_builtin_function (&    Sfprintf  );   
 extern Octave_object      Fprintf    (const Octave_object& args, int nargout) ; static builtin_function     Sprintf   = {   "printf"  ,     -1  ,     1  ,   0 ,     Fprintf  ,    
  "printf (FORMAT, ...)"   }; install_builtin_function (&    Sprintf  );   
 extern Octave_object      Fsprintf    (const Octave_object& args, int nargout) ; static builtin_function     Ssprintf   = {   "sprintf"  ,     -1  ,     1  ,   0 ,     Fsprintf  ,    
  "s = sprintf (FORMAT, ...)"   }; install_builtin_function (&    Ssprintf  );   
 extern Octave_object      Ffscanf    (const Octave_object& args, int nargout) ; static builtin_function     Sfscanf   = {   "fscanf"  ,     3  ,     -1  ,   0 ,     Ffscanf  ,    
  "[A, B, C, ...] = fscanf (FILENAME or FILENUM, FORMAT)"   }; install_builtin_function (&    Sfscanf  );   
 extern Octave_object      Fscanf    (const Octave_object& args, int nargout) ; static builtin_function     Sscanf   = {   "scanf"  ,     2  ,     -1  ,   0 ,     Fscanf  ,    
  "[A, B, C, ...] = scanf (FORMAT)"   }; install_builtin_function (&    Sscanf  );   
 extern Octave_object      Fsscanf    (const Octave_object& args, int nargout) ; static builtin_function     Ssscanf   = {   "sscanf"  ,     3  ,     -1  ,   0 ,     Fsscanf  ,    
  "[A, B, C, ...] = sscanf (STRING, FORMAT)"   }; install_builtin_function (&    Ssscanf  );   
 extern Octave_object      Ffread    (const Octave_object& args, int nargout) ; static builtin_function     Sfread   = {   "fread"  ,     4  ,     2  ,   0 ,     Ffread  ,    
  "[DATA, COUNT] = fread (FILENUM, SIZE, PRECISION)\n\n Reads data in binary form of type PRECISION from a file.\n\n FILENUM   : file number from fopen\n SIZE      : size specification for the Data matrix\n PRECISION : type of data to read, valid types are\n\n               'char',   'schar', 'short',  'int',  'long', 'float'\n               'double', 'uchar', 'ushort', 'uint', 'ulong'\n\n DATA      : matrix in which the data is stored\n COUNT     : number of elements read"   }; install_builtin_function (&    Sfread  );   
 extern Octave_object      Ffwrite    (const Octave_object& args, int nargout) ; static builtin_function     Sfwrite   = {   "fwrite"  ,     4  ,     1  ,   0 ,     Ffwrite  ,    
  "COUNT = fwrite (FILENUM, DATA, PRECISION)\n\n Writes data to a file in binary form of size PRECISION\n\n FILENUM   : file number from fopen\n DATA      : matrix of elements to be written\n PRECISION : type of data to read, valid types are\n\n               'char',   'schar', 'short',  'int',  'long', 'float'\n               'double', 'uchar', 'ushort', 'uint', 'ulong'\n\n COUNT     : number of elements written"   }; install_builtin_function (&    Sfwrite  );   
 extern Octave_object      Ffeof    (const Octave_object& args, int nargout) ; static builtin_function     Sfeof   = {   "feof"  ,     2  ,     1  ,   0 ,     Ffeof  ,    
  "ERROR = feof (FILENAME or FILENUM)\n\n Returns a non zero value for an end of file condition for the\n file specified by FILENAME or FILENUM from fopen"   }; install_builtin_function (&    Sfeof  );   
 extern Octave_object      Fferror    (const Octave_object& args, int nargout) ; static builtin_function     Sferror   = {   "ferror"  ,     2  ,     1  ,   0 ,     Fferror  ,    
  "ERROR = ferror (FILENAME or FILENUM)\n\n Returns a non zero value for an error condition on the\n file specified by FILENAME or FILENUM from fopen"   }; install_builtin_function (&    Sferror  );   
 extern Octave_object      Fhelp    (const Octave_object& args, int nargout) ; static builtin_function     Shelp   = {   "help"  ,     -1  ,     1  ,   1 ,     Fhelp  ,    
  "help [-i] [topic ...]\n\nprint cryptic yet witty messages"   }; install_builtin_function (&    Shelp  );   
 extern Octave_object      Ftype    (const Octave_object& args, int nargout) ; static builtin_function     Stype   = {   "type"  ,     -1  ,     1  ,   1 ,     Ftype  ,    
  "type NAME ...]\n\ndisplay the definition of each NAME that refers to a function"   }; install_builtin_function (&    Stype  );   
 extern Octave_object      Fwhich    (const Octave_object& args, int nargout) ; static builtin_function     Swhich   = {   "which"  ,     -1  ,     1  ,   1 ,     Fwhich  ,    
  "which NAME ...]\n\ndisplay the type of each NAME.  If NAME is defined from an function\nfile, print the full name of the file."   }; install_builtin_function (&    Swhich  );   
 extern Octave_object      Finput    (const Octave_object& args, int nargout) ; static builtin_function     Sinput   = {   "input"  ,     3  ,     1  ,   0 ,     Finput  ,    
  "input (PROMPT [, S])\n\nPrompt user for input.  If the second argument is present, return
value as a string."   }; install_builtin_function (&    Sinput  );   
 extern Octave_object      Fkeyboard    (const Octave_object& args, int nargout) ; static builtin_function     Skeyboard   = {   "keyboard"  ,     2  ,     1  ,   0 ,     Fkeyboard  ,    
  "keyboard (PROMPT)\n\nmaybe help in debugging function files"   }; install_builtin_function (&    Skeyboard  );   
 extern Octave_object      Fload    (const Octave_object& args, int nargout) ; static builtin_function     Sload   = {   "load"  ,     -1  ,     1  ,   1 ,     Fload  ,    
  "load [-force] [-ascii] [-binary] [-mat-binary] file [pattern ...]\n
\nload variables from a file"   }; install_builtin_function (&    Sload  );   
 extern Octave_object      Fsave    (const Octave_object& args, int nargout) ; static builtin_function     Ssave   = {   "save"  ,     -1  ,     1  ,   1 ,     Fsave  ,    
  "save [-ascii] [-binary] [-save-builtins] file [pattern ...]\n\nsave variables in a file"   }; install_builtin_function (&    Ssave  );   
 extern Octave_object      Fcasesen    (const Octave_object& args, int nargout) ; static builtin_function     Scasesen   = {   "casesen"  ,     2  ,     1  ,   1 ,     Fcasesen  ,    
  "casesen [on|off]"   }; install_builtin_function (&    Scasesen  );   
 extern Octave_object      Fflops    (const Octave_object& args, int nargout) ; static builtin_function     Sflops   = {   "flops"  ,     2  ,     1  ,   0 ,     Fflops  ,    
  "flops (): count floating point operations"   }; install_builtin_function (&    Sflops  );   
 extern Octave_object      Fquit    (const Octave_object& args, int nargout) ; static builtin_function     Squit   = {   "quit"  ,     1  ,     0  ,   0 ,     Fquit  ,    
  "quit (): exit Octave gracefully"   }; install_builtin_function (&    Squit  );   
 alias_builtin ("exit", "quit");   ;

 extern Octave_object      Fwarranty    (const Octave_object& args, int nargout) ; static builtin_function     Swarranty   = {   "warranty"  ,     1  ,     0  ,   0 ,     Fwarranty  ,    
  "warranty (): describe copying conditions"   }; install_builtin_function (&    Swarranty  );   
 extern Octave_object      Ffeval    (const Octave_object& args, int nargout) ; static builtin_function     Sfeval   = {   "feval"  ,     -1  ,     1  ,   0 ,     Ffeval  ,    
  "feval (NAME, ARGS, ...)\n\nevaluate NAME as a function, passing ARGS as its arguments"   }; install_builtin_function (&    Sfeval  );   
 extern Octave_object      Feval    (const Octave_object& args, int nargout) ; static builtin_function     Seval   = {   "eval"  ,     2  ,     1  ,   0 ,     Feval  ,    
  "eval (STRING): evaluate STRING as octave code"   }; install_builtin_function (&    Seval  );   
 extern Octave_object      Fshell_cmd    (const Octave_object& args, int nargout) ; static builtin_function     Sshell_cmd   = {   "shell_cmd"  ,     2  ,     1  ,   0 ,     Fshell_cmd  ,    
  "shell_cmd (string [, return_output]): execute shell commands"   }; install_builtin_function (&    Sshell_cmd  );   
 extern Octave_object      Fedit_history    (const Octave_object& args, int nargout) ; static builtin_function     Sedit_history   = {   "edit_history"  ,     -1  ,     1  ,   1 ,     Fedit_history  ,    
  "edit_history [first] [last]\n\nedit commands from the history list"   }; install_builtin_function (&    Sedit_history  );   
 extern Octave_object      Fhistory    (const Octave_object& args, int nargout) ; static builtin_function     Shistory   = {   "history"  ,     -1  ,     1  ,   1 ,     Fhistory  ,    
  "history [N] [-w file] [-r file] [-q]\n\ndisplay, save, or load command history"   }; install_builtin_function (&    Shistory  );   
 extern Octave_object      Frun_history    (const Octave_object& args, int nargout) ; static builtin_function     Srun_history   = {   "run_history"  ,     -1  ,     1  ,   1 ,     Frun_history  ,    
  "run_history [first] [last]\n\nrun commands from the history list"   }; install_builtin_function (&    Srun_history  );   
 extern Octave_object      Fdiary    (const Octave_object& args, int nargout) ; static builtin_function     Sdiary   = {   "diary"  ,     -1  ,     1  ,   1 ,     Fdiary  ,    
  "diary [on|off]\ndiary [file]\n\nredirect all input and screen output to a file."   }; install_builtin_function (&    Sdiary  );   
 extern Octave_object      Fdisp    (const Octave_object& args, int nargout) ; static builtin_function     Sdisp   = {   "disp"  ,     3  ,     1  ,   0 ,     Fdisp  ,    
  "disp (X): display value without name tag"   }; install_builtin_function (&    Sdisp  );   
 extern Octave_object      Fformat    (const Octave_object& args, int nargout) ; static builtin_function     Sformat   = {   "format"  ,     -1  ,     1  ,   1 ,     Fformat  ,    
  "format [style]\n\nset output formatting style"   }; install_builtin_function (&    Sformat  );   
 extern Octave_object      Fclc    (const Octave_object& args, int nargout) ; static builtin_function     Sclc   = {   "clc"  ,     1  ,     0  ,   0 ,     Fclc  ,    
  "clc (): clear screen"   }; install_builtin_function (&    Sclc  );   
 alias_builtin ("home", "clc");   ;

 extern Octave_object      Fgetenv    (const Octave_object& args, int nargout) ; static builtin_function     Sgetenv   = {   "getenv"  ,     2  ,     1  ,   0 ,     Fgetenv  ,    
  "getenv (STRING): get environment variable values"   }; install_builtin_function (&    Sgetenv  );   
 extern Octave_object      Fkbhit    (const Octave_object& args, int nargout) ; static builtin_function     Skbhit   = {   "kbhit"  ,     1  ,     1  ,   0 ,     Fkbhit  ,    
  "kbhit: get a single character from the terminal"   }; install_builtin_function (&    Skbhit  );   
 extern Octave_object      Fpause    (const Octave_object& args, int nargout) ; static builtin_function     Spause   = {   "pause"  ,     1  ,     1  ,   0 ,     Fpause  ,    
  "pause (seconds): suspend program execution"   }; install_builtin_function (&    Spause  );   
 extern Octave_object      Fclock    (const Octave_object& args, int nargout) ; static builtin_function     Sclock   = {   "clock"  ,     1  ,     0  ,   0 ,     Fclock  ,    
  "clock (): return current date and time in vector with elements\n\n  [ year, month, day-of-month, hour, minute, second ]"   }; install_builtin_function (&    Sclock  );   
 extern Octave_object      Fdate    (const Octave_object& args, int nargout) ; static builtin_function     Sdate   = {   "date"  ,     1  ,     0  ,   0 ,     Fdate  ,    
  "date (): return current date in a string, in the form `18-Jul-94'"   }; install_builtin_function (&    Sdate  );   
 extern Octave_object      Fva_arg    (const Octave_object& args, int nargout) ; static builtin_function     Sva_arg   = {   "va_arg"  ,     1  ,     1  ,   0 ,     Fva_arg  ,    
  "va_arg (): return next argument in a function that takes a\nvariable number of parameters"   }; install_builtin_function (&    Sva_arg  );   
 extern Octave_object      Fva_start    (const Octave_object& args, int nargout) ; static builtin_function     Sva_start   = {   "va_start"  ,     1  ,     0  ,   0 ,     Fva_start  ,    
  "va_start (): reset the pointer to the list of optional arguments\nto the beginning"   }; install_builtin_function (&    Sva_start  );   
 extern Octave_object      Fcloseplot    (const Octave_object& args, int nargout) ; static builtin_function     Scloseplot   = {   "closeplot"  ,     1  ,     0  ,   0 ,     Fcloseplot  ,    
  "closeplot (): close the stream to plotter"   }; install_builtin_function (&    Scloseplot  );   
 extern Octave_object      Fhold    (const Octave_object& args, int nargout) ; static builtin_function     Shold   = {   "hold"  ,     -1  ,     1  ,   1 ,     Fhold  ,    
  "hold [on|off]\n\ndetermine whether the plot window is cleared before the next line is\ndrawn.  With no argument, toggle the current state."   }; install_builtin_function (&    Shold  );    
 extern Octave_object      Fpurge_tmp_files    (const Octave_object& args, int nargout) ; static builtin_function     Spurge_tmp_files   = {   "purge_tmp_files"  ,     5  ,     1  ,   0 ,     Fpurge_tmp_files  ,    
  "delete temporary data files used for plotting"   }; install_builtin_function (&    Spurge_tmp_files  );   
 extern Octave_object      Fset    (const Octave_object& args, int nargout) ; static builtin_function     Sset   = {   "set"  ,     -1  ,     1  ,   1 ,     Fset  ,    
  "set [options]\n\nset plotting options"   }; install_builtin_function (&    Sset  );   
 extern Octave_object      Fshow    (const Octave_object& args, int nargout) ; static builtin_function     Sshow   = {   "show"  ,     -1  ,     1  ,   1 ,     Fshow  ,    
  "show [options]\n\nshow plotting options"   }; install_builtin_function (&    Sshow  );   
 extern Octave_object      Fis_global    (const Octave_object& args, int nargout) ; static builtin_function     Sis_global   = {   "is_global"  ,     2  ,     1  ,   0 ,     Fis_global  ,    
  "is_global (X): return 1 if the string X names a global variable\notherwise, return 0."   }; install_builtin_function (&    Sis_global  );   
 extern Octave_object      Fexist    (const Octave_object& args, int nargout) ; static builtin_function     Sexist   = {   "exist"  ,     2  ,     1  ,   0 ,     Fexist  ,    
  "exist (NAME): check if variable or file exists\n\nreturn 0 if NAME is undefined, 1 if it is a variable, or 2 if it is\na function."   }; install_builtin_function (&    Sexist  );    
 extern Octave_object      Fdocument    (const Octave_object& args, int nargout) ; static builtin_function     Sdocument   = {   "document"  ,     -1  ,     1  ,   1 ,     Fdocument  ,    
  "document symbol string ...\n\nAssociate a cryptic message with a variable name."   }; install_builtin_function (&    Sdocument  );   
 extern Octave_object      Fwho    (const Octave_object& args, int nargout) ; static builtin_function     Swho   = {   "who"  ,     -1  ,     1  ,   1 ,     Fwho  ,    
  "who [-all] [-builtins] [-functions] [-long] [-variables]\n\nList currently defined symbol(s).  Options may be shortened to one\ncharacter, but may not be combined."   }; install_builtin_function (&    Swho  );   
 extern Octave_object      Fwhos    (const Octave_object& args, int nargout) ; static builtin_function     Swhos   = {   "whos"  ,     -1  ,     1  ,   1 ,     Fwhos  ,    
  "whos [-all] [-builtins] [-functions] [-long] [-variables]\n\nList currently defined symbol(s).  Options may be shortened to one\ncharacter, but may not be combined."   }; install_builtin_function (&    Swhos  );   
 extern Octave_object      Fclear    (const Octave_object& args, int nargout) ; static builtin_function     Sclear   = {   "clear"  ,     -1  ,     1  ,   1 ,     Fclear  ,    
  "clear [name ...]\n\nclear symbol(s) matching a list of globbing patterns\nif no arguments are given, clear all user-defined variables and functions"   }; install_builtin_function (&    Sclear  );   
 extern Octave_object      Fbalance    (const Octave_object& args, int nargout) ; static builtin_function     Sbalance   = {   "balance"  ,     4  ,     4  ,   0 ,     Fbalance  ,    
  "AA = balance (A [, OPT]) or [[DD,] AA] =  balance (A [, OPT])\n\ngeneralized eigenvalue problem:\n\n  [cc, dd, aa, bb] = balance (a, b [, opt])\n\nwhere OPT is an optional single character argument as follows: \n\n  N: no balancing; arguments copied, transformation(s) set to identity\n  P: permute argument(s) to isolate eigenvalues where possible\n  S: scale to improve accuracy of computed eigenvalues\n  B: (default) permute and scale, in that order.  Rows/columns\n     of a (and b) that are isolated by permutation are not scaled\n\n[DD, AA] = balance (A, OPT) returns aa = dd\a*dd,\n\n[CC, DD, AA, BB] = balance (A, B, OPT) returns AA (BB) = CC*A*DD (CC*B*DD)"   }; install_builtin_function (&    Sbalance  );   
 extern Octave_object      Fchol    (const Octave_object& args, int nargout) ; static builtin_function     Schol   = {   "chol"  ,     2  ,     1  ,   0 ,     Fchol  ,    
  "R = chol (X): cholesky factorization"   }; install_builtin_function (&    Schol  );   
 extern Octave_object      Fcolloc    (const Octave_object& args, int nargout) ; static builtin_function     Scolloc   = {   "colloc"  ,     7  ,     4  ,   0 ,     Fcolloc  ,    
  "[R, A, B, Q] = colloc (N [, \"left\"] [, \"right\"]): collocation weights"   }; install_builtin_function (&    Scolloc  );   
 extern Octave_object      Fdassl    (const Octave_object& args, int nargout) ; static builtin_function     Sdassl   = {   "dassl"  ,     5  ,     1  ,   0 ,     Fdassl  ,    
  "dassl (\"function_name\", x_0, xdot_0, t_out)\ndassl (F, X_0, XDOT_0, T_OUT, T_CRIT)\n\nThe first argument is the name of the function to call to\ncompute the vector of residuals.  It must have the form\n\n  res = f (x, xdot, t)\n\nwhere x, xdot, and res are vectors, and t is a scalar."   }; install_builtin_function (&    Sdassl  );   
 extern Octave_object      Fdassl_options    (const Octave_object& args, int nargout) ; static builtin_function     Sdassl_options   = {   "dassl_options"  ,     -1  ,     1  ,   0 ,     Fdassl_options  ,    
  "dassl_options (KEYWORD, VALUE)\n\nSet or show options for dassl.  Keywords may be abbreviated\nto the shortest match."   }; install_builtin_function (&    Sdassl_options  );   
 extern Octave_object      Fdet    (const Octave_object& args, int nargout) ; static builtin_function     Sdet   = {   "det"  ,     2  ,     1  ,   0 ,     Fdet  ,    
  "det (X): determinant of a square matrix"   }; install_builtin_function (&    Sdet  );   
 extern Octave_object      Feig    (const Octave_object& args, int nargout) ; static builtin_function     Seig   = {   "eig"  ,     2  ,     1  ,   0 ,     Feig  ,    
  "eig (X) or [V, D] = eig (X): compute eigenvalues and eigenvectors of X"   }; install_builtin_function (&    Seig  );   
 extern Octave_object      Fexpm    (const Octave_object& args, int nargout) ; static builtin_function     Sexpm   = {   "expm"  ,     2  ,     1  ,   0 ,     Fexpm  ,    
  "expm (X): matrix exponential, e^A"   }; install_builtin_function (&    Sexpm  );   
 extern Octave_object      Ffft    (const Octave_object& args, int nargout) ; static builtin_function     Sfft   = {   "fft"  ,     2  ,     1  ,   0 ,     Ffft  ,    
  "fft (X): fast fourier transform of a vector"   }; install_builtin_function (&    Sfft  );   
 extern Octave_object      Ffind    (const Octave_object& args, int nargout) ; static builtin_function     Sfind   = {   "find"  ,     2  ,     3  ,   0 ,     Ffind  ,    
  "find (X) or [I, J, V] = find (X): Return indices of nonzero elements"   }; install_builtin_function (&    Sfind  );   
 extern Octave_object      Ffsolve    (const Octave_object& args, int nargout) ; static builtin_function     Sfsolve   = {   "fsolve"  ,     5  ,     1  ,   0 ,     Ffsolve  ,    
  "Solve nonlinear equations using Minpack.  Usage:\n\n  [X, INFO] = fsolve (F, X0)\n\nWhere the first argument is the name of the  function to call to\ncompute the vector of function values.  It must have the form\n\n  y = f (x)
\nwhere y and x are vectors."   }; install_builtin_function (&    Sfsolve  );   
 extern Octave_object      Ffsolve_options    (const Octave_object& args, int nargout) ; static builtin_function     Sfsolve_options   = {   "fsolve_options"  ,     -1  ,     1  ,   0 ,     Ffsolve_options  ,    
  "fsolve_options (KEYWORD, VALUE)\n\nSet or show options for fsolve.  Keywords may be abbreviated\nto the shortest match."   }; install_builtin_function (&    Sfsolve_options  );   
 extern Octave_object      Ffsqp    (const Octave_object& args, int nargout) ; static builtin_function     Sfsqp   = {   "fsqp"  ,     11  ,     3  ,   0 ,     Ffsqp  ,    
  "[X, PHI] = fsqp (X, PHI [, LB, UB] [, LB, A, UB] [, LB, G, UB])\n\nGroups of arguments surrounded in `[]' are optional, but\nmust appear in the same relative order shown above."   }; install_builtin_function (&    Sfsqp  );   
 extern Octave_object      Ffsqp_options    (const Octave_object& args, int nargout) ; static builtin_function     Sfsqp_options   = {   "fsqp_options"  ,     -1  ,     1  ,   0 ,     Ffsqp_options  ,    
  "fsqp_options (KEYWORD, VALUE)\n\nSet or show options for fsqp.  Keywords may be abbreviated\nto the shortest match."   }; install_builtin_function (&    Sfsqp_options  );   
 extern Octave_object      Fgivens    (const Octave_object& args, int nargout) ; static builtin_function     Sgivens   = {   "givens"  ,     3  ,     2  ,   0 ,     Fgivens  ,    
  "G = givens (X, Y)\n\ncompute orthogonal matrix G = [c s; -conj (s) c]\nsuch that G [x; y] = [*; 0]  (x, y scalars)\n\n[c, s] = givens (x, y) returns the (c, s) values themselves."   }; install_builtin_function (&    Sgivens  );   
 extern Octave_object      Fhess    (const Octave_object& args, int nargout) ; static builtin_function     Shess   = {   "hess"  ,     2  ,     2  ,   0 ,     Fhess  ,    
  "[P, H] = hess (A) or H = hess (A): Hessenberg decomposition"   }; install_builtin_function (&    Shess  );   
 extern Octave_object      Fifft    (const Octave_object& args, int nargout) ; static builtin_function     Sifft   = {   "ifft"  ,    2  ,     1  ,   0 ,     Fifft  ,    
  "ifft (X): inverse fast fourier transform of a vector"   }; install_builtin_function (&    Sifft  );   
 extern Octave_object      Finv    (const Octave_object& args, int nargout) ; static builtin_function     Sinv   = {   "inv"  ,     2  ,     1  ,   0 ,     Finv  ,    
  "inv (X): inverse of a square matrix"   }; install_builtin_function (&    Sinv  );   
 extern Octave_object      Flogm    (const Octave_object& args, int nargout) ; static builtin_function     Slogm   = {   "logm"  ,     2  ,     1  ,   0 ,     Flogm  ,    
  "logm (X): matrix logarithm"   }; install_builtin_function (&    Slogm  );   
 extern Octave_object      Fsqrtm    (const Octave_object& args, int nargout) ; static builtin_function     Ssqrtm   = {   "sqrtm"  ,     2  ,     1  ,   0 ,     Fsqrtm  ,    
 "sqrtm (X): matrix sqrt"   }; install_builtin_function (&    Ssqrtm  );   
 extern Octave_object      Flpsolve    (const Octave_object& args, int nargout) ; static builtin_function     Slpsolve   = {   "lpsolve"  ,     11  ,     3  ,   0 ,     Flpsolve  ,    
  "lp_solve (): solve linear programs using lp_solve."   }; install_builtin_function (&    Slpsolve  );   
 extern Octave_object      Flpsolve_options    (const Octave_object& args, int nargout) ; static builtin_function     Slpsolve_options   = {   "lpsolve_options"  ,     -1  ,     1  ,   0 ,     Flpsolve_options  ,    
  "lp_solve_options (KEYWORD, VALUE)\n\nSet or show options for lp_solve.  Keywords may be abbreviated\nto the shortest match."   }; install_builtin_function (&    Slpsolve_options  );   
 extern Octave_object      Flsode    (const Octave_object& args, int nargout) ; static builtin_function     Slsode   = {   "lsode"  ,     6  ,     1  ,   0 ,     Flsode  ,    
  "lsode (F, X0, T_OUT, T_CRIT)\n\nThe first argument is the name of the function to call to\ncompute the vector of right hand sides.  It must have the form\n\n  xdot = f (x, t)\n\nwhere xdot and x are vectors and t is a scalar.\n"   }; install_builtin_function (&    Slsode  );   
 extern Octave_object      Flsode_options    (const Octave_object& args, int nargout) ; static builtin_function     Slsode_options   = {   "lsode_options"  ,     -1  ,     1  ,   0 ,     Flsode_options  ,    
  "lsode_options (KEYWORD, VALUE)\n\nSet or show options for lsode.  Keywords may be abbreviated\nto the shortest match."   }; install_builtin_function (&    Slsode_options  );   
 extern Octave_object      Flu    (const Octave_object& args, int nargout) ; static builtin_function     Slu   = {   "lu"  ,     2  ,     3  ,   0 ,     Flu  ,    
  "[L, U, P] = lu (A): LU factorization"   }; install_builtin_function (&    Slu  );   
 extern Octave_object      Fmin    (const Octave_object& args, int nargout) ; static builtin_function     Smin   = {   "min"  ,     3  ,     2  ,   0 ,     Fmin  ,    
  "min (X): minimum value(s) of a vector (matrix)"   }; install_builtin_function (&    Smin  );   
 extern Octave_object      Fmax    (const Octave_object& args, int nargout) ; static builtin_function     Smax   = {   "max"  ,     3  ,     2  ,   0 ,     Fmax  ,    
  "max (X): maximum value(s) of a vector (matrix)"   }; install_builtin_function (&    Smax  );   
 extern Octave_object      Fnpsol    (const Octave_object& args, int nargout) ; static builtin_function     Snpsol   = {   "npsol"  ,     11  ,     3  ,   0 ,     Fnpsol  ,    
  "[X, OBJ, INFO, LAMBDA] = npsol (X, PHI [, LB, UB] [, LB, A, UB] [, LB, G, UB])\n\nGroups of arguments surrounded in `[]' are optional, but\nmust appear in the same relative order shown above.\n\nThe second argument is a string containing the name of the objective\nfunction to call.  The objective function must be of the form\n\n  y = phi (x)\n\nwhere x is a vector and y is a scalar.\n\nThe argument G is a string containing the name of the function that
defines the nonlinear constraints.  It must be of the form\n\n  y = g (x)\n\nwhere x is a vector and y is a vector."   }; install_builtin_function (&    Snpsol  );   
 extern Octave_object      Fnpsol_options    (const Octave_object& args, int nargout) ; static builtin_function     Snpsol_options   = {   "npsol_options"  ,     -1  ,     1  ,   0 ,     Fnpsol_options  ,    
  "npsol_options (KEYWORD, VALUE)\n\nSet or show options for npsol.  Keywords may be abbreviated\nto the shortest match."   }; install_builtin_function (&    Snpsol_options  );   
 extern Octave_object      Fqpsol    (const Octave_object& args, int nargout) ; static builtin_function     Sqpsol   = {   "qpsol"  ,     9  ,     3  ,   0 ,     Fqpsol  ,    
  "[X, OBJ, INFO, LAMBDA] = qpsol (X, H, C [, LB, UB] [, LB, A, UB])\n\nGroups of arguments surrounded in `[]' are optional, but\nmust appear in the same relative order shown above."   }; install_builtin_function (&    Sqpsol  );   
 extern Octave_object      Fqpsol_options    (const Octave_object& args, int nargout) ; static builtin_function     Sqpsol_options   = {   "qpsol_options"  ,     -1  ,     1  ,   0 ,     Fqpsol_options  ,    
  "qpsol_options (KEYWORD, VALUE)\n
\nSet or show options for qpsol.  Keywords may be abbreviated\nto the shortest match."   }; install_builtin_function (&    Sqpsol_options  );   
 extern Octave_object      Fqr    (const Octave_object& args, int nargout) ; static builtin_function     Sqr   = {   "qr"  ,     2  ,     2  ,   0 ,     Fqr  ,    
  "[Q, R] = qr (X):      form Q unitary and R upper triangular such\n                       that Q * R = X\n\n[Q, R] = qr (X, 0):    form the economy decomposition such that if X is\n                       if X is m by n then only the first n columns of Q\n                       are computed.\n\n[Q, R, P] = qr (X):    form QRP factorization of X where\n                       P is a permutation matrix such that\n                       A * P = Q * R\n\n[Q, R, P] = qr (X, 0): form the economy decomposition with \n                       permutation vector P such that Q * R = X (:, P)\n\nqr (X) alone returns the output of the LAPACK routine dgeqrf, such\nthat R = triu (qr (X))"   }; install_builtin_function (&    Sqr  );   
 extern Octave_object      Fquad    (const Octave_object& args, int nargout) ; static builtin_function     Squad   = {   "quad"  ,     6  ,     3  ,   0 ,     Fquad  ,    
  "[V, IER, NFUN] = quad (F, A, B [, TOL] [, SING])\n\nWhere the first argument is the name of the  function to call to\ncompute the value of the integrand.  It must have the form\n\n  y = f (x)
\nwhere y and x are scalars.\n\nThe second and third arguments are limits of integration.  Either or\nboth may be infinite.  The optional argument TOL specifies the desired\naccuracy of the result.  The optional argument SING is a vector of\nat which the integrand is singular."   }; install_builtin_function (&    Squad  );   
 extern Octave_object      Fquad_options    (const Octave_object& args, int nargout) ; static builtin_function     Squad_options   = {   "quad_options"  ,     -1  ,     1  ,   0 ,     Fquad_options  ,    
  "quad_options (KEYWORD, VALUE)\n\nSet or show options for quad.  Keywords may be abbreviated\nto the shortest match."   }; install_builtin_function (&    Squad_options  );   
 extern Octave_object      Fqzvalue    (const Octave_object& args, int nargout) ; static builtin_function     Sqzvalue   = {   "qzvalue"  ,     3  ,     1  ,   0 ,     Fqzvalue  ,    
  "X = qzval (A, B)\n\ncompute generalized eigenvalues of the matrix pencil (A - lambda B).\nA and B must be real matrices."   }; install_builtin_function (&    Sqzvalue  );   
 extern Octave_object      Frand    (const Octave_object& args, int nargout) ; static builtin_function     Srand   = {   "rand"  ,     2  ,     1  ,   0 ,     Frand  ,    
  "rand                  -- generate a random value\n\nrand (N)              -- generate N x N matrix\nrand (A)              -- generate matrix the size of A\nrand (N, M)           -- generate N x M matrix\nrand (\"dist\")         -- get current distribution\nrand (DISTRIBUTION)   -- set distribution type (\"normal\" or \"uniform\"\nrand (SEED)           -- get current seed\nrand (SEED, N)        -- set seed"   }; install_builtin_function (&    Srand  );   
 extern Octave_object      Fschur    (const Octave_object& args, int nargout) ; static builtin_function     Sschur   = {   "schur"  ,     3  ,     2  ,   0 ,     Fschur  ,    
  "[U, S] = schur (A) or S = schur (A)\n\nor, for ordered Schur:\n\n  [U, S] = schur (A, TYPE) or S = schur (A, TYPE)\nwhere TYPE is a string that begins with one of the following\ncharacters:\n\n  A = continuous time poles\n  D = discrete time poles\n  U = unordered schur (default)"   }; install_builtin_function (&    Sschur  );   
 extern Octave_object      Fsort    (const Octave_object& args, int nargout) ; static builtin_function     Ssort   = {   "sort"  ,     2  ,     2  ,   0 ,     Fsort  ,    
  "[S, I] = sort (X)\n\nsort the columns of X, optionally return sort index"   }; install_builtin_function (&    Ssort  );   
 extern Octave_object      Fsvd    (const Octave_object& args, int nargout) ; static builtin_function     Ssvd   = {   "svd"  ,     2  ,     3  ,   0 ,     Fsvd  ,    
  "S = svd (X) or [U, S, V] = svd (X [, 0])\n\nCompute the singular value decomposition of X.  Given a second input\nargument, an `economy' sized factorization is computed that omits\nunnecessary rows and columns of U and V"   }; install_builtin_function (&    Ssvd  );   
 extern Octave_object      Fsyl    (const Octave_object& args, int nargout) ; static builtin_function     Ssyl   = {   "syl"  ,     4  ,     1  ,   0 ,     Fsyl  ,    
  "X = syl (A, B, C): solve the Sylvester equation A X + X B + C = 0"   }; install_builtin_function (&    Ssyl  );   
}

void
install_builtins (void)
{
  install_builtin_variables ();
  install_mapper_functions ();
  install_builtin_functions ();
}
