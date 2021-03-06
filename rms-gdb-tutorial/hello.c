#include <stdio.h>

/* RMS's gdb tutorial */
/* http://unknownroad.com/rtfm/gdbtut/gdbuse.html */




int main(int argc, char**argv){
  if (argc==2){
    char* arg1=argv[1];
    printf("hello %s\n", arg1);
  } else {
    int i;
  begin:
    
    printf("hello world (%d)\n", i);
    i++;
    goto begin;
  }
  return 0;
}


/* cc hello.c -o hello
   ./hello
   gdb hello

   gdb complains about missing debug symbols
   
   gcc -g hello.c -o hello
   gdb hello

   gdb complains about missing debuginfos, and suggests (on fedora)

   sudo dnf debuginfo-install glibc-2.23.1-11.fc24.x86_64
   installs:
   glibc-debuginfo.x86_64 2.23.1-11.fc24
   glibc-debuginfo-common.x86_64 2.23.1-11.fc24
   nss-softokn-debuginfo.x86_64 3.30.2-1.0.fc24

   gdb hello 
   now starts without complaints
 
   (gdb) run
   (gdb) run banana
   (gdb) kill
   (gdb) help
   (gdb) quit

   stopping program
   (gdb) run
   C-c
   (gdb) list
   (gdb) next               ; next line
   (gdb) step               ; into
   (gdb) up
   (gdb) finish

   (gdb) print i
   (gdb) set var i = 0

   (gdb) backtrace

   (gdb) frame 7
   (gdb) info frame

   (gdb) info locals

   (gdb) info args

   ; breakpoints

   (gdb) break main
   (gdb) run

 */

/**
   Can run gdb from emacs using M-x gdb
   M-x gdb-many-windows brings up local-variables type windows
 */

/**
   Can run gdb from emacs using realgud
   https://github.com/realgud/realgud
   M-x package-install RET realgud RET
   M-x load-library RET realgud RET
   M-x realgud:gdb
 */
