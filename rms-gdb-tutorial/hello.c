#include <stdio.h>

/* RMS's gdb tutorial */
/* http://unknownroad.com/rtfm/gdbtut/gdbuse.html */


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


   (gdb) print i
   (gdb) set var i = 0

   (gdb) backtrace

   (gdb) frame 7
   (gdb) info frame

   (gdb) info locals

   (gdb) info args



 */

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
