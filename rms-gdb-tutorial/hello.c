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

 */

int main(int argc, char**argv){
  if (argc==2){
    char* arg1=argv[1];
    printf("hello %s", arg1);
  } else {
    printf("hello world\n");
  }
  return 0;
}
