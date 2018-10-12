#include<stdarg.h>
#include<stdio.h>
#include"debug.h"

void debug_vprintf(const char *fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
}
