#include "log.h"
#include <stdio.h>
#include <stdarg.h>

void log_it(const char * prefix, const char * fmt, ...)
{
   fprintf(stderr, "%s: ", prefix);

   va_list args;
   va_start(args, fmt);
   vfprintf(stderr, fmt, args);
   va_end(args);

   fprintf(stderr, "\n");
}
