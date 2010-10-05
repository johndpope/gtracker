#ifndef __LOG_H
#define __LOG_H

#define INFO(msg...)       log_it("   info", msg)
#define WARNING(msg...)    log_it("warning", msg)

#ifndef FIND_ME_NO_DEBUG
#  define DEBUG(msg...)      log_it("  debug", msg)
#else
#  define DEBUG(msg...)
#endif

#ifdef __cplusplus
extern "C" {
#endif

void log_it(const char * prefix, const char * fmt, ...) __attribute__ ((__format__(__printf__, 2, 3)));

#ifdef __cplusplus
}
#endif 

#endif /* __LOG_H */
