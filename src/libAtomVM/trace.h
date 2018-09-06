#ifndef _TRACE_H_
#define _TRACE_H_

#ifndef TRACE
    #ifdef ENABLE_TRACE
        #define TRACE printf
        #define DEBUG_FAIL_NULL(expr) assert((expr) != NULL)
    #else
        #define TRACE(...)
        #define DEBUG_FAIL_NULL(expr)
    #endif
#endif

#define USED_BY_TRACE(x) \
    (void) (x)

#endif
