#include <libfreenect.h>

freenect_context* new_freenect_context();
freenect_context* init_freenect_context(freenect_context **c);
int               process_events_timeout(freenect_context *c, int microseconds);
