#include <stdio.h>
#include <stdlib.h>

#include <libfreenect.h>

/*
 * Make a new freenect_context.
 */
freenect_context** new_freenect_context(){
  freenect_context** ptr = malloc(sizeof (*ptr));
  *ptr = malloc(sizeof (freenect_context*));
  return ptr;
};

/*
 * Make a new freenect_device.
 */
freenect_device** new_freenect_device(){
  return malloc(sizeof (freenect_device*));
};

/* int main() */
/* { */
/*   freenect_context** ctx = new_freenect_context(); */
/*   freenect_init(ctx,NULL); */
/*   freenect_device** dev = new_freenect_device(); */
/*   freenect_open_device(*ctx,dev,0); */
/*   freenect_close_device(*dev); */
/*   freenect_shutdown(*ctx); */
/*   printf("OK.\n"); */
/* } */
