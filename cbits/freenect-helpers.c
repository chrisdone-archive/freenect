#include <stdio.h>
#include <stdlib.h>

#include <libfreenect.h>

/*
 * Make a new freenect_context.
 */
freenect_context* new_freenect_context(){
  return malloc(sizeof (freenect_context*));
};

/*
 * Make a new freenect_device.
 */
freenect_device* new_freenect_device(){
  return malloc(sizeof (freenect_device*));
};

/*
 * Initialize a context.
 */
int init_freenect_context(freenect_context *ctx){
  return freenect_init(&ctx,NULL);
};

/*
 * Open a device.
 */
int open_freenect_device(freenect_context *ctx,freenect_device *dev,int i){
  return freenect_open_device(ctx,&dev,i);
};
