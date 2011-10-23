#include <stdio.h>
#include <stdlib.h>

#include <libfreenect.h>

/*
 * Make a new freenect_context.
 */
freenect_context** new_freenect_context(){
  freenect_context** ptr = malloc(sizeof (*ptr));
  *ptr = malloc(sizeof(*ptr));
  return ptr;
};

/*
 * Make a new freenect_device.
 */
freenect_device** new_freenect_device(){
  return malloc(sizeof (freenect_device*));
};

/*
 * Get a frame mode.
 */
freenect_frame_mode* find_depth_mode_freenect(uint32_t res,
                                              uint32_t fmt){
  freenect_frame_mode* mode = malloc(sizeof (*mode));
  *mode = freenect_find_depth_mode(res,fmt);
  return mode;
};

/*
 * Set the depth mode of a device.
 */
int set_freenect_depth_mode(freenect_device* dev,freenect_frame_mode* mode)
{
  return freenect_set_depth_mode(dev,*mode);
};
