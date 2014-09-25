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
 * Get a video frame mode.
 */
freenect_frame_mode* find_video_mode_freenect(uint32_t res,
                                              uint32_t fmt){
  freenect_frame_mode* mode = malloc(sizeof (*mode));
  *mode = freenect_find_video_mode(res,fmt);
  return mode;
};

/*
 * Set the video mode of a device.
 */
int set_freenect_video_mode(freenect_device* dev,freenect_frame_mode* mode)
{
  return freenect_set_video_mode(dev,*mode);
}; 

/*
 * Get the video resolution of a device.
 */
uint32_t get_freenect_video_resolution(freenect_device* dev){
  freenect_frame_mode mode = freenect_get_current_video_mode(dev);
  return mode.resolution;
}

/*
 * Get a depth frame mode.
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

/*
 * Get the depth resolution of a device.
 */
uint32_t get_freenect_depth_resolution(freenect_device* dev){
  freenect_frame_mode mode = freenect_get_current_depth_mode(dev);
  return mode.resolution;
}


int process_events_timeout(freenect_context* ctx, int microseconds)
{
    struct timeval timeout;
    
    timeout.tv_sec = microseconds / 1000000;
    timeout.tv_usec = microseconds - timeout.tv_sec * 1000000;

    return freenect_process_events_timeout(ctx, &timeout);

}

