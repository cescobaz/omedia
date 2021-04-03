#include <vips/vips.h>
#include <vips/resample.h>
#include "oimage.h"

void init() { VIPS_INIT(""); }

int thumbnail(char *inputFilename, char *outputFilename, int maxSize,
              int *width, int *height) {
  VipsImage *out = NULL;
  int result = vips_thumbnail(inputFilename, &out, maxSize, NULL);
  if (result != 0) {
    return result;
  }
  if (width) {
    *width = vips_image_get_width(out);
  }
  if (height) {
    *height = vips_image_get_height(out);
  }
  result = vips_image_write_to_file(out, outputFilename, NULL);
  if (result != 0) {
    g_object_unref(out);
    return result;
  }
  g_object_unref(out);
  return 0;
}
