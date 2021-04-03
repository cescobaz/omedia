#include <vips/vips.h>
#include <vips/resample.h>
#include "oimage.h"

void init() { VIPS_INIT(""); }

int thumbnail(char *inputFilename, char *outputFilename, int maxSize) {
  VipsImage *out = NULL;
  int result = vips_thumbnail(inputFilename, &out, maxSize, NULL);
  if (result != 0) {
    return result;
  }
  result = vips_image_write_to_file(out, outputFilename, NULL);
  if (result != 0) {
    g_object_unref(out);
    return result;
  }
  g_object_unref(out);
  return 0;
}

int exif_string(VipsImage *image, const char *name, char **out) {
  GValue value = {0};
  int result = vips_image_get(image, name, &value);
  if (result != 0) {
    printf("vips_image_get error %d", result);
    return result;
  }
  size_t sValueLength = 0;
  const char *sValue = vips_value_get_ref_string(&value, &sValueLength);
  if (!sValue) {
    return -1;
  }
  *out = malloc(sValueLength + 1);
  memcpy(*out, sValue, sValueLength + 1);
  g_value_unset(&value);
  return 0;
}
int exif(char *filename, Metadata **out) {
  Metadata *metadata = calloc(1, sizeof(Metadata));
  VipsImage *image = vips_image_new_from_file(filename, NULL);

  const char *name = "exif-ifd2-DateTimeOriginal";
  exif_string(image, name, (char **)&(metadata->dateTimeOriginal));
  printf("%s = %s\n", name, metadata->dateTimeOriginal);
  name = "exif-ifd2-SubSecTimeOriginal";
  exif_string(image, name, (char **)&(metadata->subSecTimeOriginal));
  printf("%s = %s\n", name, metadata->subSecTimeOriginal);

  g_object_unref(image);
  return 0;
}
