#include <vips/vips.h>
#include <libexif/exif-data.h>
#include "oexif.h"

#define COUNT 19

// https://exiftool.org/TagNames/EXIF.html
static const char *DateTimeOriginal = "exif-ifd2-DateTimeOriginal";
static const char *SubSecTimeOriginal = "exif-ifd2-SubSecTimeOriginal";
static const char *OffsetTimeOriginal = "exif-ifd2-OffsetTimeOriginal";
static const char *ModifyDate = "exif-ifd0-DateTime";
static const char *SubSecTime = "exif-ifd0-SubSecTime";
static const char *OffsetTime = "exif-ifd0-OffsetTime";
static const char *DateTimeDigitized = "exif-ifd2-DateTimeDigitized";
static const char *SubSecTimeDigitized = "exif-ifd2-SubSecTimeDigitized";
static const char *OffsetTimeDigitized = "exif-ifd2-OffsetTimeDigitized";
static const char *Orientation = "exif-ifd0-Orientation";
static const char *UniqueCameraModel = "exif-ifd0-UniqueCameraModel";
static const char *LocalizedCameraModel = "exif-ifd0-LocalizedCameraModel";
static const char *Model = "exif-ifd0-Model";
static const char *GPSLatitude = "exif-ifd3-GPSLatitude";
static const char *GPSLatitudeRef = "exif-ifd3-GPSLatitudeRef";
static const char *GPSLongitude = "exif-ifd3-GPSLongitude";
static const char *GPSLongitudeRef = "exif-ifd3-GPSLongitudeRef";
static const char *GPSAltitude = "exif-ifd3-GPSAltitude";
static const char *GPSAltitudeRef = "exif-ifd3-GPSAltitudeRef";

int exif(char *filename, void ***out, int *count) {
  void **metadata = calloc(COUNT * 2, sizeof(void *));
  if (!metadata) {
    return -1;
  }
  VipsImage *image = vips_image_new_from_file(filename, NULL);
  if (!image) {
    free(metadata);
    return -1;
  }
  *count = COUNT;
  *out = metadata;

  metadata[0] = (void *)DateTimeOriginal;
  metadata[2] = (void *)SubSecTimeOriginal;
  metadata[4] = (void *)OffsetTimeOriginal;
  metadata[6] = (void *)ModifyDate;
  metadata[8] = (void *)SubSecTime;
  metadata[10] = (void *)OffsetTime;
  metadata[12] = (void *)DateTimeDigitized;
  metadata[14] = (void *)SubSecTimeDigitized;
  metadata[16] = (void *)OffsetTimeDigitized;
  metadata[18] = (void *)UniqueCameraModel;
  metadata[20] = (void *)LocalizedCameraModel;
  metadata[22] = (void *)Model;
  metadata[24] = (void *)GPSAltitude;
  metadata[26] = (void *)GPSAltitudeRef;
  metadata[28] = (void *)GPSLatitude;
  metadata[30] = (void *)GPSLatitudeRef;
  metadata[32] = (void *)GPSLongitude;
  metadata[34] = (void *)GPSLongitudeRef;
  metadata[36] = (void *)Orientation;
  int max = COUNT * 2;
  for (int i = 0; i < max; i += 2) {
    const char *name = metadata[i];
    char *value;
    int result = vips_image_get_as_string(image, name, &value);
    if (result == 0) {
      metadata[i + 1] = value;
    }
  }

  g_object_unref(image);
  return 0;
}
void free_exif(void ***out, int count) {
  void **headers = *out;
  int max = count * 2;
  for (int i = 1; i < max; i += 2) {
    void *ptr = headers[i];
    if (ptr) {
      g_free(ptr);
    }
  }
  free(*out);
  *out = NULL;
}
