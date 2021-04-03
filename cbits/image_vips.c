#include <vips/vips.h>
#include <vips/resample.h>

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

// https://exiftool.org/TagNames/EXIF.html
typedef struct Metadata {
  char *dateTimeOriginal;
  char *subSecTimeOriginal;
  char *offsetTimeOriginal;
  char *dateTime;
  char *offsetTime;
  char *subSecTime;
  char *dateTimeDigitized;
  char *subSecTimeDigitized;
  char *offsetTimeDigitized;
  int orientation;
  char *uniqueCameraModel;
  char *localizedCameraModel;
  char *model;
  double *gpsLatitude;
  char *gpsLatitudeRef;
  double *gpsLongitude;
  char *gpsLongitudeRef;
  double gpsAltitude;
  char *gpsAltitudeRef;
} Metadata;

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

  g_object_unref(image);
  return 0;
}
int main(int argc, char **argv) {
  VIPS_INIT("");
  char *filename = "/Users/cescobaz/omedia/to-import/"
                   "E399459F-476E-4CAB-95E6-60BC20E26D12.heic";
  int result = exif(filename, NULL);
  printf("exif %d\n", result);
  return thumbnail(filename, "prova.jpg", 256);
}
