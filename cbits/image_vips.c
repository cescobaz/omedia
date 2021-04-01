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
typedef struct Metadata {
  char * dateTimeOriginal ;
  char * subSecTimeOriginal ;
  char * offsetTimeOriginal ;
  char * dateTime ;
  char * offsetTime ;
  char * subSecTime ;
  char * dateTimeDigitized ;
  char * subSecTimeDigitized ;
  char * offsetTimeDigitized ;
  int orientation ;
  char * uniqueCameraModel ;
  char * localizedCameraModel ;
  char * model ;
  double * gpsLatitude ;
  char * gpsLatitudeRef ;
  double * gpsLongitude ;
  char * gpsLongitudeRef ;
  double gpsAltitude ;
  char * gpsAltitudeRef;
} Metadata;

int exif_string(VipsImage *image, const char *name, char ** out) {
  GValue value = { 0 };
  int result = vips_image_get (image, name, &value);
  if (result != 0) {
    printf("vips_image_get error %d", result);
    return result;
  }
  if (G_VALUE_TYPE (&value) != G_TYPE_STRING) {
    printf("G_VALUE_TYPE error %lu != %lu\n", G_VALUE_TYPE(&value), G_TYPE_DOUBLE);
    //g_value_unset (&value);
  }
  size_t sValueLength = 0;
  const char * sValue = vips_value_get_ref_string(&value, &sValueLength);
  *out = malloc(sValueLength + 1);
  memcpy(*out, sValue, sValueLength + 1);
  printf("%s = %s (%zu)\n", name, sValue, sValueLength);
  g_value_unset (&value);
  return 0;
}
int exif(char *filename) {
  VipsImage *image = vips_image_new_from_file(filename, NULL);
  const char *name = "exif-ifd2-DateTimeOriginal";
  char * sValue = NULL;
  int result = exif_string(image, name, &sValue);
  if (result != 0) {
    printf("exif_string error %d\n", result);
    g_object_unref(image);
    return result;
  }
  printf("%s = %s\n", name, sValue);
  g_object_unref(image);
  return 0;
}
int main(int argc, char ** argv) {
  VIPS_INIT("");
  char * filename = "/Users/cescobaz/omedia/to-import/E399459F-476E-4CAB-95E6-60BC20E26D12.heic";
  int result = exif(filename);
  printf("exif %d\n", result);
  return thumbnail(filename, "prova.jpg", 256);
}


