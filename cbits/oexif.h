#include <stdint.h>

typedef struct Metadata {
  char byteOrder;
  char *dateTime;
  char *subSecTime;
  char *offsetTime;
  uint16_t *orientation;
  char *uniqueCameraModel;
  char *localizedCameraModel;
  char *model;
  char *dateTimeOriginal;
  char *subSecTimeOriginal;
  char *offsetTimeOriginal;
  char *dateTimeDigitized;
  char *subSecTimeDigitized;
  char *offsetTimeDigitized;
  double *gpsLatitude;
  int gpsLatitudeCount;
  char *gpsLatitudeRef;
  double *gpsLongitude;
  int gpsLongitudeCount;
  char *gpsLongitudeRef;
  double *gpsAltitude;
  int8_t *gpsAltitudeRef;
} Metadata;

int exif_read_from_file(char *filename, Metadata **out);

void exif_free(Metadata **metadata);

void exif_print_metadata(Metadata *metadata);
