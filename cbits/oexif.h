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
  char *gpsLatitudeRef;
  double *gpsLongitude;
  char *gpsLongitudeRef;
  double *gpsAltitude;
  int8_t *gpsAltitudeRef;
} Metadata;

int exif(char *filename, Metadata **out);

void free_exif(Metadata **metadata);
