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
  int *orientation;
  char *uniqueCameraModel;
  char *localizedCameraModel;
  char *model;
  char *gpsAltitude;
  char *gpsAltitudeRef;
  char *gpsLatitude;
  char *gpsLatitudeRef;
  char *gpsLongitude;
  char *gpsLongitudeRef;
} Metadata;

int exif(char *filename, Metadata *metadata);
