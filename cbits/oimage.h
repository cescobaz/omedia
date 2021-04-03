void init();

int thumbnail(char *inputFilename, char *outputFilename, int maxSize,
              int *width, int *height);

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

int exif(char *filename, Metadata **out);
