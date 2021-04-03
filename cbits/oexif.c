#include <vips/vips.h>
#include "oexif.h"

int exif(char *filename, Metadata *metadata) {
  VipsImage *image = vips_image_new_from_file(filename, NULL);

  vips_image_get_as_string(image, "exif-ifd2-DateTimeOriginal",
                           &(metadata->dateTimeOriginal));
  vips_image_get_as_string(image, "exif-ifd2-SubSecTimeOriginal",
                           &(metadata->subSecTimeOriginal));
  vips_image_get_as_string(image, "exif-ifd2-OffsetTimeOriginal",
                           &(metadata->offsetTimeOriginal));
  vips_image_get_as_string(image, "exif-ifd2-ModifyDate",
                           &(metadata->dateTime));
  vips_image_get_as_string(image, "exif-ifd2-ModifyDate",
                           &(metadata->dateTime));
  vips_image_get_as_string(image, "exif-ifd2-SubSecTime",
                           &(metadata->subSecTime));
  vips_image_get_as_string(image, "exif-ifd2-OffsetTime",
                           &(metadata->offsetTime));
  vips_image_get_as_string(image, "exif-ifd2-DateTimeDigitized",
                           &(metadata->dateTimeDigitized));
  vips_image_get_as_string(image, "exif-ifd2-SubSecTimeDigitized",
                           &(metadata->subSecTimeDigitized));
  vips_image_get_as_string(image, "exif-ifd2-OffsetTimeDigitized",
                           &(metadata->offsetTimeDigitized));
  if (vips_image_get_typeof(image, "exif-ifd0-Orientation")) {
    metadata->orientation = malloc(sizeof(int));
    *(metadata->orientation) = vips_image_get_orientation(image);
  }
  vips_image_get_as_string(image, "exif-ifd0-UniqueCameraModel",
                           &(metadata->uniqueCameraModel));
  vips_image_get_as_string(image, "exif-ifd0-LocalizedCameraModel",
                           &(metadata->localizedCameraModel));
  vips_image_get_as_string(image, "exif-ifd0-Model", &(metadata->model));

  vips_image_get_as_string(image, "exif-ifd3-GPSAltitude",
                           &(metadata->gpsAltitude));
  vips_image_get_as_string(image, "exif-ifd3-GPSAltitudeRef",
                           &(metadata->gpsAltitudeRef));
  vips_image_get_as_string(image, "exif-ifd3-GPSLatitude",
                           &(metadata->gpsLatitude));
  vips_image_get_as_string(image, "exif-ifd3-GPSLatitudeRef",
                           &(metadata->gpsLatitudeRef));
  vips_image_get_as_string(image, "exif-ifd3-GPSLongitude",
                           &(metadata->gpsLongitude));
  vips_image_get_as_string(image, "exif-ifd3-GPSLongitudeRef",
                           &(metadata->gpsLongitudeRef));

  g_object_unref(image);
  return 0;
}
