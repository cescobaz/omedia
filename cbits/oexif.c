#include <vips/vips.h>
#include <libexif/exif-data.h>
#include <stdint.h>
#include "oexif.h"

uint16_t reverse16(uint16_t value) {
  return (((value << 8) & 0xFF00) | ((value >> 8) & 0x00FF));
}

uint32_t reverse32(uint32_t value) {
  return ((value >> 24) & 0x000000FF) | ((value >> 8) & 0x0000FF00) |
         ((value << 8) & 0x00FF0000) | ((value << 24) & 0xFF000000);
}
uint64_t reverse64(uint64_t value) {
  return (((value >> 56) & 0x00000000000000FF) |
          ((value >> 40) & 0x000000000000FF00) |
          ((value >> 24) & 0x0000000000FF0000) |
          ((value >> 8) & 0x00000000FF000000) |
          ((value << 8) & 0x000000FF00000000) |
          ((value << 24) & 0x0000FF0000000000) |
          ((value << 40) & 0x00FF000000000000) |
          ((value << 56) & 0xFF00000000000000));
}
int stringEntry(ExifContent *content, uint tag, char **out) {
  if (out == NULL) {
    return -1;
  }
  ExifEntry *entry = exif_content_get_entry(content, tag);
  if (entry == NULL) {
    return -1;
  }
  *out = calloc(1, entry->size + 1);
  memcpy(*out, entry->data, entry->size);
  return 0;
}
int anyEntry(ExifContent *content, uint tag, void **out, size_t size, int count,
             char byteOrder, int *countOut) {
  if (out == NULL) {
    return -1;
  }
  ExifEntry *entry = exif_content_get_entry(content, tag);
  if (entry == NULL) {
    return -1;
  }
  *out = calloc(count, size);
  size_t copySize = MIN(entry->size, count * size);
  if (countOut) {
    *countOut = copySize / count;
  }
  memcpy(*out, entry->data, copySize);
  if (byteOrder == 'l' || size == 1) {
    return 0;
  }
  uint8_t *ptr8 = *out;
  uint16_t *ptr16;
  uint32_t *ptr32;
  uint64_t *ptr64;
  for (int i = 0; i < count; ++i) {
    switch (size) {
    case 2:
      ptr16 = (uint16_t *)ptr8;
      *ptr16 = reverse16(*ptr16);
      // printf("0x%04x[%d] = %u (reverse16)\n", tag, i, *ptr16);
      break;
    case 4:
      ptr32 = (uint32_t *)ptr8;
      *ptr32 = reverse32(*ptr32);
      // printf("0x%04x[%d] = %u (0x%08x)(reverse32)\n", tag, i, *ptr32,
      // *(uint32_t *)ptr32);
      break;
    case 8:
      ptr64 = (uint64_t *)ptr8;
      *ptr64 = reverse64(*ptr64);
      // printf("0x%04x[%d] = %llu\n", tag, i, *ptr64);
      break;
    default:
      break;
    }
    ptr8 += size;
  }
  return 0;
}
// rational is 2 long: numerator and denominator
// https://www.exif.org/Exif2-2.PDF
int rationalEntryToDouble(ExifContent *content, uint tag, double **out,
                          int count, char byteOrder, int *countOut) {
  if (out == NULL) {
    return -1;
  }
  int longCount = count * 2;
  size_t longSize = sizeof(uint32_t);
  uint32_t *buffer;
  int realLongCount;
  int result = anyEntry(content, tag, (void **)&buffer, longSize, longCount,
                        byteOrder, &realLongCount);
  if (result != 0) {
    return result;
  }
  if (realLongCount < 2) {
    free(buffer);
    return -1;
  }
  int realCount = realLongCount / 2;
  *out = calloc(realCount, sizeof(double));
  if (countOut) {
    *countOut = realCount;
  }
  uint32_t *ptr = buffer;
  for (int i = 0; i < realCount; ++i) {
    uint32_t n = *ptr;
    ptr += 1;
    uint32_t d = *ptr;
    ptr += 1;
    (*out)[i] = (double)n / (double)d;
    // printf("%u / %u = %f \n", n, d, (*out)[i]);
  }
  free(buffer);
  return 0;
}
void content(ExifContent *content, Metadata *data) {
  ExifIfd ifd = exif_content_get_ifd(content);
  if (ifd == EXIF_IFD_COUNT) {
    return;
  }
  // https://exiftool.org/TagNames/EXIF.html
  switch (ifd) {
  case 0: {
    stringEntry(content, 0x0132, &(data->dateTime));
    anyEntry(content, 0x0112, (void **)&(data->orientation), sizeof(uint16_t),
             1, data->byteOrder, NULL);
    stringEntry(content, 0xc614, &(data->uniqueCameraModel));
    stringEntry(content, 0xc615, &(data->localizedCameraModel));
    stringEntry(content, 0x0110, &(data->model));
  } break;
  case 1: {
  } break;
  case 2: {
    stringEntry(content, 0x9290, &(data->subSecTime));
    stringEntry(content, 0x9010, &(data->offsetTime));
    stringEntry(content, 0x9003, &(data->dateTimeOriginal));
    stringEntry(content, 0x9291, &(data->subSecTimeOriginal));
    stringEntry(content, 0x9011, &(data->offsetTimeOriginal));
    stringEntry(content, 0x9004, &(data->dateTimeDigitized));
    stringEntry(content, 0x9292, &(data->subSecTimeDigitized));
    stringEntry(content, 0x9012, &(data->offsetTimeDigitized));
  } break;
  case 3: {
    rationalEntryToDouble(content, 0x0002, &(data->gpsLatitude), 3,
                          data->byteOrder, &(data->gpsLatitudeCount));
    stringEntry(content, 0x0003, &(data->gpsLongitudeRef));
    rationalEntryToDouble(content, 0x0004, &(data->gpsLongitude), 3,
                          data->byteOrder, &(data->gpsLongitudeCount));
    anyEntry(content, 0x0005, (void **)&(data->gpsAltitudeRef), sizeof(uint8_t),
             1, data->byteOrder, NULL);
    rationalEntryToDouble(content, 0x0006, &(data->gpsAltitude), 1,
                          data->byteOrder, NULL);
  } break;
  default:
    break;
  }
}

int exif(char *filename, Metadata **out) {
  VipsImage *image = vips_image_new_from_file(filename, NULL);
  if (image == NULL) {
    return -1;
  }
  if (vips_image_get_typeof(image, "exif-data") != VIPS_TYPE_BLOB) {
    g_object_unref(image);
    return -1;
  }
  const void *exifDataRaw;
  size_t exifDataRawLength;
  int result =
      vips_image_get_blob(image, "exif-data", &exifDataRaw, &exifDataRawLength);
  if (result != 0) {
    g_object_unref(image);
    return -1;
  }
  ExifData *exifData = exif_data_new();
  if (exifData == NULL) {
    g_object_unref(image);
    return -1;
  }
  exif_data_unset_option(exifData, EXIF_DATA_OPTION_IGNORE_UNKNOWN_TAGS);
  exif_data_load_data(exifData, exifDataRaw, exifDataRawLength);
  Metadata *metadata = calloc(1, sizeof(Metadata));
  if (metadata == NULL) {
    exif_data_unref(exifData);
    g_object_unref(image);
    return -1;
  }
  *out = metadata;
  ExifByteOrder byteOrder = exif_data_get_byte_order(exifData);
  if (byteOrder == EXIF_BYTE_ORDER_MOTOROLA) {
    metadata->byteOrder = 'b';
  } else {
    metadata->byteOrder = 'l';
  }
  exif_data_foreach_content(exifData, (ExifDataForeachContentFunc)&content,
                            metadata);

  exif_data_unref(exifData);
  g_object_unref(image);
  return 0;
}
void free_exif(Metadata **out) {
  free(*out);
  *out = NULL;
}

void print_exif_metadata(Metadata *metadata) {
  if (metadata->byteOrder) {
    printf("byteOrder = %c\n", metadata->byteOrder);
  }
  if (metadata->dateTime) {
    printf("dateTime = %s\n", metadata->dateTime);
  }
  if (metadata->subSecTime) {
    printf("subSecTime = %s\n", metadata->subSecTime);
  }
  if (metadata->offsetTime) {
    printf("offsetTime = %s\n", metadata->offsetTime);
  }
  if (metadata->orientation) {
    printf("orientation = %d\n", *metadata->orientation);
  }
  if (metadata->uniqueCameraModel) {
    printf("uniqueCameraModel = %s\n", metadata->uniqueCameraModel);
  }
  if (metadata->localizedCameraModel) {
    printf("localizedCameraModel = %s\n", metadata->localizedCameraModel);
  }
  if (metadata->model) {
    printf("model = %s\n", metadata->model);
  }
  if (metadata->dateTimeOriginal) {
    printf("dateTimeOriginal = %s\n", metadata->dateTimeOriginal);
  }
  if (metadata->subSecTimeOriginal) {
    printf("subSecTimeOriginal = %s\n", metadata->subSecTimeOriginal);
  }
  if (metadata->offsetTimeOriginal) {
    printf("offsetTimeOriginal = %s\n", metadata->offsetTimeOriginal);
  }
  if (metadata->dateTimeDigitized) {
    printf("dateTimeDigitized = %s\n", metadata->dateTimeDigitized);
  }
  if (metadata->subSecTimeDigitized) {
    printf("subSecTimeDigitized = %s\n", metadata->subSecTimeDigitized);
  }
  if (metadata->offsetTimeDigitized) {
    printf("offsetTimeDigitized = %s\n", metadata->offsetTimeDigitized);
  }
  if (metadata->gpsLatitude) {
    printf("gpsLatitude = [%f, %f, %f]\n", metadata->gpsLatitude[0],
           metadata->gpsLatitude[1], metadata->gpsLatitude[2]);
  }
  if (metadata->gpsLatitudeCount) {
    printf("gpsLatitudeCount = %d\n", metadata->gpsLatitudeCount);
  }
  if (metadata->gpsLatitudeRef) {
    printf("gpsLatitudeRef = %s\n", metadata->gpsLatitudeRef);
  }
  if (metadata->gpsLongitude) {
    printf("gpsLongitude = [%f, %f, %f]\n", metadata->gpsLongitude[0],
           metadata->gpsLongitude[1], metadata->gpsLongitude[2]);
  }
  if (metadata->gpsLongitudeCount) {
    printf("gpsLongitudeCount = %d\n", metadata->gpsLongitudeCount);
  }
  if (metadata->gpsLongitudeRef) {
    printf("gpsLongitudeRef = %s\n", metadata->gpsLongitudeRef);
  }
  if (metadata->gpsAltitude) {
    printf("gpsAltitude = %f\n", *metadata->gpsAltitude);
  }
  if (metadata->gpsAltitudeRef) {
    printf("gpsAltitudeRef = %s\n", metadata->gpsAltitudeRef);
  }
}
