#include <vips/vips.h>
#include <libexif/exif-data.h>
#include <stdint.h>
#include "oexif.h"

uint16_t reverse16(uint16_t value) {
  return (((value & 0x00FF) << 8) | ((value & 0xFF00) >> 8));
}

uint32_t reverse32(uint32_t value) {
  return ((value >> 24) & 0x000000FF) | ((value >> 8) & 0x0000FF00) |
         ((value << 8) & 0x00FF0000) | ((value << 24) & 0xFF000000);
}
uint64_t reverse64(uint64_t value) {
  return (((value & 0x00000000000000FF) << 56) |
          ((value & 0x000000000000FF00) << 40) |
          ((value & 0x0000000000FF0000) << 24) |
          ((value & 0x00000000FF000000) << 8) |
          ((value & 0x000000FF00000000) >> 8) |
          ((value & 0x0000FF0000000000) >> 24) |
          ((value & 0x00FF000000000000) >> 40) |
          ((value & 0xFF00000000000000) >> 56));
}
void stringEntry(ExifContent *content, uint tag, char **out) {
  if (!out) {
    return;
  }
  ExifEntry *entry = exif_content_get_entry(content, tag);
  if (entry == NULL) {
    return;
  }
  *out = calloc(1, entry->size + 1);
  memcpy(*out, entry->data, entry->size);
}
void anyEntry(ExifContent *content, uint tag, void **out, size_t size,
              int count, char byteOrder) {
  if (!out) {
    return;
  }
  ExifEntry *entry = exif_content_get_entry(content, tag);
  if (entry == NULL) {
    return;
  }
  *out = calloc(count, size);
  size_t copySize = MIN(entry->size, count * size);
  memcpy(*out, entry->data, copySize);
  if (byteOrder == 'l' || size == 1) {
    return;
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
}
// rational is 2 long: numerator and denominator
// https://www.exif.org/Exif2-2.PDF
void rationalEntryToDouble(ExifContent *content, uint tag, double **out,
                           int count, char byteOrder) {
  int longCount = count * 2;
  size_t longSize = sizeof(uint32_t);
  uint32_t *buffer;
  anyEntry(content, tag, (void **)&buffer, longSize, longCount, byteOrder);
  *out = calloc(count, sizeof(double));
  uint32_t *ptr = buffer;
  for (int i = 0; i < count; ++i) {
    uint32_t n = *ptr;
    ptr += 1;
    uint32_t d = *ptr;
    ptr += 1;
    (*out)[i] = (double)n / (double)d;
    // printf("%u / %u = %f \n", n, d, (*out)[i]);
  }
  free(buffer);
}
void content(ExifContent *content, Metadata *data) {
  ExifIfd ifd = exif_content_get_ifd(content);
  if (ifd == EXIF_IFD_COUNT) {
    return;
  }
  ExifEntry *entry;
  char *buffer;
  // https://exiftool.org/TagNames/EXIF.html
  switch (ifd) {
  case 0: {
    stringEntry(content, 0x0132, &(data->dateTime));
    stringEntry(content, 0x9290, &(data->subSecTime));
    stringEntry(content, 0x9010, &(data->offsetTime));
    anyEntry(content, 0x0112, (void **)&(data->orientation), sizeof(uint16_t),
             1, data->byteOrder);
    stringEntry(content, 0xc614, &(data->uniqueCameraModel));
    stringEntry(content, 0xc615, &(data->localizedCameraModel));
    stringEntry(content, 0x0110, &(data->model));
  } break;
  case 1: {

  } break;
  case 2: {
    stringEntry(content, 0x9003, &(data->dateTimeOriginal));
    stringEntry(content, 0x9291, &(data->subSecTimeOriginal));
    stringEntry(content, 0x9011, &(data->offsetTimeOriginal));
    stringEntry(content, 0x9004, &(data->dateTimeDigitized));
    stringEntry(content, 0x9292, &(data->subSecTimeDigitized));
    stringEntry(content, 0x9012, &(data->offsetTimeDigitized));
  } break;
  case 3: {
    stringEntry(content, 0x0001, &(data->gpsLatitudeRef));
    rationalEntryToDouble(content, 0x0002, &(data->gpsLatitude), 3,
                          data->byteOrder);
    stringEntry(content, 0x0003, &(data->gpsLongitudeRef));
    rationalEntryToDouble(content, 0x0004, &(data->gpsLongitude), 3,
                          data->byteOrder);
    anyEntry(content, 0x0005, (void **)&(data->gpsAltitudeRef), sizeof(uint8_t),
             1, data->byteOrder);
    rationalEntryToDouble(content, 0x0006, &(data->gpsAltitude), 1,
                          data->byteOrder);
  } break;
  default:
    break;
  }
}
int exif(char *filename, Metadata **out) {
  VipsImage *image = vips_image_new_from_file(filename, NULL);
  if (!image) {
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
  ExifData *exifData = exif_data_new_from_data(exifDataRaw, exifDataRawLength);
  if (exifData == NULL) {
    g_object_unref(image);
    return -1;
  }
  Metadata *metadata = calloc(1, sizeof(Metadata));
  if (!metadata) {
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
