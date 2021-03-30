#include <vips/vips.h>
#include <vips/resample.h>

int thumbnail(char *inputFilename, char *outputFilename) {
  VipsImage *out;
  int result = vips_thumbnail(inputFilename, &out, 256, NULL);
  if (result != 0) {
    return result;
  }
  result = vips_image_write_to_file(out, outputFilename, NULL);
  if (result != 0) {
    return result;
  }
  return 0;
}
int main(int argc, char ** argv) {
  return thumbnail("/Users/cescobaz/omedia/to-import/01A6E7C0-24F4-4E1B-ACC7-820A740449FA.heic", "prova.jpg");
}
