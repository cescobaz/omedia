#include <stdio.h>
#include "oimage.h"

int main(int argc, char **argv) {
  init();
  char *filename = "/Users/cescobaz/omedia/to-import/"
                   "E399459F-476E-4CAB-95E6-60BC20E26D12.heic";
  int result = exif(filename, NULL);
  printf("exif %d\n", result);
  return thumbnail(filename, "prova.jpg", 256);
}
