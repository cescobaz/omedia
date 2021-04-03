#include <stdio.h>
#include <stdlib.h>
#include "oimage.h"

int main(int argc, char **argv) {
  init();
  char *filename, *ofilename;
  if (argc > 0) {
    filename = argv[1];
  } else {
    filename = "/Users/cescobaz/omedia/to-import/"
               "E399459F-476E-4CAB-95E6-60BC20E26D12.heic";
  }
  if (argc > 1) {
    ofilename = argv[2];
  } else {
    ofilename = "prova.jpg";
  }
  printf("filename %s\n", filename);
  int result = exif(filename, NULL);
  printf("exif %d\n", result);
  result = thumbnail(filename, ofilename, 256);
  if (result != 0) {
    printf("error %d\n", result);
    exit(result);
  }
  return 0;
}
