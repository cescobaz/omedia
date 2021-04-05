#include <stdio.h>
#include <stdlib.h>
#include "oimage.h"
#include "oexif.h"

int main(int argc, char **argv) {
  init();
  printf("arc %d\n", argc);
  char *filename, *ofilename;
  if (argc > 1) {
    filename = argv[1];
  } else {
    filename = "/Users/cescobaz/omedia/to-import/wait/"
               "E399459F-476E-4CAB-95E6-60BC20E26D12.heic";
  }
  if (argc > 2) {
    ofilename = argv[2];
  } else {
    ofilename = "prova.jpg";
  }
  printf("filename %s\n", filename);
  printf("ofilename %s\n", ofilename);
  Metadata *metadata = NULL;
  int result = exif(filename, &metadata);
  print_exif_metadata(metadata);
  free_exif(&metadata);
  return thumbnail(filename, ofilename, 256, NULL, NULL);
}
