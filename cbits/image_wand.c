#include <MagickWand/MagickWand.h>

int thumbnail(char *inputFilename, char *outputFilename, int maxSize) {
  MagickWandGenesis();
  
  MagickWand *m_wand = NewMagickWand();
  MagickReadImage(m_wand, inputFilename);
  
  int width = MagickGetImageWidth(m_wand);
  int height = MagickGetImageHeight(m_wand);
  
  if (width > height && width > maxSize) {
    height = ((float)maxSize / (float)width) * (float)height;
    width = maxSize;
  } else if (height > width && height > maxSize) {
    width = ((float)maxSize / (float)height) * (float)width;
    height = maxSize;
  } else if (width == height && width > maxSize) {
    width = maxSize;
    height = maxSize;
  }
  printf("%d %d", width, height);
  // https://urmaul.com/blog/imagick-filters-comparison/
  MagickResizeImage(m_wand,width,height,BoxFilter);
  //MagickScaleImage(m_wand,width,height);
  MagickSetImageCompressionQuality(m_wand,90);
  MagickWriteImage(m_wand, outputFilename);
  if(m_wand) {
    m_wand = DestroyMagickWand(m_wand);
  }
  
  MagickWandTerminus();
  return 0;
}
int main(int argc, char ** argv) {
  return thumbnail("/Users/cescobaz/omedia/to-import/E399459F-476E-4CAB-95E6-60BC20E26D12.heic", "prova.jpg", 256);
}
