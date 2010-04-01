#ifndef BITMAPREADER_H
#define BITMAPREADER_H

#include <string>
#include "PascalTypes.h"

struct RecordRGB
{
  Byte r;
  Byte g;
  Byte b;
};

struct RecordRGBA
{
  Byte r;
  Byte g;
  Byte b;
  Byte a;
};

//typedef char (*pa)[3]; // "pa" is now a type for a pointer to an array of 3 chars

typedef RecordRGB (TArraySq32RGB)[32*32];

typedef RecordRGBA (TArraySq32RGBA)[32*32];

typedef RecordRGB (TArray128x64RGB)[128*64];

typedef RecordRGBA (TArray128x64RGBA)[128*64];

struct TBitmapFileHeader
{
  Word bfType;
  LongWord bfSize;
  LongWord bfReserved;
  LongWord bfOffBits;
};//rec

struct TBitmapInfoHeader
{
  LongWord biSize;
  LongInt biWidth;
  LongInt biHeight;
  Word biPlanes;
  Word biBitCount;
  LongWord biCompression;
  LongWord biSizeImage;
  LongInt biXPixelsPerMeter;
  LongInt biYPixelsPerMeter;
  LongWord biClrUsed;
  LongWord biClrImportant;
};//rec

  /*function ReadBitmap(const FileName: string; var bfh: TBitmapFileHeader;
                     var bih: TBitmapInfoHeader; var Data: Pointer;
                     var err: string): Boolean; */
bool ReadBitmapToArr32RGB(const std::string& FileName, TArraySq32RGB& AData,
                          std::string& err);
bool ReadBitmapToArr128x64RGB(const std::string& FileName, TArray128x64RGB& AData,
                              std::string& err);
void SwapRGB_To_BGR(TArraySq32RGB& pic);
void SwapRGB_To_BGR(TArray128x64RGB& pic);
void SwapRGB_To_RBG(TArraySq32RGB& pic);
void SwapRGB_To_GRB(TArraySq32RGB& pic);
void GetAlphaByColor(const TArraySq32RGB& src, TArraySq32RGBA& dest);
void GetAlphaByColor(const TArray128x64RGB& src, TArray128x64RGBA& dest);

#endif

