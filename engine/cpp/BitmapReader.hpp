/* **************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2009, 2010  Thoronador

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

  ***************************************************************************
*/

#ifndef BITMAPREADER_HPP
#define BITMAPREADER_HPP

#include "PascalTypes.hpp"
#include <array>
#include <string>

struct RGB
{
  Byte r;
  Byte g;
  Byte b;
}; //struct

struct RGBA
{
  Byte r;
  Byte g;
  Byte b;
  Byte a;
}; //struct

/* array that can hold a 32x32 px RGB image */
typedef std::array<RGB, 32*32> TArraySq32RGB;

/* array that can hold a 32x32 px RGB image with alpha channel */
typedef std::array<RGBA, 32*32> TArraySq32RGBA;

/* array that can hold a 128x64 px RGB image */
typedef std::array<RGB, 128*64> TArray128x64RGB;

/* array that can hold a 128x64 px RGB image with alpha channel */
typedef std::array<RGBA, 128*64> TArray128x64RGBA;


/* part of file header used in Bitmap files*/
struct TBitmapFileHeader
{
  Word bfType;
  LongWord bfSize;
  LongWord bfReserved;
  LongWord bfOffBits;
}; //rec

/* part of file header used in Bitmap files*/
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
}; //rec

/*function ReadBitmap(const FileName: string; var bfh: TBitmapFileHeader;
                      var bih: TBitmapInfoHeader; var Data: Pointer;
                      var err: string): Boolean;*/

/* tries to read a 32x32 px RGB bitmap from a file and returns true on success

   parameters:
       FileName - location of the bitmap
       Data     - record that will hold the image data in case of success
       err      - a string that contains an error message after the function
                  failed
*/
bool ReadBitmapToArr32RGB(const std::string& FileName, TArraySq32RGB& Data,
                          std::string& err);

/* tries to read a 128x64 px RGB bitmap from a file and returns true on success

   parameters:
       FileName - location of the bitmap
       Data     - record that will hold the image data in case of success
       err      - a string that contains an error message after the function
                  failed
*/
bool ReadBitmapToArr128x64RGB(const std::string& FileName, TArray128x64RGB& Data,
                              std::string& err);

/* swaps the red and blue colour components of the given 32x32 px RGB image

   parameters:
       pic - the array containing the image data that has to be altered
*/
void SwapRGB_To_BGR(TArraySq32RGB& pic);

/* swaps the red and blue colour components of the given 128x64 px RGB image

   parameters:
       pic - the array containing the image data that has to be altered
*/
void SwapRGB_To_BGR(TArray128x64RGB& pic);

/* swaps the green and blue colour components of the given 32x32 px RGB image

   parameters:
       pic - the array containing the image data that has to be altered
*/
void SwapRGB_To_RBG(TArraySq32RGB& pic);

/* swaps the red and green colour components of the given 32x32 px RGB image

   parameters:
       pic - the array containing the image data that has to be altered
*/
void SwapRGB_To_GRB(TArraySq32RGB& pic);

/* adds alpha channel data to a 32x32 px RGB image

   parameters:
       src  - the array containing the source image
       dest - the returned image with alpha channel
*/
void GetAlphaByColor(const TArraySq32RGB& src, TArraySq32RGBA& dest);

/* adds alpha channel data to a 128x64 px RGB image

   parameters:
       src  - the array containing the source image
       dest - the returned image with alpha channel
*/
void GetAlphaByColor(const TArray128x64RGB& src, TArray128x64RGBA& dest);

#endif // BITMAPREADER_HPP
