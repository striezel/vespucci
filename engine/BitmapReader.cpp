#include "BitmapReader.h"
#include <fstream>
#include <cstdlib>
#include "Helper.h"

/*function ReadBitmap(const FileName: string; var bfh: TBitmapFileHeader;
                     var bih: TBitmapInfoHeader; var Data: Pointer;
                     var err: string): Boolean;
var fs: TFileStream;
    i: Integer;
begin
  Result:= False;
  Data:= nil;
  err:= 'No error yet.';
  if (not FileExists(FileName)) then begin
    err:= 'File not found.';
    Exit;
  end;
  try
    fs:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  except
    err:= 'Could not open file.';
    fs.Free;
    Exit;
  end;//tryxcept
  if fs.Read(bfh, 14)<>14 then
  begin
    err:= 'File is to short to read BitmapFileHeader.';
    fs.Free;
    Exit;
  end;//if
  if fs.Read(bih, 40)<>40 then
  begin
    err:= 'File is to short to read BitmapInfoHeader.';
    fs.Free;
    Exit;
  end;//if

  //check header info
  if bfh.bfType <> 19778 then //i.e. <> "BM"
  begin
    err:= 'Wrong bfType ('+IntToStr(bfh.bfType)+'). Should be 19778 ("BM").';
    fs.Free;
    Exit;
  end;//if

  if bih.biSize<>40 then
  begin
    err:= 'Wrong size of BitmapInfoHeader ('+IntToStr(bih.biSize)+'), should be 40.';
    fs.Free;
    Exit;
  end;

  //check width for power of two
  i:=1;
  while i<1024*1024 do //image is hardly larger than 1.000.000 pixel in width
  begin
    if bih.biWidth = i then break;
    i:= i *2; //multiply with 2
  end;//while
  if i>=1024*1024 then
  begin
    err:= 'Width is no power of two. (Width: '+IntToStr(bih.biWidth)+')';
    fs.Free;
    Exit;
  end;
  //check height for power of two
  i:=1;
  while i<1024*1024 do //image is hardly larger than 1.000.000 pixel in width
  begin
    if abs(bih.biHeight) = i then break;
    i:= i *2; //multiply with 2
  end;//while
  if i>=1024*1024 then
  begin
    err:= 'Height is no power of two. (Height: '+IntToStr(abs(bih.biHeight))+')';
    fs.Free;
    Exit;
  end;

  if bih.biPlanes <> 1 then
  begin
    err:= 'Invalid number of planes.';
    fs.Free;
    Exit;
  end;//if

  if bih.biBitCount <> 24 then //we only want 24bit images
  begin
    err:= 'Bits per pixel is different from 24!';
    fs.Free;
    Exit;
  end;//if

  if bih.biCompression <> 0 then //only uncompressed bitmaps wanted
  begin
    err:= 'Bitmap uses unsupported, compressed format!';
    fs.Free;
    Exit;
  end;//if

  //bih.biSizeImage shows size of data in bytes
  if (bih.biSizeImage<>(bih.biWidth*abs(bih.biHeight)*3)) then
  //there aren't three bytes for every px
  begin
    err:= 'Data has invalid size.';
    fs.Free;
    Exit;
  end;//if

  //now go for the data
  //order for 24bpp is blue, green, red

  //allocate memory
  GetMem(Data, bih.biSizeImage);
  //check for Offset and seek it, if neccessary
  if (bfh.bfOffBits<>fs.Position) then fs.Seek(bfh.bfOffBits, soBeginning);
  //now we can start the reading
  if (fs.Read(Data^, bih.biSizeImage)<> bih.biSizeImage) then
  begin
    err:= 'Could not read all data from file!';
    fs.Free;
    FreeMem(Data, bih.biSizeImage);
    Data:= nil;
    Exit;
  end;//if
  //we are lucky...so far
  fs.Free;
  Result:= True;
  err:= 'No error.';
end;//func */

bool ReadBitmapToArr32RGB(const std::string& FileName, TArraySq32RGB& AData,
                          std::string& err)
{
  std::ifstream fs;
  fs.open(FileName.c_str());
  if (!fs)
  {
    err = "Could not open file.";
    return false;
  }

  fs.seekg(0, std::ios::end);
  if (fs.tellg()<14+40)
  {
    err = "File is to short to read BitmapFileHeader and BitmapInfoHeader.";
    fs.close();
    return false;
  }
  fs.seekg(0, std::ios::beg);

  TBitmapFileHeader bfh;
  fs.read((char*) &bfh, 14);
  if ((fs.gcount()!=14) or (!fs.good()))
  {
    err = "Error while reading BitmapFileHeader.";
    fs.close();
    return false;
  }

  TBitmapInfoHeader bih;
  fs.read((char*) &bih, 40);
  if ((fs.gcount()!=40) or (!fs.good()))
  {
    err = "Error while reading BitmapInfoHeader.";
    fs.close();
    return false;
  }

  //check header info
  if (bfh.bfType != 19778)//i.e. != "BM"
  {
    err = "Wrong bfType ("+IntToStr(bfh.bfType)+"). Should be 19778.";
    fs.close();
    return false;
  }//if

  if (bih.biSize!=40)
  {
    err = "Wrong size of BitmapInfoHeader ("+IntToStr(bih.biSize)+"), should be 40.";
    fs.close();
    return false;
  }

  //check width
  if (bih.biWidth != 32)
  {
    err = "Width is not 32px. (Width: "+IntToStr(bih.biWidth)+")";
    fs.close();
    return false;
  }
  //check height
  if (abs(bih.biHeight) != 32)
  {
    err = "Height is not 32px. (Height: "+IntToStr(abs(bih.biHeight))+")";
    fs.close();
    return false;
  }

  if (bih.biPlanes != 1)
  {
    err = "Invalid number of planes.";
    fs.close();
    return false;
  }//if

  if (bih.biBitCount != 24) //we only want 24bit images
  {
    err = "Bits per pixel is different from 24!";
    fs.close();
    return false;
  }//if

  if (bih.biCompression != 0) //only uncompressed bitmaps wanted
  {
    err = "Bitmap uses unsupported, compressed format!";
    fs.close();
    return false;
  }//if

  //bih.biSizeImage shows size of data in bytes
  if (bih.biSizeImage!=(32*32*3))
  //there aren't three bytes for every px
  {
    err = "Data has invalid size.";
    fs.close();
    return false;
  }//if

  //now go for the data
  //order for 24bpp is blue, green, red

  //check for Offset and seek it, if neccessary
  if (bfh.bfOffBits!=fs.tellg()) fs.seekg(bfh.bfOffBits, std::ios::beg);
  //now we can start the reading
  fs.read((char*) &AData, bih.biSizeImage);
  if ((fs.gcount()!=32*32*3) or (!fs.good()))
  {
    err = "Error while reading data from file!";
    fs.close();
    return false;
  }//if
  //we are lucky...so far
  fs.close();
  err = "No error.";
  return true;
}//func

bool ReadBitmapToArr128x64RGB(const std::string& FileName, TArray128x64RGB& AData,
                              std::string& err)
{
  std::ifstream fs;
  fs.open(FileName.c_str());
  if (!fs)
  {
    err = "Could not open file.";
    return false;
  }

  fs.seekg(0, std::ios::end);
  if (fs.tellg()<14+40)
  {
    err = "File is to short to read BitmapFileHeader and BitmapInfoHeader.";
    fs.close();
    return false;
  }
  fs.seekg(0, std::ios::beg);

  TBitmapFileHeader bfh;
  fs.read((char*) &bfh, 14);
  if ((fs.gcount()!=14) or (!fs.good()))
  {
    err = "Error while reading BitmapFileHeader.";
    fs.close();
    return false;
  }

  TBitmapInfoHeader bih;
  fs.read((char*) &bih, 40);
  if ((fs.gcount()!=40) or (!fs.good()))
  {
    err = "Error while reading BitmapInfoHeader.";
    fs.close();
    return false;
  }

  //check header info
  if (bfh.bfType != 19778) //i.e. <> "BM"
  {
    err = "Wrong bfType ("+IntToStr(bfh.bfType)+"). Should be 19778.";
    fs.close();
    return false;
  }//if

  if (bih.biSize!=40)
  {
    err = "Wrong size of BitmapInfoHeader ("+IntToStr(bih.biSize)+"), should be 40.";
    fs.close();
    return false;
  }

  //check width
  if (bih.biWidth != 128)
  {
    err = "Width is not 128px. (Width: "+IntToStr(bih.biWidth)+")";
    fs.close();
    return false;
  }
  //check height
  if (abs(bih.biHeight) != 64)
  {
    err = "Height is not 64px. (Height: "+IntToStr(abs(bih.biHeight))+")";
    fs.close();
    return false;
  }

  if (bih.biPlanes != 1)
  {
    err = "Invalid number of planes.";
    fs.close();
    return false;
  }//if

  if (bih.biBitCount != 24) //we only want 24bit images
  {
    err = "Bits per pixel is different from 24!";
    fs.close();
    return false;
  }//if

  if (bih.biCompression != 0) //only uncompressed bitmaps wanted
  {
    err = "Bitmap uses unsupported, compressed format!";
    fs.close();
    return false;
  }//if

  //bih.biSizeImage shows size of data in bytes
  if (bih.biSizeImage!=(128*64*3))
  //there aren't three bytes for every px
  {
    err = "Data has invalid size.";
    fs.close();
    return false;
  }//if

  //now go for the data
  //order for 24bpp is blue, green, red

  //check for Offset and seek it, if neccessary
  if (bfh.bfOffBits!=fs.tellg()) fs.seekg(bfh.bfOffBits, std::ios::beg);
  //now we can start the reading
  fs.read((char*) &AData, bih.biSizeImage);
  if ((fs.gcount()!=128*64*3) or (!fs.good()))
  {
    err = "Could not read all data from file!";
    fs.close();
    return false;
  }//if
  //we are lucky...so far
  fs.close();
  err = "No error.";
  return true;
}//func

void SwapRGB_To_BGR(TArraySq32RGB& pic)
{
  int i;
  Byte temp;
  for (i= 0; i<32*32; ++i)
  {
    temp = pic[i].r;
    pic[i].r = pic[i].b;
    pic[i].b = temp;
  }//for
}//proc

void SwapRGB_To_BGR(TArray128x64RGB& pic)
{
  int i;
  Byte temp;
  for (i= 0; i<128*64; ++i)
  {
    temp = pic[i].r;
    pic[i].r = pic[i].b;
    pic[i].b = temp;
  }//for
}//proc

void SwapRGB_To_RBG(TArraySq32RGB& pic)
{
  int i;
  Byte temp;
  for (i= 0; i<32*32; ++i)
  {
    temp = pic[i].b;
    pic[i].b = pic[i].g;
    pic[i].g = temp;
  }//for
}//proc

void SwapRGB_To_GRB(TArraySq32RGB& pic)
{
  int i;
  Byte temp;
  for (i= 0; i<32*32; ++i)
  {
    temp = pic[i].r;
    pic[i].r = pic[i].g;
    pic[i].g = temp;
  }//for
}//proc

void GetAlphaByColor(const TArraySq32RGB& src, TArraySq32RGBA& dest)
{
  int i;
  for (i= 0; i<32*32; ++i)
  {
    dest[i].r = src[i].r;
    dest[i].g = src[i].g;
    dest[i].b = src[i].b;
    if ((src[i].r==255) and (src[i].g==0) and (src[i].b==255)) dest[i].a = 0;
    else dest[i].a = 255;
  }//for
}//proc

void GetAlphaByColor(const TArray128x64RGB& src, TArray128x64RGBA& dest)
{
  int i;
  for (i= 0; i<128*64; ++i)
  {
    dest[i].r = src[i].r;
    dest[i].g = src[i].g;
    dest[i].b = src[i].b;
    if ((src[i].r==255) and (src[i].g==0) and (src[i].b==255)) dest[i].a = 0;
    else dest[i].a = 255;
  }//for
}//proc

