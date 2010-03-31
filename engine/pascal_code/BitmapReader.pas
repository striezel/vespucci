unit BitmapReader;

interface

uses
  SysUtils, Classes;

type
  TArraySq32RGB = array [0..32*32-1] of packed record
                                          r: Byte;
                                          g: Byte;
                                          b: Byte;
                                        end;
  TArraySq32RGBA = array [0..32*32-1] of packed record
                                           r: Byte;
                                           g: Byte;
                                           b: Byte;
                                           a: Byte;
                                         end;
  TArray128x64RGB = array [0..128*64-1] of packed record
                                          r: Byte;
                                          g: Byte;
                                          b: Byte;
                                        end;
  TArray128x64RGBA = array [0..128*64-1] of packed record
                                           r: Byte;
                                           g: Byte;
                                           b: Byte;
                                           a: Byte;
                                         end;



  TBitmapFileHeader = packed record
    bfType: Word;
    bfSize: LongWord;
    bfReserved: LongWord;
    bfOffBits: LongWord;
  end;//rec
  TBitmapInfoHeader = packed record
    biSize: LongWord;
    biWidth: Longint;
    biHeight: Longint;
    biPlanes: Word;
    biBitCount: Word;
    biCompression: LongWord;
    biSizeImage: LongWord;
    biXPixelsPerMeter: Longint;
    biYPixelsPerMeter: Longint;
    biClrUsed: LongWord;
    biClrImportant: LongWord;
  end;//rec

  {function ReadBitmap(const FileName: string; var bfh: TBitmapFileHeader;
                     var bih: TBitmapInfoHeader; var Data: Pointer;
                     var err: string): Boolean;}
  function ReadBitmapToArr32RGB(const FileName: string; var Data: TArraySq32RGB;
                     var err: string): Boolean;
  function ReadBitmapToArr128x64RGB(const FileName: string; var Data: TArray128x64RGB;
                     var err: string): Boolean;
  procedure SwapRGB_To_BGR(var pic: TArraySq32RGB); overload;
  procedure SwapRGB_To_BGR(var pic: TArray128x64RGB); overload;
  procedure SwapRGB_To_RBG(var pic: TArraySq32RGB);
  procedure SwapRGB_To_GRB(var pic: TArraySq32RGB);
  procedure GetAlphaByColor(const src: TArraySq32RGB; var dest: TArraySq32RGBA); overload;
  procedure GetAlphaByColor(const src: TArray128x64RGB; var dest: TArray128x64RGBA); overload;

implementation

{function ReadBitmap(const FileName: string; var bfh: TBitmapFileHeader;
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
end;//func}

function ReadBitmapToArr32RGB(const FileName: string; var Data: TArraySq32RGB;
                     var err: string): Boolean;
var fs: TFileStream;
    bfh: TBitmapFileHeader;
    bih: TBitmapInfoHeader;
begin
  Result:= False;
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
    err:= 'Wrong bfType ('+IntToStr(bfh.bfType)+'). Should be 19778.';
    fs.Free;
    Exit;
  end;//if

  if bih.biSize<>40 then
  begin
    err:= 'Wrong size of BitmapInfoHeader ('+IntToStr(bih.biSize)+'), should be 40.';
    fs.Free;
    Exit;
  end;

  //check width
  if bih.biWidth <> 32 then
  begin
    err:= 'Width is not 32px. (Width: '+IntToStr(bih.biWidth)+')';
    fs.Free;
    Exit;
  end;
  //check height
  if abs(bih.biHeight) <>32 then
  begin
    err:= 'Height is not 32px. (Height: '+IntToStr(abs(bih.biHeight))+')';
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
  if (bih.biSizeImage<>(32*32*3)) then
  //there aren't three bytes for every px
  begin
    err:= 'Data has invalid size.';
    fs.Free;
    Exit;
  end;//if

  //now go for the data
  //order for 24bpp is blue, green, red

  //check for Offset and seek it, if neccessary
  if (bfh.bfOffBits<>fs.Position) then fs.Seek(bfh.bfOffBits, soBeginning);
  //now we can start the reading
  if (fs.Read(Data, bih.biSizeImage)<> 32*32*3) then
  begin
    err:= 'Could not read all data from file!';
    fs.Free;
    Exit;
  end;//if
  //we are lucky...so far
  fs.Free;
  Result:= True;
  err:= 'No error.';
end;//func

function ReadBitmapToArr128x64RGB(const FileName: string; var Data: TArray128x64RGB;
                     var err: string): Boolean;
var fs: TFileStream;
    bfh: TBitmapFileHeader;
    bih: TBitmapInfoHeader;
begin
  Result:= False;
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
    err:= 'Wrong bfType ('+IntToStr(bfh.bfType)+'). Should be 19778.';
    fs.Free;
    Exit;
  end;//if

  if bih.biSize<>40 then
  begin
    err:= 'Wrong size of BitmapInfoHeader ('+IntToStr(bih.biSize)+'), should be 40.';
    fs.Free;
    Exit;
  end;

  //check width
  if bih.biWidth <> 128 then
  begin
    err:= 'Width is not 128px. (Width: '+IntToStr(bih.biWidth)+')';
    fs.Free;
    Exit;
  end;
  //check height
  if abs(bih.biHeight) <>64 then
  begin
    err:= 'Height is not 64px. (Height: '+IntToStr(abs(bih.biHeight))+')';
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
  if (bih.biSizeImage<>(128*64*3)) then
  //there aren't three bytes for every px
  begin
    err:= 'Data has invalid size.';
    fs.Free;
    Exit;
  end;//if

  //now go for the data
  //order for 24bpp is blue, green, red

  //check for Offset and seek it, if neccessary
  if (bfh.bfOffBits<>fs.Position) then fs.Seek(bfh.bfOffBits, soBeginning);
  //now we can start the reading
  if (fs.Read(Data, bih.biSizeImage)<> 128*64*3) then
  begin
    err:= 'Could not read all data from file!';
    fs.Free;
    Exit;
  end;//if
  //we are lucky...so far
  fs.Free;
  Result:= True;
  err:= 'No error.';
end;//func

procedure SwapRGB_To_BGR(var pic: TArraySq32RGB); overload;
var i: Integer;
    temp: Byte;
begin
  for i:= 0 to 32*32-1 do
  begin
    temp:= pic[i].r;
    pic[i].r:= pic[i].b;
    pic[i].b:= temp;
  end;//for
end;//proc

procedure SwapRGB_To_BGR(var pic: TArray128x64RGB); overload;
var i: Integer;
    temp: Byte;
begin
  for i:= 0 to 128*64-1 do
  begin
    temp:= pic[i].r;
    pic[i].r:= pic[i].b;
    pic[i].b:= temp;
  end;//for
end;//proc

procedure SwapRGB_To_RBG(var pic: TArraySq32RGB);
var i: Integer;
    temp: Byte;
begin
  for i:= 0 to 32*32-1 do
  begin
    temp:= pic[i].b;
    pic[i].b:= pic[i].g;
    pic[i].g:= temp;
  end;//for
end;//proc

procedure SwapRGB_To_GRB(var pic: TArraySq32RGB);
var i: Integer;
    temp: Byte;
begin
  for i:= 0 to 32*32-1 do
  begin
    temp:= pic[i].r;
    pic[i].r:= pic[i].g;
    pic[i].g:= temp;
  end;//for
end;//proc

procedure GetAlphaByColor(const src: TArraySq32RGB; var dest: TArraySq32RGBA); overload;
var i: Integer;
begin
  for i:= 0 to 32*32-1 do
  begin
    dest[i].r:= src[i].r;
    dest[i].g:= src[i].g;
    dest[i].b:= src[i].b;
    if ((src[i].r=255) and (src[i].g=0) and (src[i].b=255)) then dest[i].a:= 0
    else dest[i].a:= 255;
  end;//for
end;//proc

procedure GetAlphaByColor(const src: TArray128x64RGB; var dest: TArray128x64RGBA); overload;
var i: Integer;
begin
  for i:= 0 to 128*64-1 do
  begin
    dest[i].r:= src[i].r;
    dest[i].g:= src[i].g;
    dest[i].b:= src[i].b;
    if ((src[i].r=255) and (src[i].g=0) and (src[i].b=255)) then dest[i].a:= 0
    else dest[i].a:= 255;
  end;//for
end;//proc

end.