unit Helper;

interface

const
  cSpace01 = ' ';
  cSpace02 = '  ';
  cSpace04 = '    ';
  cSpace08 = '        ';
  cSpace16 = cSpace08+cSpace08;
  cSpace32 = cSpace16+cSpace16;
  cSpace64 = cSpace32+cSpace32;
  cSpace128 = cSpace64+cSpace64;
//  cSpace256 = cSpace128+cSpace128;

type
  TShortStrArr = array of ShortString;

  function IntToStr(const i: Integer): string;
  function ToShortStrArr(const s1, s2: ShortString): TShortStrArr; overload;
  function ToShortStrArr(const s1, s2, s3: ShortString): TShortStrArr; overload;
  function ToShortStrArr(const s1, s2, s3, s4: ShortString): TShortStrArr; overload;
  function ToShortStrArr(const s1, s2, s3, s4, s5: ShortString): TShortStrArr; overload;
  function ToShortStrArr(const s1: ShortString; const arr: TShortStrArr): TShortStrArr; overload;
  function SpaceString(const len: Byte): string;

implementation

//helper functions
function IntToStr(const i: Integer): string;
begin
  Str(i, Result);
end;//func

function ToShortStrArr(const s1, s2: ShortString): TShortStrArr; overload;
begin
  SetLength(Result, 2);
  Result[0]:= s1;
  Result[1]:= s2;
end;//func

function ToShortStrArr(const s1, s2, s3: ShortString): TShortStrArr; overload;
begin
  SetLength(Result, 3);
  Result[0]:= s1;
  Result[1]:= s2;
  Result[2]:= s3;
end;//func

function ToShortStrArr(const s1, s2, s3, s4: ShortString): TShortStrArr; overload;
begin
  SetLength(Result, 4);
  Result[0]:= s1;
  Result[1]:= s2;
  Result[2]:= s3;
  Result[3]:= s4;
end;//func

function ToShortStrArr(const s1, s2, s3, s4, s5: ShortString): TShortStrArr; overload;
begin
  SetLength(Result, 5);
  Result[0]:= s1;
  Result[1]:= s2;
  Result[2]:= s3;
  Result[3]:= s4;
  Result[4]:= s5;
end;//func

function ToShortStrArr(const s1: ShortString; const arr: TShortStrArr): TShortStrArr; overload;
var i: Integer;
begin
  SetLength(Result, 1+length(arr));
  Result[0]:= s1;
  for i:= 1 to High(Result) do
    Result[i]:= arr[i-1];
end;//func

function SpaceString(const len: Byte): string;
begin
  Result:= '';
  if ((len and 128)<>0) then Result:= Result+cSpace128;
  if ((len and 64)<>0) then Result:= Result+cSpace64;
  if ((len and 32)<>0) then Result:= Result+cSpace32;
  if ((len and 16)<>0) then Result:= Result+cSpace16;
  if ((len and 8)<>0) then Result:= Result+cSpace08;
  if ((len and 4)<>0) then Result:= Result+cSpace04;
  if ((len and 2)<>0) then Result:= Result+cSpace02;
  if ((len and 1)<>0) then Result:= Result+cSpace01;
end;//func

end.
