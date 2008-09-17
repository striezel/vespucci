unit Helper;

interface

type
  TShortStrArr = array of ShortString;

  function IntToStr(const i: Integer): string;
  function ToShortStrArr(const s1, s2: ShortString): TShortStrArr; overload;
  function ToShortStrArr(const s1, s2, s3: ShortString): TShortStrArr; overload;
  function ToShortStrArr(const s1, s2, s3, s4: ShortString): TShortStrArr; overload;
  function ToShortStrArr(const s1, s2, s3, s4, s5: ShortString): TShortStrArr; overload;

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

end.