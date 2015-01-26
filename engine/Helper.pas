{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2010  Dirk Stolle

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
}

{ ********
  **** unit Helper
  ****
  **** purpose: holds some utility functions, constants and types that are used
  ****          by different other units
  *******
}

unit Helper;

interface

const
  { constants for quicker construction of strings in function SpaceString() }
  cSpace01 = ' ';
  cSpace02 = '  ';
  cSpace04 = '    ';
  cSpace08 = '        ';
  cSpace16 = cSpace08+cSpace08;
  cSpace32 = cSpace16+cSpace16;
  cSpace60 = '                                                            ';
  cSpace64 = cSpace32+cSpace32;
  cSpace128 = cSpace64+cSpace64;

type
  { type to represent an array of (short, i.e. <255 characters) strings }
  TShortStrArr = array of ShortString;

  { returns the string representation of an integer value

    parameters:
        i - the integer value

    remarks:
        There is a function with the same name and for the same purpose within
        a certain system unit, but I use that function to avoid that unit in
        the uses-clause of other units (because IntToStr() is the only function
        from that system unit that is needed).
  }
  function IntToStr(const i: Integer): string;

  { returns the length that the string representation of the given integer
    would have

    parameters:
        i - the integer value
  }
  function IntegerLength(const i: Integer): Byte;

  { returns a string array with the two specified elements

    parameters:
        see overloaded version of function below
  }
  function ToShortStrArr(const s1, s2: ShortString): TShortStrArr; overload;

  { returns a string array with the three specified elements

    parameters:
        see overloaded version of function below
  }
  function ToShortStrArr(const s1, s2, s3: ShortString): TShortStrArr; overload;

  { returns a string array with the four specified elements

    parameters:
        see overloaded version of function below
  }
  function ToShortStrArr(const s1, s2, s3, s4: ShortString): TShortStrArr; overload;

  { returns a string array with the five specified elements

    parameters:
        s1 - first string in the returned array
        s2 - second string in the returned array
        s3 - third string in the returned array
        s4 - fourth string in the returned array
        s5 - fifth string in the returned array
  }
  function ToShortStrArr(const s1, s2, s3, s4, s5: ShortString): TShortStrArr; overload;

  { returns a string array that is constructed by the given string and the array

    parameters:
        s1  - first string in the returned array
        arr - array that holds the rest of the returned array's elements
  }
  function ToShortStrArr(const s1: ShortString; const arr: TShortStrArr): TShortStrArr; overload;

  { returns a string that entirely consists of spaces

    parameters:
        len - length of the string (=number of spaces)
  }
  function SpaceString(const len: Byte): string;

  { "trims" a string, i.e. deletes all leading and trailing spaces, and returns
    the trimmed string

    parameters:
        str1 - the string that has to be trimmed

    remarks:
        See remarks for IntToStr(), these apply here, too.
  }
  function Trim(const str1: AnsiString): AnsiString;

  { "stretches" the concatenation of two strings to 59 characters, i.e.
    concatenates the first and second string, and puts enough spaces between
    them so that the result is exactly 59 characters long

    parameters:
        s1 - first part of that string
        s2 - last part of that string

    remarks:
        If the concatenation of s1 and s2 is already longer than 59 characters,
        then only one single space character is put between them.
  }
  function StretchTo59(const s1, s2: ShortString): ShortString;

  { "stretches" the concatenation of two strings to 60 characters, i.e.
    concatenates the first and second string, and puts enough spaces between
    them so that the result is exactly 60 characters long

    parameters:
        s1 - first part of that string
        s2 - last part of that string

    remarks:
        If the concatenation of s1 and s2 is already longer than 60 characters,
        then only one single space character is put between them.
  }
  function StretchTo60(const s1, s2: ShortString): ShortString;

  { returns the smaller value of the given parameters }
  function Min(const a,b: Integer): Integer; overload;
  function Min(const a,b: Double): Double; overload;

implementation

//helper functions
function IntToStr(const i: Integer): string;
begin
  Str(i, Result);
end;//func

function IntegerLength(const i: Integer): Byte;
begin
  if i<0 then Result:= IntegerLength(-i)+1
  else begin
    case i of
      0..9: Result:= 1;
      10..99: Result:= 2;
      100..999: Result:= 3;
      1000..9999: Result:= 4;
      10000..99999: Result:= 5;
      100000..999999: Result:= 6;
      1000000..9999999: Result:= 7;
      10000000..99999999: Result:= 8;
      100000000..999999999: Result:= 9;
    else Result:= 10;
    end;//case
  end;//else
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
  if ((len and 128)<>0) then Result:= cSpace128 else Result:= '';
  if ((len and 64)<>0) then Result:= Result+cSpace64;
  if ((len and 32)<>0) then Result:= Result+cSpace32;
  if ((len and 16)<>0) then Result:= Result+cSpace16;
  if ((len and 8)<>0) then Result:= Result+cSpace08;
  if ((len and 4)<>0) then Result:= Result+cSpace04;
  if ((len and 2)<>0) then Result:= Result+cSpace02;
  if ((len and 1)<>0) then Result:= Result+cSpace01;
end;//func

function Trim(const str1: AnsiString): AnsiString;
var
  i, len: Integer;
begin
  len:= length(str1);
  i:= 1;
  while (i<=len) and (str1[i] <= ' ') do Inc(i);
  if i>len then Result:= ''
  else begin
    while str1[len] <= ' ' do Dec(len);
    Result := copy(str1, i, len-i+1);
  end;//else
end;//func

function StretchTo59(const s1, s2: ShortString): ShortString;
begin
  Result:= Trim(s1);
  if length(Result)+length(Trim(s2))<59 then
    Result:= Result+SpaceString(59-length(Result)-length(Trim(s2)))+Trim(s2)
  else Result:= Result +' '+Trim(s2);
end;//func

function StretchTo60(const s1, s2: ShortString): ShortString;
begin
  Result:= Trim(s1);
  if length(Result)+length(Trim(s2))<60 then
    Result:= Result+SpaceString(60-length(Result)-length(Trim(s2)))+Trim(s2)
  else Result:= Result +' '+Trim(s2);
end;//func

function Min(const a,b: Integer): Integer; overload;
begin
  if a<=b then Result:= a else Result:= b;
end;//func

function Min(const a,b: Double): Double; overload;
begin
  if a<=b then Result:= a else Result:= b;
end;//func

end.
