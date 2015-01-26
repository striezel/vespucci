{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2009, 2010  Dirk Stolle

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

unit Nation;

interface

uses
  Helper, Classes;

const
  cMinNations = 1;
  cMinEuropean = 1;
  cMaxEuropean = 4;
  cMinIndian = 5;
  cMaxIndian = 12;
  cMaxNations = 12;

  { integer constant representing a nation }
  //Europeans
  cNationEngland = 1;
  cNationFrance = 2;
  cNationSpain = 3;
  cNationHolland = 4;
  //Indians
  cNationArawak = 5;
  cNationAztec = 6;
  cNationInca = 7;
  cNationTupi = 8;
  cNationCherokee = 9;
  cNationIroquois = 10;
  cNationSioux = 11;
  cNationApache = 12;

  //the colours of the nations as RGB values
  cNationColours: array[cMinNations..cMaxIndian] of array[0..2] of Byte
                =( //europeans
                   (255, 0, 0), //England
                   (50, 50, 255), //France
                   (255, 255, 0), //Spain
                   (255, 128, 0), //Holland
                   //indians
                   (100, 140, 190), //Arawak
                   (200, 160, 30), //Aztec
                   (240, 240, 200), //Inca
                   (0, 100, 0), //Tupi
                   (120, 160, 80), //Cherokee
                   (110, 60, 25), //Iroquois
                   (140, 0, 0), //Sioux
                   (190, 170, 130) //Apache
                 );

type
  { ********
    **** TNation class
    ****
    **** purpose: represents a nation within the game, i.e. a European country
    ****          or an Indian nation. However, there are more specialised
    ****          classes for both, derived from TNation.
    *******
  }
  TNation = class
    protected
      m_count: LongInt;
      m_NameStr: string;
    public
      { constructor

        parameters:
            num     - integer that identifies that nation
            NameStr - name of the nation
      }
      constructor Create(const num: LongInt; const NameStr: string);

      { destructor }
      destructor Destroy; override;

      { returns true, if this nation is an IndianNation

        remarks:
            Abstract function, has to be implemented in derived classes.
      }
      function IsIndian: Boolean; virtual; abstract;

      { returns true, if this nation is an EuropeanNation

        remarks:
            Abstract function, has to be implemented in derived classes.
      }
      function IsEuropean: Boolean; virtual; abstract;

      { returns the integer that identifies that nation }
      function GetCount: LongInt;

      { set the integer that identifies that nation

        parameters:
            new_num - new integer value that should identify that nation

        remarks:
            This procedure should not be called directly, it's only used during
            the loading process.
      }
      procedure ChangeCount(const new_num: LongInt);

      { returns the nation's name }
      function GetName: string;

      { changes the nation's name

        parameters:
            new_name - the new name of the nation - empty string is not allowed
      }
      procedure ChangeName(const new_name: string);

      { tries to save this nation's data to the given stream and returns true
        in case of success, or false if an error occured

        parameters:
            fs - the file stream the nation shall be saved in

        remarks:
            The file stream already has to be opened and be ready for writing.
      }
      function SaveToStream(var fs: TFileStream): Boolean; virtual;

      { loads the nation from the stream and returns true on success

        parameters:
            fs   - the file stream the nation will be loaded from
      }
      function LoadFromStream(var fs: TFileStream): Boolean; virtual;
  end;//class

implementation

//**** functions of TNation ****

constructor TNation.Create(const num: LongInt; const NameStr: string);
begin
  m_count:= num;
  m_NameStr:= NameStr;
end;//construc

destructor TNation.Destroy;
begin
  inherited Destroy;
end;//destructor

function TNation.GetName: string;
begin
  Result:= m_NameStr;
end;//func

procedure TNation.ChangeName(const new_name: string);
begin
  if new_name<>'' then m_NameStr:= new_name;
end;//proc

function TNation.GetCount: LongInt;
begin
  Result:= m_count;
end;//func

procedure TNation.ChangeCount(const new_num: LongInt);
begin
  m_count:= new_num;
end;//proc

function TNation.SaveToStream(var fs: TFileStream): Boolean;
var i: LongInt;
begin
  Result:= False;
  if fs=nil then Exit;
  Result:= (fs.Write(m_Count, sizeof(LongInt))=sizeof(LongInt));
  i:= length(m_NameStr);
  Result:= Result and (fs.Write(i, sizeof(LongInt))=sizeof(LongInt));
  Result:= Result and (fs.Write(m_NameStr[1], length(m_NameStr))=length(m_NameStr));
end;//func

function TNation.LoadFromStream(var fs: TFileStream): Boolean;
var i: LongInt;
    temp_str: string;
begin
  Result:= False;
  if (fs=nil) then  Exit;
  i:= 0;
  Result:= (fs.Read(i, sizeof(LongInt))=sizeof(LongInt));
  if (not Result) then
  begin
    WriteLn('TNation.LoadFromStream: Error while reading count.');
    Exit;
  end;//if
  self.ChangeCount(i);
  Result:= Result and (fs.Read(i, sizeof(LongInt))=sizeof(LongInt));
  if (i<=0) or (i>255) then
  begin
    Result:= False; //string to short or to long
    WriteLn('TNation.LoadFromStream: Error: name string does not meet the '
            +'length requirements.');
    Exit;
  end;//if
  temp_str:= SpaceString(i);
  Result:= Result and (fs.Read(temp_str[1], i)=i);
  if (not Result) then
  begin
    WriteLn('TNation.LoadFromStream: Error while reading name.');
    Exit;
  end;//if
  temp_str:= Trim(temp_str);
  ChangeName(temp_str);
end;//func

end.
