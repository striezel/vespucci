{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2009, 2010, 2011  Dirk Stolle

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

unit Settlement;

interface

uses
  Classes, Nation;

type
  { ********
    **** TSettlement class
    ****
    **** purpose: represents a settlement within the game, i.e. a colony or a
    ****          tribe of the Indians. However, there are more specialised
    ****          classes for both, derived from TSettlement.
    *******
  }
  TSettlement = class
    public
      { constructor

        parameters:
            X, Y    - x- and y-compoment of settlement's position
            ANation - nation that founded the settlement
      *}
      constructor Create(const X, Y: LongInt; const ANation: LongInt);

      { destructor }
      destructor Destroy; override;

      { returns the nation that owns this settlement }
      function GetNation: LongInt;

      { changes the nation that owns the settlement

        parameters:
            new_nation - ID of the new nation
      }
      procedure ChangeNation(const new_nation: LongInt);

      { returns the x-component of settlement's position }
      function GetPosX: LongInt;
      { returns the x-component of settlement's position }
      function GetPosY: LongInt;

      { sets a new map position for the settlement

        parameters:
            x, y - x- and y-component of the new position

        remarks:
            Both values, x and y, have to be greater than zero. If they aren't,
            then the procedure behaves as if they had the value of 1.
      }
      procedure SetPosition(const x,y: LongInt);

      { tries to save the settlement to a stream and returns true in case of
        success

        parameters:
            fs - the file stream the settlement has to be saved to

        remarks:
            The file stream already has to be openend and has to be ready for
            writing.
      }
      function SaveToStream(var fs: TFileStream): Boolean; virtual;

      { tries to load a settlement from a stream and returns true in case of
        success

        parameters:
            fs - the file stream the settlement has to be loaded from

        remarks:
            The file stream already has to be openend and has to be ready for
            reading.
      }
      function LoadFromStream(var fs: TFileStream): Boolean; virtual;
    protected
      //index of the nations that owns the settlement
      m_Nation: LongInt;
      //settlement's position
      PosX, PosY: LongInt;
  end;//class

implementation

// **** TSettlement functions ****

constructor TSettlement.Create(const X, Y: Integer; const ANation: LongInt);
begin
  //no settlements outside of range or at border row/column (index: 0) allowed
  if X>0 then PosX:= X else PosX:= 1;
  if Y>0 then PosY:= Y else PosY:= 1;
  m_Nation:= ANation;
end;

destructor TSettlement.Destroy;
begin
  inherited Destroy;
end;//destruc

function TSettlement.GetNation: LongInt;
begin
  Result:= m_Nation;
end;//func

procedure TSettlement.ChangeNation(const new_nation: LongInt);
begin
  if new_nation>=0 then m_Nation:= new_nation;
end;//proc

function TSettlement.GetPosX: LongInt;
begin
  Result:= PosX;
end;//func

function TSettlement.GetPosY: LongInt;
begin
  Result:= PosY;
end;//func

procedure TSettlement.SetPosition(const x,y: LongInt);
begin
  if x>0 then PosX:= x else PosX:= 1;
  if y>0 then PosY:= y else PosY:= 1;
end;//proc

function TSettlement.SaveToStream(var fs: TFileStream): Boolean;
begin
  if fs=nil then
  begin
    Result:= False;
    Exit;
  end;//if
  //write nation ID
  Result:= (fs.Write(m_Nation, sizeof(LongInt))=sizeof(LongInt));
  //write position
  Result:= Result and (fs.Write(PosX, sizeof(LongInt))=sizeof(LongInt));
  Result:= Result and (fs.Write(PosY, sizeof(LongInt))=sizeof(LongInt));
end;//func

function TSettlement.LoadFromStream(var fs: TFileStream): Boolean;
var i, j: LongInt;
begin
  if fs=nil then
  begin
    Result:= False;
    Exit;
  end;//if
  //read nation ID
  i:= -1;
  Result:= (fs.Read(i, sizeof(LongInt))=sizeof(LongInt));
  if not Result then
  begin
    WriteLn('TSettlement.LoadFromStream: Error: could not read nation index.');
    Exit;
  end;//if
  if not (i in [cMinNations..cMaxNations]) then
  begin
    Result:= False;
    WriteLn('TSettlement.LoadFromStream: Error: invalid nation index.');
    Exit;
  end;//if
  ChangeNation(i);
  Result:= Result and (fs.Read(i, sizeof(LongInt))=sizeof(LongInt));
  Result:= Result and (fs.Read(j, sizeof(LongInt))=sizeof(LongInt));
  if not Result then
  begin
    WriteLn('TSettlement.LoadFromStream: Error: could not read position.');
    Exit;
  end;//if
  Result:= Result and (i>=0) and (j>=0);
  SetPosition(i, j);
end;//func

end.
