{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2011, 2012  Dirk Stolle

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

unit LandfallCallback;

interface

uses
  BasicCallback, Map, Units;

const
  { integer constant that identifies the type of a callback record }
  CBT_LANDFALL = 2;

type
  TLandfallCallback = class(TBasicCallback)
    protected
      Ship: TUnit;
      UType: TUnitType;
      x,y: Byte;
      AMap: TMap;

    public
      { function to handle the callback, i.e. perform all necessary steps after
        the player has made his/her choice. Should return true on success, false
        on failure

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function Handle: Boolean; override;

      { function to return the callback's type }
      function GetType: Integer; override;

      constructor Create(AShip: TUnit; const passengerType: TUnitType; const posX, posY: Byte; theMap: TMap);
  end;//class
  PLandfallCallback = ^TLandfallCallback;

implementation

function TLandfallCallback.Handle: Boolean;
begin
  if ((option=1) and (Ship<>nil)) then Result:= Ship.UnloadUnit(UType, x,y, AMap)
  else Result:= False;
end;//func

function TLandfallCallback.GetType: Integer;
begin
  Result:= CBT_LANDFALL;
end;//func

constructor TLandfallCallback.Create(AShip: TUnit; const passengerType: TUnitType; const posX, posY: Byte; theMap: TMap);
begin
  Ship:= AShip;
  UType:= passengerType;
  x:= posX;
  y:= posY;
  AMap:= theMap;
end;//construc

end.
