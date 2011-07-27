{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2011  Dirk Stolle

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
    public
      Ship: TUnit;
      UType: TUnitType;
      x,y: Byte;
      AMap: TMap;

      { function to handle the callback, i.e. perform all necessary steps after
        the player has made his/her choice. Should return true on success, false
        on failure

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function Handle: Boolean; override;

      constructor Create(AShip: TUnit; const posX, posY: Byte; theMap: TMap);
  end;//class
  PLandfallCallback = ^TLandfallCallback;

implementation

function TLandfallCallback.Handle: Boolean;
begin
  if ((option=1) and (Ship<>nil)) then Result:= Ship.UnloadUnit(UType, x,y, AMap)
  else Result:= False;
end;//func

constructor TLandfallCallback.Create(AShip: TUnit; const posX, posY: Byte; theMap: TMap);
begin
  _type:= CBT_LANDFALL;
  Ship:= AShip;
  x:= posX;
  y:= posY;
  AMap:= theMap;
end;//construc

end.