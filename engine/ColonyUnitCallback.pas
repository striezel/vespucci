{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2011  Thoronador

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

unit ColonyUnitCallback;

interface

uses
  BasicCallback, Units;

const
  { integer constant that identifies the type of a callback record }
  CBT_COLONY_UNIT = 12;

type

  TColonyUnitCallback = class(TBasicCallback)
    public
      AUnit: TUnit;

      { function to handle the callback, i.e. perform all necessary steps after
        the player has made his/her choice. Should return true on success, false
        on failure

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function Handle: Boolean; override;

      constructor Create(const u: TUnit);
  end;//class
  PColonyUnitCallback = ^TColonyUnitCallback;

implementation

function TColonyUnitCallback.Handle: Boolean;
begin
  if AUnit=nil then
  begin
    Result:= False;
    Exit;
  end;//if
  Result:= True;
  case option of
    0: case AUnit.GetState of
         usFortified, usWaitingForShip: AUnit.SetState(usNormal);
       else AUnit.SetState(usWaitingForShip);
       end;//case
    1: case AUnit.GetState of
         usFortified: AUnit.SetState(usWaitingForShip);
         usWaitingForShip, usNormal: AUnit.SetState(usFortified);
       end;//case
  //2: keine Veränderung/ no changes
  else Result:= False;
  end;//case
end;//func

constructor TColonyUnitCallback.Create(const u: TUnit);
begin
  _type:= CBT_COLONY_UNIT;
  AUnit:= u;
end;//construc

end.