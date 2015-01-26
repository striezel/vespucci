{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2009, 2011, 2012  Dirk Stolle

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

unit ColonyCallbacks;

interface

uses
  BasicCallback, Colony, Units;

const
  { integer constant that identifies the type of a callback record }
  CBT_RENAME_COLONY = 10;
  CBT_COLONY_UNIT = 12;
  CBT_CONSTRUCTION = 14;

type

  TRenameColonyCallback = class(TBasicCallback)
    protected
      AColony: TColony;

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

      constructor Create(const col: TColony);
  end;//class
  PRenameColonyCallback = ^TRenameColonyCallback;


  TColonyUnitCallback = class(TBasicCallback)
    protected
      AUnit: TUnit;

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

      constructor Create(const u: TUnit);
  end;//class
  PColonyUnitCallback = ^TColonyUnitCallback;


  TConstructionCallback = class(TBasicCallback)
    protected
      AColony: TColony;

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

      constructor Create(const col: TColony);
  end;//class
  PConstructionCallback = ^TConstructionCallback;

implementation

{ **** TRenameColonyCallback **** }

function TRenameColonyCallback.Handle: Boolean;
begin
  if (AColony=nil) then
  begin
    Result:= False;
    Exit;
  end;//if
  AColony.SetName(inputText);
  Result:= True;
end;//func

function TRenameColonyCallback.GetType: Integer;
begin
  Result:= CBT_RENAME_COLONY;
end;//func

constructor TRenameColonyCallback.Create(const col: TColony);
begin
  AColony:= col;
end;//construc

{ **** TColonyUnitCallback **** }

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

function TColonyUnitCallback.GetType: Integer;
begin
  Result:= CBT_COLONY_UNIT;
end;//func

constructor TColonyUnitCallback.Create(const u: TUnit);
begin
  AUnit:= u;
end;//construc

{ **** TConstructionCallback **** }

function TConstructionCallback.Handle: Boolean;
var bt_arr: array of TBuildingType;
    i: Integer;
begin
  Result:= False;
  if (AColony=nil) then Exit;
  if (option=0) then
  begin
    AColony.SetCurrentConstruction(btNone);
    Result:= True
  end
  else begin
    SetLength(bt_arr, 0);
    for i:= Ord(Succ(btNone)) to Ord(High(TBuildingType)) do
    begin
      if (AColony.GetBuildingLevel(TBuildingType(i))<GetMaxBuildingLevel(TBuildingType(i))) then
      begin
        SetLength(bt_arr, length(bt_arr)+1);
        bt_arr[High(bt_arr)]:= TBuildingType(i);
      end;//if
    end;//for
    if (option>High(bt_arr)+1) then Exit;
    AColony.SetCurrentConstruction(bt_arr[option-1]);
    Result:= True;
  end;//else-branch
end;//func

function TConstructionCallback.GetType: Integer;
begin
  Result:= CBT_CONSTRUCTION;
end;//func

constructor TConstructionCallback.Create(const col: TColony);
begin
  AColony:= col;
end;//construc

end.
