{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2009, 2011  Thoronador

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

unit ConstructionCallback;

interface

uses
  BasicCallback, Colony;

const
  { integer constant that identifies the type of a callback record }
  CBT_CONSTRUCTION = 14;

type

  TConstructionCallback = class(TBasicCallback)
    public
      AColony: TColony;

      { function to handle the callback, i.e. perform all necessary steps after
        the player has made his/her choice. Should return true on success, false
        on failure

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function Handle: Boolean; override;

      constructor Create(const col: TColony);
  end;//class
  PConstructionCallback = ^TConstructionCallback;

implementation

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

constructor TConstructionCallback.Create(const col: TColony);
begin
  _type:= CBT_CONSTRUCTION;
  AColony:= col;
end;//construc

end.