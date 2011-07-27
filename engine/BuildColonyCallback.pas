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

unit BuildColonyCallback;

interface

uses
  BasicCallback, Data, Units;

const
  { integer constant that identifies the type of a callback record }
  CBT_BUILD_COLONY = 3;

type

  TBuildColonyCallback = class(TBasicCallback)
    public
      x,y: Byte;
      founder: TUnit;
      AData: TData;

      { function to handle the callback, i.e. perform all necessary steps after
        the player has made his/her choice. Should return true on success, false
        on failure

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function Handle: Boolean; override;

      constructor Create(const posX, posY: Byte; f: TUnit; dat: TData);
  end;//class
  PBuildColonyCallback = ^TBuildColonyCallback;

implementation

uses
  Colony, Map;

function TBuildColonyCallback.Handle: Boolean;
var i,j: Integer;
begin
  Result:= False;
  if ((inputText='') or (founder=nil) or (AData=nil) or (x>=cMap_X-1)
      or (y>=cMap_Y-1) or (x=0) or (y=0)) then Exit;
  if founder.WarpToXY(x,y, AData.GetMap) then
  begin
    //set founder into first square that is not water
    i:= -1;
    while ((i<=1) and (not Result)) do
    begin
      j:= -1;
      while ((j<=1) and (not Result)) do
      begin
        if (AData.GetMap.IsValidMapPosition(x+i, y+j)) then
        begin
          //check if tile has no water and whether it's not the colony square
          if ((not AData.GetMap.tiles[x+i, y+j].IsWater) and ((i<>0) or (j<>0))) then
          begin
            //first non-watery tile is reached, place the unit
            (AData.NewColony(x,y, founder.GetNation, inputText)).SetUnitInField(i, j, founder);
            AData.GetMap.tiles[x,y].CreateRoad;
            Result:= True;
          end;//if
        end;//if
        j:= j+1;
      end; //while j
      i:= i+1;
    end;//while i
    //Did we find a non-water field?
    if (not Result) then
    begin
      //try to set founder into townhall
      (AData.NewColony(x,y, founder.GetNation, inputText)).SetUnitInBuilding(btTownHall, 0, founder);
      AData.GetMap.tiles[x,y].CreateRoad;
      Result:= True;
    end;//if
  end;//if
end;//func

constructor TBuildColonyCallback.Create(const posX, posY: Byte; f: TUnit; dat: TData);
begin
  _type:= CBT_BUILD_COLONY;
  inputText:= '';
  x:= posX;
  y:= posY;
  founder:= f;
  AData:= dat;
end;//construc

end.