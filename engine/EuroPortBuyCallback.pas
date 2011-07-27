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

unit EuroPortBuyCallback;

interface

uses
  BasicCallback, Data, EuropeanNation;

const
  { integer constant that identifies the type of a callback record }
  CBT_EURO_PORT_BUY = 8;

type

  TEuroPortBuyCallback = class(TBasicCallback)
    public
      AData: TData;
      EuroNat: TEuropeanNation;

      { function to handle the callback, i.e. perform all necessary steps after
        the player has made his/her choice. Should return true on success, false
        on failure

        remarks:
            Derived classes have to implement their own version of that function.
      }
      function Handle: Boolean; override;

      constructor Create(const dat: TData; const eNat: TEuropeanNation);
  end;//class
  PEuroPortBuyCallback = ^TEuroPortBuyCallback;

implementation

uses
  Map, Units;

function TEuroPortBuyCallback.Handle: Boolean;
var buy_unit: TUnitType;
    new_unit: TUnit;
    start_x, start_y: Byte;
begin
  Result:= (option=0);
  if (EuroNat=nil) or (AData=nil) or (option=0) then Exit;
  case option of
    1: buy_unit:= utArtillery;
    2: buy_unit:= utCaravel;
    3: buy_unit:= utTradingShip;
    4: buy_unit:= utGalleon;
    5: buy_unit:= utPrivateer;
    6: buy_unit:= utFrigate;
  else buy_unit:= utCriminal;//should not happen
  end;//case
  if buy_unit=utCriminal then Exit;
  //buy it
  //Is there enough gold to by it?
  if EuroNat.GetGold>=cShipPrices[buy_unit] then
  begin
    if length(AData.GetAllShipsInEurope(EuroNat.GetCount))>0 then
    begin
      //set start position to position of first ship
      new_unit:= AData.GetAllShipsInEurope(EuroNat.GetCount)[0];
      start_x:= new_unit.GetPosX;
      start_y:= new_unit.GetPosY;
    end//if
    else begin
      //check for spawnpoint
      if EuroNat.HasValidSpawnpoint then
      begin
        //set start position to spawnpoint position
        start_x:= EuroNat.GetSpawnpointX;
        start_y:= EuroNat.GetSpawnpointY;
      end
      else begin
        //set start position to center at the eastern end of map
        start_x:= cMap_X-1;
        start_y:= cMap_Y div 2;
      end;//else
    end;//else
    new_unit:= AData.NewUnit(buy_unit, EuroNat.GetCount, start_x, start_y);
    new_unit.SetLocation(ulEurope);
    EuroNat.DecreaseGold(cShipPrices[buy_unit]);
    Result:= True;
  end;//if
end;//func

constructor TEuroPortBuyCallback.Create(const dat: TData; const eNat: TEuropeanNation);
begin
  _type:= CBT_EURO_PORT_BUY;
  AData:= dat;
  EuroNat:= eNat;
end;//construc

end.