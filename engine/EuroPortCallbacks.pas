{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2010, 2011, 2012  Dirk Stolle

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

unit EuroPortCallbacks;

interface

uses
  BasicCallback, Data, EuropeanNation, Units;

const
  { integer constant that identifies the type of a callback record }
  CBT_EURO_PORT_UNIT = 7;
  CBT_EURO_PORT_BUY = 8;
  CBT_EURO_PORT_TRAIN = 9;

type

  TEuroPortUnitCallback = class(TBasicCallback)
    protected
      AUnit: TUnit;
      EuroNat: TEuropeanNation;

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

      constructor Create(const u: TUnit; const eNat: TEuropeanNation);
  end;//class
  PEuroPortUnitCallback = ^TEuroPortUnitCallback;


  TEuroPortBuyCallback = class(TBasicCallback)
    protected
      AData: TData;
      EuroNat: TEuropeanNation;

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

      constructor Create(const dat: TData; const eNat: TEuropeanNation);
  end;//class
  PEuroPortBuyCallback = ^TEuroPortBuyCallback;


  TEuroPortTrainCallback = class(TBasicCallback)
    protected
      AData: TData;
      EuroNat: TEuropeanNation;

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

      constructor Create(const dat: TData; const eNat: TEuropeanNation);
  end;//class
  PEuroPortTrainCallback = ^TEuroPortTrainCallback;

implementation

uses
  Goods, Map;

{ **** TEuroPortUnitCallback **** }

function TEuroPortUnitCallback.Handle: Boolean;
begin
  Result:= False;
  if ((AUnit=nil) or (EuroNat=nil)) then Exit;
  case option of
    0: begin //ship
         if (AUnit.GetState=usWaitingForShip) then AUnit.SetState(usNormal)
         else AUnit.SetState(usWaitingForShip);
       end;
    1: //muskets
       if not EuroNat.IsBoycotted(gtMusket) then
       begin
         if (AUnit.HasMuskets) then
         begin
           EuroNat.SellGood(gtMusket, 50);
           AUnit.GiveMuskets(False);
         end//if
         else begin
           if EuroNat.GetGold>=EuroNat.GetPrice(gtMusket, False)*50 then
           begin
             EuroNat.BuyGood(gtMusket, 50);
             AUnit.GiveMuskets(True);
           end;//if
         end;//if
       end; //if
    2: //horses
       if not EuroNat.IsBoycotted(gtHorses) then
       begin
         if (AUnit.HasHorses) then
         begin
           EuroNat.SellGood(gtHorses, 50);
           AUnit.GiveHorses(False);
         end//if
         else begin
           if EuroNat.GetGold>=EuroNat.GetPrice(gtHorses, False)*50 then
           begin
             EuroNat.BuyGood(gtHorses, 50);
             AUnit.GiveHorses(True);
           end;//if
         end;//if
       end; //if
    3: begin//tools
         if not EuroNat.IsBoycotted(gtTool) then
         begin
           if (AUnit.GetToolAmount>0) then
           begin
             EuroNat.SellGood(gtTool, AUnit.GetToolAmount);
             AUnit.GiveTools(0);
           end//if
           else begin
             if (EuroNat.GetGold>=EuroNat.GetPrice(gtTool, False)*(100-AUnit.GetToolAmount)) then
             begin
               EuroNat.BuyGood(gtTool, 100-AUnit.GetToolAmount);
               AUnit.GiveTools(100);
             end;//if
           end;//if
         end;//if no boycott
       end;//3 (tools)
    4: ;//no changes at all
  end;//case
  Result:= True;
end;//func

function TEuroPortUnitCallback.GetType: Integer;
begin
  Result:= CBT_EURO_PORT_UNIT;
end;//func

constructor TEuroPortUnitCallback.Create(const u: TUnit; const eNat: TEuropeanNation);
begin
  AUnit:= u;
  EuroNat:= eNat;
end;//construc

{ **** TEuroPortBuyCallback **** }

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

function TEuroPortBuyCallback.GetType: Integer;
begin
  Result:= CBT_EURO_PORT_BUY;
end;//func

constructor TEuroPortBuyCallback.Create(const dat: TData; const eNat: TEuropeanNation);
begin
  AData:= dat;
  EuroNat:= eNat;
end;//construc

{ **** TEuroPortTrainCallback **** }

function TEuroPortTrainCallback.Handle: Boolean;
var train_unit: TUnitType;
    new_unit: TUnit;
begin
  Result:= False;
  if (EuroNat=nil) or (AData=nil) or (option=0) then Exit;
  case option of
    1: train_unit:= utFarmer;
    2: train_unit:= utFisher;
    3: train_unit:= utSilverMiner;
    4: train_unit:= utWoodcutter;
    5: train_unit:= utOreMiner;
    6: train_unit:= utPreacher;
    7: train_unit:= utStatesman;
    8: train_unit:= utCarpenter;
    9: train_unit:= utDistiller;
    10: train_unit:= utWeaver;
    11: train_unit:= utTobacconist;
    12: train_unit:= utFurTrader;
    13: train_unit:= utSmith;
    14: train_unit:= utWeaponSmith;
    15: train_unit:= utPioneer;
    16: train_unit:= utMissionary;
    17: train_unit:= utRegular;
  else train_unit:= utCriminal;//should not happen here
  end;//case
  if train_unit=utCriminal then Exit;
  if EuroNat.GetGold>=cUnitPrices[train_unit] then
  begin
    if EuroNat.HasValidSpawnpoint then
      new_unit:= AData.NewUnit(train_unit, EuroNat.GetCount, EuroNat.GetSpawnpointX, EuroNat.GetSpawnpointY)
    else
      new_unit:= AData.NewUnit(train_unit, EuroNat.GetCount, cMap_X-1, cMap_Y div 2);
    new_unit.SetLocation(ulEurope);
    new_unit.SetState(usWaitingForShip);
    EuroNat.DecreaseGold(cUnitPrices[train_unit]);
    Result:= True;
  end;//if
end;//func

function TEuroPortTrainCallback.GetType: Integer;
begin
  Result:= CBT_EURO_PORT_TRAIN;
end;//func

constructor TEuroPortTrainCallback.Create(const dat: TData; const eNat: TEuropeanNation);
begin
  AData:= dat;
  EuroNat:= eNat;
end;//construc

end.
