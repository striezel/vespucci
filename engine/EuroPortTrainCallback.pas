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

unit EuroPortTrainCallback;

interface

uses
  BasicCallback, Data, EuropeanNation;

const
  { integer constant that identifies the type of a callback record }
  CBT_EURO_PORT_TRAIN = 9;

type

  TEuroPortTrainCallback = class(TBasicCallback)
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
  PEuroPortTrainCallback = ^TEuroPortTrainCallback;

implementation

uses
  Units, Map;

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

constructor TEuroPortTrainCallback.Create(const dat: TData; const eNat: TEuropeanNation);
begin
  _type:= CBT_EURO_PORT_TRAIN;
  AData:= dat;
  EuroNat:= eNat;
end;//construc

end.