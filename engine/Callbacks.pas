{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2009, 2010, 2011 Thoronador

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

unit Callbacks;

interface

uses
  Units, Map, Data, Colony, Goods, Nation, EuropeanNation, FoundingFathers;

const
  { integer constant that identifies the type of a callback record }
  CBT_ANY = 0;
  CBT_EXIT = 1;
  CBT_LANDFALL = 2;
  CBT_BUILD_COLONY = 3;
  CBT_SAVE_GAME = 4;
  CBT_LOAD_GAME = 5;
  CBT_JOB_CHANGE = 6;
  CBT_EURO_PORT_UNIT = 7;
  CBT_EURO_PORT_BUY = 8;
  CBT_EURO_PORT_TRAIN = 9;
  CBT_RENAME_COLONY = 10;
  CBT_ABANDON_COLONY = 11;
  CBT_COLONY_UNIT = 12;
  CBT_GOTO_SHIP = 13;
  CBT_CONSTRUCTION = 14;
  CBT_SELECT_FOUNDING_FATHER = 15;

type
  TExitCallback = procedure (const option: Integer);
  TLandfallCallback = function (const option: Integer; AShip: TUnit; const AType: TUnitType; const x,y: Byte; AMap: TMap): Boolean;

  { records that hold the data which is not common to all types of callbacks }
  TLandfallData = record
                    cbLandfall: TLandfallCallback;
                    Ship: TUnit;
                    UType: TUnitType;
                    x,y: Byte;
                    AMap: TMap;
                  end;//rec
  TBuildColonyData = record
                        x,y: Byte;
                        //ColName: ShortString; //delivered through input text
                        //num_nation: Byte; //given by founder.GetNation
                        founder: TUnit;
                        AData: TData;
                      end;//rec
  TSaveGameData = record
                    AData: TData;
                  end;//rec
  TLoadGameData = record
                    AData: TData;
                  end;//rec
  TJobChangeData = record
                     x_shift, y_shift: ShortInt;
                     AColony: TColony;
                   end;//rec
  TEuroPortUnitData = record
                        AUnit: TUnit;
                        EuroNat: TEuropeanNation;
                      end;//rec
  TEuroBuyData = record
                   AData: TData;
                   EuroNat: TEuropeanNation;
                 end;//rec
  TEuroTrainData = record
                     AData: TData;
                     EuroNat: TEuropeanNation;
                   end;//rec
  TRenameColonyData = record
                        AColony: TColony;
                      end;//rec
  TAbandonColonyData = record
                         AColony: TColony;
                         AData: TData;
                       end;//rec
  TColonyUnitData = record
                      AUnit: TUnit;
                    end;//rec
  TGotoShipData = record
                    Ship: TUnit;
                    AData: TData;
                  end;//rec
  TConstructionData = record
                        AColony: TColony;
                      end;//rec
  TFoundingSelectData = record
                          ENat: TEuropeanNation;
                          Choices: TFoundingFatherArray;
                        end;

  { record that holds all information that is neccessary to handle a callback }
  TCallbackRec = record
                   option: Integer;
                   inputText: ShortString;
                   _type: LongInt;
                   case LongInt of
                     0: (Data: Pointer);
                     1: (cbExit: TExitCallback);
                     2: (Landfall: TLandfallData);
                     3: (BuildColony: TBuildColonyData);
                     4: (SaveGame: TSaveGameData);
                     5: (LoadGame: TLoadGameData);
                     6: (JobChange: TJobChangeData);
                     7: (EuroPort: TEuroPortUnitData);
                     8: (EuroBuy: TEuroBuyData);
                     9: (EuroTrain: TEuroTrainData);
                     10: (RenameColony: TRenameColonyData);
                     11: (AbandonColony: TAbandonColonyData);
                     12: (ColonyUnit: TColonyUnitData);
                     13: (GotoShip: TGotoShipData);
                     14: (Construction: TConstructionData);
                     15: (FoundingSelect: TFoundingSelectData);
                 end;//rec

const
  { constant record for an empty callback, i.e. "no callback" }
  cEmptyCallback: TCallbackRec =(option: 0; inputText: ''; _type: CBT_ANY; Data: nil);

  procedure CBF_Exit(const option: Integer);
  function CBF_Landfall(const option: Integer; AShip: TUnit; const AType: TUnitType; const x,y: Byte; AMap: TMap): Boolean;

  { handles the given callback record, i.e. calls another function to handle it,
    and returns true on succes. What exactly "success" means, depends on the
    type of the callback.

    parameters:
        cbRec - the TCallbackRec structure which holds the data needed to handle
                the callback
  }
  function HandleCallback(const cbRec: TCallbackRec): Boolean;

implementation

procedure CBF_Exit(const option: Integer);
begin
  if option=1 then halt;
end;//func

function CBF_Landfall(const option: Integer; AShip: TUnit; const AType: TUnitType; const x,y: Byte; AMap: TMap): Boolean;
begin
  if ((option=1) and (AShip<>nil)) then Result:= AShip.UnloadUnit(AType, x,y, AMap)
  else Result:= False;
end;//func

function CBF_BuildColony(const x,y: Byte; {const num_nation: Byte;} ColName: ShortString; founder: TUnit; AData: TData): Boolean;
var i,j: Integer;
begin
  Result:= False;
  if ((ColName='') or (founder=nil) or (AData=nil) or (x>=cMap_X-1)
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
            (AData.NewColony(x,y, founder.GetNation, ColName)).SetUnitInField(i, j, founder);
            AData.GetMap.tiles[x,y].CreateRoad;
            Result:= True;
          end;//if
        end;//if
        j:= j+1;
      end; //while j
      i:= i+1;
    end;//while i
  end;//if
end;//func

function CBF_SaveGame(const option: Integer; AData: TData): Boolean;
var err_str: string;
begin
  err_str:= 'not saved.';
  if (option>0) and (option<65536) then
  begin
    if (AData<>nil) then
    begin
      Result:= AData.SaveData(option, err_str);
    end//if
    else Result:= False;
  end//if
  else Result:= False;
  WriteLn('SaveGame errors: '+err_str);
end;//func

function CBF_LoadGame(const option: Integer; AData: TData): Boolean;
var err_str: string;
begin
  err_str:= 'not loaded.';
  if ((option>0) and (option<65536) and (AData<>nil)) then
  begin
    Result:= AData.LoadData(option, err_str);
  end//if
  else Result:= False;
  WriteLn('LoadGame errors: '+err_str);
end;//if

function CBF_JobChange(const option: Integer; const cbRec: TCallbackRec): Boolean;
var new_good: TGoodType;
begin
  Result:= False;
  if ((abs(cbRec.JobChange.x_shift)>1) or (abs(cbRec.JobChange.y_shift)>1)
     or (cbRec.JobChange.AColony=nil)) then Exit;
  if cbRec.JobChange.AColony.GetUnitInField(cbRec.JobChange.x_shift, cbRec.JobChange.y_shift)=nil then Exit;
  case option of
    0: new_good:= gtFood;
    1: new_good:= gtSugar;
    2: new_good:= gtTobacco;
    3: new_good:= gtCotton;
    4: new_good:= gtFur;
    5: new_good:= gtWood;
    6: new_good:= gtOre;
    7: new_good:= gtSilver;
  else //should not happen
    new_good:= gtFood;
  end;//case
  cbRec.JobChange.AColony.SetUnitInField(cbRec.JobChange.x_shift, cbRec.JobChange.y_shift,
        cbRec.JobChange.AColony.GetUnitInField(cbRec.JobChange.x_shift, cbRec.JobChange.y_shift), new_good);
  Result:= True;
end;//func

function CBF_EuroPortUnit(const option: Integer; AUnit: TUnit; EuroNat: TEuropeanNation): Boolean;
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

function CBF_EuroPortBuy(const option: Integer; AData: TData; EuroNat: TEuropeanNation): Boolean;
var buy_unit: TUnitType;
    new_unit: TUnit;
    start_x, start_y: Byte;
begin
  Result:= (option=0);
  if (EuroNat=nil) or (AData=nil) or (option=0) then Exit;
  case option of
    1: buy_unit:= utCaravel;
    2: buy_unit:= utTradingShip;
    3: buy_unit:= utGalleon;
    4: buy_unit:= utPrivateer;
    5: buy_unit:= utFrigate;
  else buy_unit:= utCriminal;//should not happen
  end;//case
  if buy_unit=utCriminal then Exit;
  //buy it
  If EuroNat.GetGold>=cShipPrices[buy_unit] then
  begin
    if length(AData.GetAllShipsInEurope(EuroNat.GetCount))>0 then
    begin
      new_unit:= AData.GetAllShipsInEurope(EuroNat.GetCount)[0];
      start_x:= new_unit.GetPosX;
      start_y:= new_unit.GetPosY;
    end//if
    else begin
      start_x:= cMap_X-1;
      start_y:= cMap_Y div 2;
    end;//else
    new_unit:= AData.NewUnit(buy_unit, EuroNat.GetCount, start_x, start_y);
    new_unit.SetLocation(ulEurope);
    EuroNat.DecreaseGold(cShipPrices[buy_unit]);
    Result:= True;
  end;//if
end;//func

function CBF_EuroPortTrain(const option: Integer; AData: TData; EuroNat: TEuropeanNation): Boolean;
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
    new_unit:= AData.NewUnit(train_unit, EuroNat.GetCount, cMap_X-1, cMap_Y div 2);
    new_unit.SetLocation(ulEurope);
    new_unit.SetState(usWaitingForShip);
    EuroNat.DecreaseGold(cUnitPrices[train_unit]);
    Result:= True;
  end;//if
end;//func

function CBF_RenameColony(const ACol: TColony; const NewName: ShortString): Boolean;
begin
  if (ACol=nil) then
  begin
    Result:= False;
    Exit;
  end;//if
  ACol.SetName(NewName);
  Result:= True;
end;//func

function CBF_AbandonColony(const option: Integer; ACol: TColony; AData: TData): Boolean;
begin
  Result:= False;
  if ((ACol=nil) or (AData=nil)) then Exit;
  if option=1 then
  begin
    Result:= AData.DeleteColony(ACol.GetPosX, ACol.GetPosY);
  end;//if
end;//func

function CBF_ColonyUnit(const option: Integer; AUnit: TUnit): Boolean;
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

function CBF_GotoShip(const option: Integer; Ship: TUnit; dat: TData): Boolean;
var col_arr: TColonyArr;
    goTask: TGoToTask;
begin
  Result:= False;
  if ((Ship=nil) or (dat=nil) or (option<0)) then Exit;
  if option=0 then Result:= True
  else begin
    col_arr:= dat.GetColonyList(Ship.GetNation);
    if length(col_arr)<option then Exit;
    goTask:= TGoToTask.Create(Ship, col_arr[option-1].GetPosX, col_arr[option-1].GetPosY, dat.GetMap,
               col_arr[option-1].GetPosX, col_arr[option-1].GetPosY);//path with destination as special node
    Ship.SetTask(goTask, True);
    Result:= True;
  end;//else
end;//func

function CBF_Construction(const option: Integer; ACol: TColony): Boolean;
var bt_arr: array of TBuildingType;
    i: Integer;
begin
  Result:= False;
  if (ACol=nil) then Exit;
  if (option=0) then
  begin
    ACol.SetCurrentConstruction(btNone);
    Result:= True
  end
  else begin
    SetLength(bt_arr, 0);
    for i:= Ord(Succ(btNone)) to Ord(High(TBuildingType)) do
    begin
      if (ACol.GetBuildingLevel(TBuildingType(i))<GetMaxBuildingLevel(TBuildingType(i))) then
      begin
        SetLength(bt_arr, length(bt_arr)+1);
        bt_arr[High(bt_arr)]:= TBuildingType(i);
      end;//if
    end;//for
    if (option>High(bt_arr)+1) then Exit;
    ACol.SetCurrentConstruction(bt_arr[option-1]);
    Result:= True;
  end;//else-branch
end;//func

function CBF_FoundingSelect(const option: Integer; ENat: TEuropeanNation; const Choices: TFoundingFatherArray): Boolean;
begin
  Result:= False;
  if ((ENat=nil) or (option<0) or (option>High(Choices))) then Exit;
  //check if Founding father is already present (should never happen)
  if ENat.HasFoundingFather(Choices[option]) then Exit;
  //none is no acceptable selection
  if Choices[option]=ffNone then Exit;
  ENat.SetNextFoundingFather(Choices[option]);
  Result:= True;
end;//func

function HandleCallback(const cbRec: TCallbackRec): Boolean;
begin
  case cbRec._type of
    CBT_ANY: Result:=True; //do nothing here
    CBT_EXIT: begin
                cbRec.cbExit(cbRec.option);
                Result:= True;
              end;
    CBT_LANDFALL: Result:= cbRec.Landfall.cbLandfall(cbRec.option, cbRec.Landfall.Ship,
                    cbRec.Landfall.UType, cbRec.Landfall.x, cbRec.Landfall.y,
                    cbRec.Landfall.AMap);
    CBT_BUILD_COLONY: Result:= CBF_BuildColony(cbRec.BuildColony.x, cbRec.BuildColony.y,
                        cbRec.inputText, cbRec.BuildColony.founder, cbRec.BuildColony.AData);
    CBT_SAVE_GAME: Result:= CBF_SaveGame(cbRec.option, cbRec.SaveGame.AData);
    CBT_LOAD_GAME: Result:= CBF_LoadGame(cbRec.option, cbRec.LoadGame.AData);
    CBT_JOB_CHANGE: Result:= CBF_JobChange(cbRec.option, cbRec);
    CBT_EURO_PORT_UNIT: Result:= CBF_EuroPortUnit(cbRec.option, cbRec.EuroPort.AUnit, cbRec.EuroPort.EuroNat);
    CBT_EURO_PORT_BUY: Result:= CBF_EuroPortBuy(cbRec.option, cbRec.EuroBuy.AData, cbRec.EuroBuy.EuroNat);
    CBT_EURO_PORT_TRAIN: Result:= CBF_EuroPortTrain(cbRec.option, cbRec.EuroTrain.AData, cbRec.EuroTrain.EuroNat);
    CBT_RENAME_COLONY: Result:= CBF_RenameColony(cbRec.RenameColony.AColony, cbRec.inputText);
    CBT_ABANDON_COLONY: Result:= CBF_AbandonColony(cbRec.option, cbRec.AbandonColony.AColony, cbRec.AbandonColony.AData);
    CBT_COLONY_UNIT: Result:= CBF_ColonyUnit(cbRec.option, cbRec.ColonyUnit.AUnit);
    CBT_GOTO_SHIP: Result:= CBF_GoToShip(cbRec.option, cbRec.GotoShip.Ship, cbRec.GotoShip.AData);
    CBT_CONSTRUCTION: Result:= CBF_Construction(cbRec.option, cbRec.Construction.AColony);
    CBT_SELECT_FOUNDING_FATHER: Result:= CBF_FoundingSelect(cbRec.option,
                                         cbRec.FoundingSelect.ENat, cbRec.FoundingSelect.Choices);
  else
    Result:= False; //unknown callback type or type not supported/ implemented
  end;//case
end;//proc

end.
