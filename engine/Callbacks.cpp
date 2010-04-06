#include "Callbacks.h"
#include <iostream>
#include <cstdlib>

TCallbackRec cEmptyCallback()
{
  TCallbackRec temp;
  temp.option=0;
  temp.inputText="";
  temp._type=CBT_ANY;
  temp.Data = NULL;
  return temp;
}

void CBF_Exit(const LongInt option)
{
  if (option==1) exit(0);
};//func

bool CBF_Landfall(const LongInt option, TUnit* AShip, const TUnitType AType, const Byte x, const Byte y, TMap& AMap)
{
  if ((option==1) and (AShip!=NULL)) return AShip->UnloadUnit(AType, x,y, TMap::GetSingleton());
  else return false;
}//func

bool CBF_BuildColony(const Byte x, const Byte y, const Byte num_nation, const std::string& ColName, TUnit* founder, TData& AData)
{
  if ((ColName=="") or (founder==NULL) or /*(AData=nil) or*/ (x>=cMap_X-1)
      or (y>=cMap_Y-1) or (x==0) or (y==0)) return false;
  if (founder->WarpToXY(x,y, TMap::GetSingleton()))
  {
    TColony* tc = AData.NewColony(x,y, num_nation, ColName);
    tc->SetUnitInField(-1, -1, founder);
    //founder.SetLocation(ulInColony);
    TMap::GetSingleton().tiles[x][y]->CreateRoad();
    return true;
  };//if
  return false;
}//func

bool CBF_SaveGame(const LongInt option, TData& AData)
{
  bool Result;
  std::string err_str = "not saved.";
  if ((option>0) and (option<65536))
  {
    /*if (AData<>nil) then
    begin*/
      Result = AData.SaveData(option, err_str);
    /*end//if
    else Result:= False;*/
  }//if
  else Result = false;
  std::cout <<"SaveGame errors: "<<err_str<<"\n";
  return Result;
}//func

bool CBF_LoadGame(const LongInt option, TData& AData)
{
  std::string err_str = "not loaded.";
  bool Result;
  if ((option>0) and (option<65536) /*and (AData<>nil)*/)
  {
    Result = AData.LoadData(option, err_str);
  }//if
  else Result = false;
  std::cout << "LoadGame errors: "<<err_str<<"\n";
  return Result;
}//if

bool CBF_JobChange(const LongInt option, const TCallbackRec cbRec)
{
  if ((abs(cbRec.JobChange->x_shift)>1) or (abs(cbRec.JobChange->y_shift)>1)
     or (cbRec.JobChange->AColony==NULL)) return false;
  if (cbRec.JobChange->AColony->GetUnitInField(cbRec.JobChange->x_shift, cbRec.JobChange->y_shift)==NULL) return false;
  TGoodType new_good;
  switch (option)
  {
    case 0: new_good = gtFood; break;
    case 1: new_good = gtSugar; break;
    case 2: new_good = gtTobacco; break;
    case 3: new_good = gtCotton; break;
    case 4: new_good = gtFur; break;
    case 5: new_good = gtWood; break;
    case 6: new_good = gtOre; break;
    case 7: new_good = gtSilver;break;
    default: //should not happen
            new_good = gtFood; break;
  }//swi
  cbRec.JobChange->AColony->SetUnitInField(cbRec.JobChange->x_shift, cbRec.JobChange->y_shift,
        cbRec.JobChange->AColony->GetUnitInField(cbRec.JobChange->x_shift, cbRec.JobChange->y_shift), new_good);
  return true;
}//func

bool CBF_EuroPortUnit(const LongInt option, TUnit* AUnit, TEuropeanNation* EuroNat)
{
  if ((AUnit==NULL) or (EuroNat==NULL)) return false;
  switch (option)
  {
    case 0: //ship
         if (AUnit->GetState()==usWaitingForShip) AUnit->SetState(usNormal);
         else AUnit->SetState(usWaitingForShip);
         break;
    case 1: //muskets
         if (!EuroNat->IsBoycotted(gtMusket))
         {
           if (AUnit->HasMuskets())
           {
             EuroNat->SellGood(gtMusket, 50);
             AUnit->GiveMuskets(false);
           }//if
           else
           {
             if (EuroNat->GetGold()>=EuroNat->GetPrice(gtMusket, false)*50)
             {
               EuroNat->BuyGood(gtMusket, 50);
               AUnit->GiveMuskets(true);
             }//if
           }//if
         } //if
         break;
    case 2: //horses
         if (!EuroNat->IsBoycotted(gtHorses))
         {
           if (AUnit->HasHorses())
           {
             EuroNat->SellGood(gtHorses, 50);
             AUnit->GiveHorses(false);
           }//if
           else
           {
             if (EuroNat->GetGold()>=EuroNat->GetPrice(gtHorses, false)*50)
             {
               EuroNat->BuyGood(gtHorses, 50);
               AUnit->GiveHorses(true);
             }//if
           }//if
         } //if
         break;
    case 3: //tools
         if (!EuroNat->IsBoycotted(gtTool))
         {
           if (AUnit->GetToolAmount()>0)
           {
             EuroNat->SellGood(gtTool, AUnit->GetToolAmount());
             AUnit->GiveTools(0);
           }//if
           else
           {
             if (EuroNat->GetGold()>=EuroNat->GetPrice(gtTool, false)*(100-AUnit->GetToolAmount()))
             {
               EuroNat->BuyGood(gtTool, 100-AUnit->GetToolAmount());
               AUnit->GiveTools(100);
             }//if
           }//if
         }//if no boycott
         break; //3 (tools)
    case 4: break;//no changes at all
  }//swi
  return true;
}//func

bool CBF_EuroPortBuy(const LongInt option, TData& AData, TEuropeanNation* EuroNat)
{
  if ((EuroNat==NULL) /*or (AData=nil)*/ or (option==0)) return (option==0);
  TUnitType buy_unit;
  switch (option)
  {
    case 1: buy_unit = utCaravel; break;
    case 2: buy_unit = utTradingShip; break;
    case 3: buy_unit = utGalleon; break;
    case 4: buy_unit = utPrivateer; break;
    case 5: buy_unit = utFrigate; break;
    default: buy_unit = utCriminal; break;//should not happen
  }//swi
  if (buy_unit==utCriminal) return false;
  //buy it
  if (EuroNat->GetGold()>=cShipPrices(buy_unit))
  {
    Byte start_x, start_y;
    TUnit* new_unit = NULL;
    if (AData.GetAllShipsInEurope(EuroNat->GetCount()).size() >0)
    {
      new_unit = AData.GetAllShipsInEurope(EuroNat->GetCount())[0];
      start_x = new_unit->GetPosX();
      start_y = new_unit->GetPosY();
    }//if
    else
    {
      start_x = cMap_X-1;
      start_y = cMap_Y / 2;
    }//else
    new_unit = AData.NewUnit(buy_unit, EuroNat->GetCount(), start_x, start_y);
    new_unit->SetLocation(ulEurope);
    EuroNat->DecreaseGold(cShipPrices(buy_unit));
    return true;
  }//if
  return false;
}//func

bool CBF_EuroPortTrain(const LongInt option, TData& AData, TEuropeanNation* EuroNat)
{
  if ((EuroNat==NULL) /*or (AData=nil)*/ or (option==0)) return false;;
  TUnitType train_unit;
  switch (option)
  {
    case 1: train_unit = utFarmer; break;
    case 2: train_unit = utFisher; break;
    case 3: train_unit = utSilverMiner; break;
    case 4: train_unit = utWoodcutter; break;
    case 5: train_unit = utOreMiner; break;
    case 6: train_unit = utPreacher; break;
    case 7: train_unit = utStatesman; break;
    case 8: train_unit = utCarpenter; break;
    case 9: train_unit = utDistiller; break;
    case 10: train_unit = utWeaver; break;
    case 11: train_unit = utTobacconist; break;
    case 12: train_unit = utFurTrader; break;
    case 13: train_unit = utSmith; break;
    case 14: train_unit = utWeaponSmith; break;
    case 15: train_unit = utPioneer; break;
    case 16: train_unit = utMissionary; break;
    case 17: train_unit = utRegular; break;
    default: train_unit = utCriminal; break;//should not happen here
  }//swi
  if (train_unit==utCriminal) return false;
  if (EuroNat->GetGold()>=cUnitPrices(train_unit))
  {
    TUnit* new_unit = AData.NewUnit(train_unit, EuroNat->GetCount(), cMap_X-1, cMap_Y / 2);
    new_unit->SetLocation(ulEurope);
    new_unit->SetState(usWaitingForShip);
    EuroNat->DecreaseGold(cUnitPrices(train_unit));
    return true;
  }//if
  return false;
}//func

bool CBF_RenameColony(TColony* ACol, const std::string& NewName)
{
  if (ACol==NULL)
    return false;
  ACol->SetName(NewName);
  return true;
}//func

bool CBF_AbandonColony(const LongInt option, TColony* ACol, TData& AData)
{
  if ((ACol==NULL) /*or (AData=nil)*/) return false;
  if (option==1)
  {
    return AData.DeleteColony(ACol->GetPosX(), ACol->GetPosY());
  }//if
  return false;
}//func

bool CBF_ColonyUnit(const LongInt option, TUnit* AUnit)
{
  if (AUnit==NULL)
    return false;
  switch (option)
  {
    case 0: switch (AUnit->GetState())
            {
              case usFortified:
              case usWaitingForShip:
                   AUnit->SetState(usNormal);
                   break;
              default:
                   AUnit->SetState(usWaitingForShip);
                   break;
            }//swi
            break;
    case 1: switch (AUnit->GetState())
            {
              case usFortified:
                   AUnit->SetState(usWaitingForShip);
                   break;
              case usWaitingForShip:
              case usNormal:
                   AUnit->SetState(usFortified);
                   break;
            }//swi
  //2: keine Veränderung/ no changes
    default: return false; break;
  }//swi
  return true;
}//func

bool CBF_GotoShip(const LongInt option, TUnit* Ship, TData& dat)
{
  if ((Ship==NULL) /*or (dat=nil)*/ or (option<0)) return false;
  if (option==0) return true;
  else
  {
    TColonyArr col_arr = dat.GetColonyList(Ship->GetNation());
    if (col_arr.size()<option) return false;
    TGoToTask* goTask = new TGoToTask(Ship, static_cast<Byte> (col_arr[option-1]->GetPosX()), static_cast<Byte>(col_arr[option-1]->GetPosY()),
               col_arr[option-1]->GetPosX(), col_arr[option-1]->GetPosY());//path with destination as special node
    Ship->SetTask(goTask, true);
    return true;
  }//else
}//func

bool CBF_Construction(const LongInt option, TColony* ACol)
{
  if (ACol==NULL) return false;
  if (option==0)
  {
    ACol->SetCurrentConstruction(btNone);
    return true;
  }
  else
  {
    std::vector<TBuildingType> bt_arr;
    bt_arr.clear();
    LongInt i;
    for (i= Ord(Succ(btNone)); i<=Ord(High(btNone)); ++i)
    {
      if (ACol->GetBuildingLevel(TBuildingType(i))<GetMaxBuildingLevel(TBuildingType(i)))
      {
        /*SetLength(bt_arr, length(bt_arr)+1);
        bt_arr[High(bt_arr)]:= TBuildingType(i);*/
        bt_arr.push_back(TBuildingType(i));
      }//if
    }//for
    if (option>bt_arr.size()) return false;
    ACol->SetCurrentConstruction(bt_arr.at(option-1));
    return true;
  }//else-branch
}//func

bool HandleCallback(const TCallbackRec cbRec)
{
  switch (cbRec._type)
  {
    case CBT_ANY: return true; //do nothing here
    case CBT_EXIT:
         CBF_Exit(cbRec.option);
         return true;
         break;
    case CBT_LANDFALL:
         return cbRec.Landfall->cbLandfall(cbRec.option, cbRec.Landfall->Ship,
                    cbRec.Landfall->UType, cbRec.Landfall->x, cbRec.Landfall->y,
                    cbRec.Landfall->AMap);
         break;
    case CBT_BUILD_COLONY:
         return CBF_BuildColony(cbRec.BuildColony->x, cbRec.BuildColony->y,
                        cbRec.BuildColony->num_nation, cbRec.inputText,
                        cbRec.BuildColony->founder, cbRec.BuildColony->AData);
         break;
    case CBT_SAVE_GAME:
         return CBF_SaveGame(cbRec.option, cbRec.SaveGame->AData);
         break;
    case CBT_LOAD_GAME:
         return CBF_LoadGame(cbRec.option, cbRec.LoadGame->AData);
         break;
    case CBT_JOB_CHANGE:
         return CBF_JobChange(cbRec.option, cbRec);
         break;
    case CBT_EURO_PORT_UNIT:
         return CBF_EuroPortUnit(cbRec.option, cbRec.EuroPort->AUnit, cbRec.EuroPort->EuroNat);
         break;
    case CBT_EURO_PORT_BUY:
         return CBF_EuroPortBuy(cbRec.option, cbRec.EuroBuy->AData, cbRec.EuroBuy->EuroNat);
         break;
    case CBT_EURO_PORT_TRAIN:
         return CBF_EuroPortTrain(cbRec.option, cbRec.EuroTrain->AData, cbRec.EuroTrain->EuroNat);
         break;
    case CBT_RENAME_COLONY:
         return CBF_RenameColony(cbRec.RenameColony->AColony, cbRec.inputText);
         break;
    case CBT_ABANDON_COLONY:
         return CBF_AbandonColony(cbRec.option, cbRec.AbandonColony->AColony, cbRec.AbandonColony->AData);
         break;
    case CBT_COLONY_UNIT:
         return CBF_ColonyUnit(cbRec.option, cbRec.ColonyUnit->AUnit);
         break;
    case CBT_GOTO_SHIP:
         return CBF_GotoShip(cbRec.option, cbRec.GotoShip->Ship, cbRec.GotoShip->AData);
         break;
    case CBT_CONSTRUCTION:
         return CBF_Construction(cbRec.option, cbRec.Construction->AColony);
         break;
    default:
         return false; //unknown callback type or type not supported/ implemented
         break;
  }//swi
}//func
