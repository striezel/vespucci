/* **************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2009, 2010, 2011, 2015  Thoronador

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
*/

#include "Units.hpp"
#include <iostream>
//#include "Colony.hpp"
//#include "Data.hpp"
#include "DebugWriter.hpp"

//helper procedure
void ApplyDir(Byte& x, Byte& y, const TDirection dir)
{
  switch (dir)
  {
    case dirW:
    case dirSW:
    case dirNW:
         if (x>0) x = x + 1;
         break;
    case dirE:
    case dirNE:
    case dirSE:
         if (x<cMap_X-1) x = x -1;
         break;
  }//swi
  switch (dir)
  {
    case dirNW:
    case dirN:
    case dirNE:
         if (y>0) y = y - 1;
         break;
    case dirSW:
    case dirS:
    case dirSE:
         if (y<cMap_Y-1) y = y + 1;
  }//swi
}//proc

TDirection GetApplyingDirection(const Byte from_x, const Byte from_y, const Byte to_x, const Byte to_y)
{
  if ((abs(from_x-to_x)>1) || (abs(from_y-to_y)>1)) return dirNone;
  else
  {
    switch (to_x-from_x)
    {
      case 1:
           switch (to_y-from_y)
           {
             case -1: return dirNE;
             case 0:  return dirE;
             case 1:  return dirSE;
           }//swi
           break;
      case 0:
           switch (to_y-from_y)
           {
             case -1: return dirN;
             case 0:  return dirNone;
             case 1:  return dirS;
           }//swi
           break;
      case -1:
           switch (to_y-from_y)
           {
             case -1: return dirNW;
             case 0:  return dirW;
             case 1:  return dirSW;
           }//swi
           break;
    }//swi
  }//else
}//func

// ***************
// *TUnit methods*
// ***************

TUnit::TUnit(const TUnitType TypeOfUnit, const int ANation, int X, int Y)
{
  UnitType = TypeOfUnit;
  PosX = X;
  PosY = Y;
  m_location = ulAmerica;
  m_State = usNormal;
  m_RoundsInOpenSea = 0;
  MovesLeft = MovesPerRound();
  m_Nation = ANation;
  items = 0;
  AI_Task = nullptr;
  if (TypeOfUnit == utPioneer)
    GiveTools(100);
  else if ((TypeOfUnit == utRegular) || (TypeOfUnit == utDragoon))
    GiveMuskets(true);
  if ((TypeOfUnit == utScout) or (TypeOfUnit == utDragoon) or (TypeOfUnit == utBraveOnHorse))
    GiveHorses(true);
  int i;
  for (i = 0; i<=5; ++i)
  {
    passengers[i] = nullptr;
    cargo_load[i].amount = 0;
    cargo_load[i].which = gtFood;
  }//for
}//construc

TUnit::~TUnit()
{
}//destruc

void TUnit::NewRound()
{
  //regain moves
  MovesLeft = MovesPerRound();
  //check for passage over the pond
  if (m_RoundsInOpenSea>0)
  {
    m_RoundsInOpenSea = m_RoundsInOpenSea-1;
    if (m_RoundsInOpenSea == 0)
    {
      switch (m_location)
      {
        case ulGoToNewWorld:
             m_location = ulAmerica;
             break;
        case ulGoToEurope:
             m_location = ulEurope;
             DropAllPassengers();
             break; //case GoToEurope
      }//swi
    }//if
  }//if

  //check for task and execute, if present
  if (AI_Task!=nullptr)
  {
    WriteDebugLn("New Round: Exec calling");
    AI_Task->Execute();

    if (AI_Task->Done())
    {
      AI_Task->Destroy();
      AI_Task = nullptr;
    }
  }
}//proc

bool TUnit::Move(const TDirection direction, const std::shared_ptr<TMap> AMap, const void * dat)
{
  int newX, newY;
  if (MovesLeft <= 0)
    return false;
  else
  {
    switch (direction)
    {
      case dirW:
      case dirSW:
      case dirNW:
           newX = PosX-1;
           break;
      case dirE:
      case dirNE:
      case dirSE:
           newX = PosX+1;
           break;
      default:
           newX = PosX;
           break;
    }//switch
    switch (direction)
    {
      case dirNW:
      case dirN:
      case dirNE:
           newY = PosY-1;
           break;
      case dirSW:
      case dirS:
      case dirSE:
           newY = PosY+1;
           break;
      default:
           newY = PosY;
           break;
    }//case
    bool allow = true;
    //check if we are out of map
    if ((newX<0) or (newY<0) or (newX>=cMap_X) or (newY>=cMap_Y))
      allow = false;
    else
    {
      if (AMap != nullptr)
        if (AMap->tiles[newX][newY] != nullptr)
        {
          allow = (IsShip() === AMap->tiles[newX][newY]->IsWater());
        }//if
    }//else

    if (allow)
    {
      //check ships for european route
      if (IsShip() and (AMap!=nullptr) and (AI_Task==nullptr)) //no european route for non-ships or AI tasks
        if ((AMap->tiles[PosX][PosY]->GetType()==ttOpenSea) and (AMap->tiles[newX][newY]->GetType()==ttOpenSea))
        {
          SendToEurope();
          return true;
        }//if
      if (direction!=dirNone)
      {
        //check ships
        if (IsShip() and (AMap!=nullptr))
        {
          if ((not AMap->tiles[PosX][PosY]->IsWater()) and (dat!=nullptr))
          {
            TColony tempCol = static_cast<TData>(dat).GetColonyInXY(PosX, PosY);
            if (tempCol!=nullptr)
            {
              TUnitArr u_arr = static_cast<TData>(dat).GetAllUnitsInColony(tempCol);
              int i = 0;
              while ((FreeCapacity()>0) and (i<u_arr.size()))
              {
                if (u_arr[i]->GetState()==usWaitingForShip)
                  LoadUnit(u_arr[i]);
                i = i+1;
              }//while
            }
          }//if
        }//if ship and map present
        //do the actual move
        MovesLeft = MovesLeft -1;
        PosX = newX;
        PosY = newY;
      }//if
      if ((AMap!=nullptr) and (m_Nation!=0))
        AMap->DiscoverSurroundingTiles(newX, newX, m_Nation, UnitType==utScout);
      return true;
    }//if
    else
        return false;
  }//else
}//func

bool TUnit::WarpToXY(const Byte x, const Byte y, std::shared_ptr<TMap> AMap)
{
  if ((x>=cMap_X) or (y>=cMap_Y))
    return false;
  else
  {
    PosX = x;
    PosY = y;
    if ((AMap!=nullptr) and (m_Nation!=0))
      AMap->DiscoverSurroundingTiles(x,y, m_Nation, UnitType==utScout);
    return true;
  }
}//func

LongInt TUnit::GetPosX() const
{
  return PosX;
}

LongInt TUnit::GetPosY() const
{
  return PosY;
}

LongInt TUnit::GetNation() const
{
  return m_Nation;
}//func

void TUnit::ChangeNation(const LongInt new_nation)
{
  m_Nation = new_nation;
}//proc

TUnitType TUnit::GetType() const
{
  return UnitType;
}//func

void TUnit::ChangeType(const TUnitType newType)
{
  //we don't wanna change ships' type or convoy
  if ((not IsShip()) and (UnitType!=utConvoy))
    UnitType = newType;
}//proc

TUnitLocation TUnit::GetLocation() const
{
  return m_location;
}//func

void TUnit::SetLocation(const TUnitLocation loc)
{
  m_location = loc;
}//proc

TUnitState TUnit::GetState() const
{
  return m_State;
}//func

void TUnit::SetState(const TUnitState state)
{
  m_State = state;
}//proc

Byte TUnit::GetRoundsInOpenSea() const
{
  return m_RoundsInOpenSea;
}//func

void TUnit::SetRoundsInOpenSea(const Byte rounds_left)
{
  m_RoundsInOpenSea = rounds_left;
}//proc

bool TUnit::IsShip() const
{
  return ((UnitType==utCaravel) or (UnitType==utTradingShip) or (UnitType==utGalleon) or (UnitType==utPrivateer)
       or (UnitType==utFrigate) or (UnitType==utMan_o_War));
}//func

int TUnit::MovesPerRound() const
{
  switch (UnitType)
  {
    case utScout:
    case utDragoon:
         return 4;
    case utMissionary:
    case utConvoy:
         return 2;
    case utCaravel:
         return 4;
    case utTradingShip:
         return 5;
    case utGalleon:
         return 6;
    case utPrivateer:
         return 8;
    case utFrigate:
         return 6;
    case utMan_o_War:
         return 5;
    case utBraveOnHorse:
         return 4;
    default:
         return 1;
  }//case
}//func

int TUnit::AttackStrength() const
{
  int Result = 0;
  switch (UnitType)
  {
    case utRegular:
         Result = 2;
         break;
    case utDragoon:
         Result = 3;
         break;
    case utCaravel:
         Result = 2;
         break;
    case utTradingShip:
         Result = 6;
         break;
    case utGalleon:
         Result = 10;
         break;
    case utPrivateer:
         Result = 8;
         break;
    case utFrigate:
         Result = 16;
         break;
    case utMan_o_War:
         Result = 24;
         break;
    //utBrave: Result:= 1;
    case utBraveOnHorse:
         Result = 2;
         break;
    default:
         Result = 1;
         break;
  }//switch
  if (m_State==usFortified) return (Result*3) / 2;
  return Result;
}//func

std::shared_ptr<TTask> TUnit::GetTask() const
{
  return AI_Task;
}//func

void TUnit::SetTask(const std::shared_ptr<TTask> new_task, const bool ImmediateExecute)
{
  if (AI_Task!=nullptr)
    AI_Task = nullptr; //AI_Task.Destroy;
  AI_Task = new_task;
  if ((AI_Task!=nullptr) and ImmediateExecute)
    AI_Task->Execute();
}//proc

bool TUnit::SendToEurope()
{
  if ((m_location!=ulAmerica) or not IsShip()) return false;
  else
  {
    m_RoundsInOpenSea = 2;
    MovesLeft = 0;
    m_location = ulGoToEurope;
    return true;
  }//else
}//func

bool TUnit::SendToNewWorld()
{
  if ((m_location!=ulEurope) or not IsShip()) return false;
  else
  {
    m_RoundsInOpenSea = 2;
    MovesLeft = 0;
    m_location = ulGoToNewWorld;
    return true;
  }//else
}//func

bool TUnit::CallBackToEurope()
{
  if ((m_location!=ulGoToNewWorld) or not IsShip()) return false;
  else
  {
    m_RoundsInOpenSea = 2-m_RoundsInOpenSea;
    MovesLeft = 0;
    m_location = ulGoToEurope;
    if (m_RoundsInOpenSea<=0)
    {
      m_location = ulEurope;
      DropAllPassengers();
    }//if
    return true;
  }//else
}//func

bool TUnit::CallBackToNewWorld()
{
  if ((m_location!=ulGoToEurope) or not IsShip()) return false;
  else
  {
    m_RoundsInOpenSea = 2-m_RoundsInOpenSea;
    MovesLeft = 0;
    m_location = ulGoToNewWorld;
    if (m_RoundsInOpenSea<=0)
      m_location = ulAmerica;
    return true;
  }//else
}//func

Byte TUnit::FreightCapacity() const
{
  switch (UnitType)
  {
    case utConvoy:
    case utCaravel:
    case utPrivateer:
         return 2;
    case utTradingShip:
    case utFrigate:
         return 4;
    case utGalleon:
    case utMan_o_War:
         return 6;
    default:
         return 0;
  }//case
}//func

Byte TUnit::FreeCapacity() const
{
  if (FreightCapacity()==0) return 0;
  else
  {
    Byte occupied = 0;
    int i;
    for (i = 0; i<= 5; ++i)
    {
      if (passengers[i]!=nullptr)
        occupied = occupied+1;
      if (cargo_load[i].amount>0)
        occupied = occupied+1;
    }//for
    if (FreightCapacity()<=occupied) return 0;
    else return FreightCapacity() - occupied;
  }//else
}//func

Byte TUnit::EmbarkedPassengers() const
{
  Byte Result = 0;
  int i;
  for (i = 0; i<= 5; ++i)
    if (passengers[i]!=nullptr)
      Result = Result+1;
  return Result;
}//func

std::shared_ptr<TUnit> TUnit::GetFirstEmbarkedPassenger() const
{
  std::shared_ptr<TUnit> Result = nullptr;
  int i;
  for (i = 5; i >= 0; --i)
  {
    if (passengers[i]!=nullptr)
      Result = passengers[i];
  }//for
  return Result;
}//func

std::shared_ptr<TUnit> TUnit::GetPassengerBySlot(const Byte slot) const
{
  if (slot>5) return nullptr;
  else return passengers[slot];
}//func

/*tries to load num units of good 'AGood'; maximum is 100
 TO-DO: function still uses a new slot for every good, even if there already is
 ****** an amount of the given good loaded (e.g. trying to load 20 food and
        then again less then 80 food will occupy two slots, even though a slot
        is able to store up to 100 units of a good.
*/
bool TUnit::LoadGood(const TGoodType AGood, const Byte num)
{
  if ((num>100) or (FreeCapacity()==0)) return false;
  else
  {
    Byte slot = 0;
    while ((cargo_load[slot].amount!=0) and (slot<5))
      slot = slot+1;
    cargo_load[slot].amount = num;
    cargo_load[slot].which = AGood;
    return true;
  }//else
}//func

//tries to unload 'num' units of good 'AGood' and returns number of unloaded units
Byte TUnit::UnloadGood(const TGoodType AGood, const Byte num)
{
  Byte Result =0;
  if (FreightCapacity()>0)
  {
    int slot = 5; //needs to be signed type, because it can get negative (-1)
    Byte cap;
    while ((slot>=0) and (Result<num))
    {
      if (cargo_load[slot].which==AGood)
      {
        if (cargo_load[slot].amount<num-Result)
          cap = cargo_load[slot].amount;
        else
          cap = num-Result;
        Result = Result+cap;
        cargo_load[slot].amount = cargo_load[slot].amount - cap;
      }//if
      slot = slot - 1;
    }//while
  }//if
  return Result;
}//func

//tries to load unit 'AUnit' and returns True on success
bool TUnit::LoadUnit(std::shared_ptr<TUnit> AUnit)
{
  if ((FreeCapacity()==0) or (AUnit==nullptr) or (UnitType==utConvoy)) return false;
  else if (AUnit.FreightCapacity()>0) return false; //no ships or convoys
  else
  {
    Byte slot = 0;
    while ((passengers[slot]!=nullptr) and (slot<5))
      slot = slot+1;
    passengers[slot] = AUnit;
    AUnit->SetLocation(ulEmbarked);
    AUnit->SetState(usWaitingForShip);
    return true;
  }//else
}//func

/*tries to unload a unit and place it at the given coordinates
 -Return value: true on success, false otherwise
 -TODO: unloads first unit of given type, so if there are two ore more units of
  ===== the same type loaded, then it migth unload the wrong one*/
bool TUnit::UnloadUnit(const TUnitType AType, const Byte x, const Byte y, TMap& AMap)
{
  if (FreightCapacity()<=0) return false;
  if ((sqr(x-GetPosX)>1) or (sqr(y-GetPosY)>1)) return false;
  int i;
  for (i = 5; i >= 0; --i)
  {
    if (passengers[i]!=nullptr)
      if (passengers[i]->GetType()==AType)
        if (passengers[i]->WarpToXY(x,y,AMap))
        {
          passengers[i]->SetLocation(this->GetLocation());
          passengers[i]->SetState(usNormal);
          passengers[i] = nullptr;
          return true;
        }//if
  }//for
  return false;
}//func

void TUnit::DropAllPassengers()
{
  Byte slot;
  for (slot = 0; slot<=5; ++slot)
  {
    if (passengers[slot]!=nullptr)
    {
      if (m_location == ulEurope)
        passengers[slot]->SetLocation(ulEurope);
      else
      {
        passengers[slot]->SetLocation(ulAmerica);
        passengers[slot]->WarpToXY(PosX, PosY, nullptr);
      }//else
      passengers[slot]->SetState(usNormal);
      passengers[slot] = nullptr;
    }//if
  }//for
}//proc

Byte TUnit::GetToolAmount() const
{
  return (items bitand UNIT_TOOL_MASK)*20;
}//func

void TUnit::GiveTools(const Byte amount)
{
  Byte temp = amount / 20;
  if (temp>5) temp = 5;
  items = temp bitor (items bitand (UNIT_HORSE_BIT bitor UNIT_MUSKET_BIT));
}//proc

bool TUnit::HasHorses() const
{
  return ((items bitand UNIT_HORSE_BIT)>0);
}//func

void TUnit::GiveHorses(const bool has)
{
  if (has)
  {
    items = (items bitor UNIT_HORSE_BIT);
    if (UnitType==utRegular) UnitType = utDragoon;
    if (UnitType==utBrave) UnitType = utBraveOnHorse;
  }//if
  else
  {
    items = (items bitand (!UNIT_HORSE_BIT));
    if (UnitType==utDragoon) UnitType = utRegular;
    if (UnitType==utBraveOnHorse) UnitType = utBrave;
  }//else
}//proc

bool TUnit::HasMuskets() const
{
  return ((items bitand UNIT_MUSKET_BIT)>0);
}//func

void TUnit::GiveMuskets(const bool has)
{
  if (has) items = (items bitor UNIT_MUSKET_BIT);
  else items = (items bitand (!UNIT_MUSKET_BIT));
}//proc

void TUnit::ChangeAllItems(const Byte new_items)
{
  items = new_items;
}//proc

/* saving function, returns true on success.
  Loading function is part of TData (to keep data integrity).*/
bool TUnit::SaveToStream(std::ofstream& fs) const
{
  if (!fs.is_open())
  {
    return false;
  }//if
  fs.write(reinterpret_cast<const char*>(&MovesLeft), sizeof(MovesLeft));
  fs.write(reinterpret_cast<const char*>(&PosX), sizeof(PosX));
  fs.write(reinterpret_cast<const char*>(&PosY), sizeof(PosY));
  fs.write(reinterpret_cast<const char*>(&UnitType), sizeof(TUnitType));
  fs.write(reinterpret_cast<const char*>(&m_location), sizeof(TUnitLocation));
  fs.write(reinterpret_cast<const char*>(&m_State), sizeof(TUnitState));
  fs.write(reinterpret_cast<const char*>(&m_RoundsInOpenSea), sizeof(Byte));
  fs.write(reinterpret_cast<const char*>(&m_Nation), sizeof(LongInt));
  fs.write(reinterpret_cast<const char*>(&items), sizeof(items));
  //save cargo
  int i;
  for (i = 0; i <= 5; ++i)
  {
    fs.write(reinterpret_cast<const char*>(&cargo_load[i].amount), sizeof(Byte));
    fs.write(reinterpret_cast<const char*>(&cargo_load[i].which), sizeof(TGoodType));
  }//for
  //save passengers
  Byte pass_count = EmbarkedPassengers();
  fs.write(reinterpret_cast<const char*>(&pass_count), sizeof(Byte));
  for (i = 0; i<= 5; ++i)
    if (passengers[i]!=nullptr)
      if (!passengers[i]->SaveToStream(fs))
        return false;
  //********* save tasks needs to be done yet! *********
  return fs.good();
}//func

Byte TUnit::GetCargoAmountBySlot(const Byte slot) const
{
  if (slot<=5) return cargo_load[slot].amount;
  else return 0;
}//func

TGoodType TUnit::GetCargoGoodBySlot(const Byte slot) const
{
  if (slot<=5) return cargo_load[slot].which;
  else return gtCross;
}//func


//only used during loading routine
void TUnit::SetCargo(const Byte slot, const Byte new_amount, const TGoodType AGood)
{
  if ((FreightCapacity()==0) or (slot>5) or (new_amount>100)) return;
  else
  {
    cargo_load[slot].amount = new_amount;
    cargo_load[slot].which = AGood;
  }//else
}//func

//**** AI-related functions ****

TTask::TTask(const std::shared_ptr<TUnit> target_unit)
{
  target = target_unit;
}//construc

TTask::~TTask()
{
  if (target!=nullptr) target->SetState(usNormal);
}//destruc

TTaskType TTask::GetType() const
{
  return ttGeneric;
}//func

//**** TPloughTask methods ****

TPloughTask::TPloughTask(const std::shared_ptr<TUnit> target_unit, Byte X, Byte Y, const std::shared_ptr<TMap> AMap)
: TTask(target_unit)
{
  m_X = X;
  m_Y = Y;
  m_Map = AMap;
  RoundsLeft = 4;
  if (target!=nullptr)
  {
    target_unit->SetState(usPloughing);
    if (target->GetType() == utPioneer) RoundsLeft = 2;
  }
  if (AMap->tiles[m_X][m_Y]->IsPloughed()) RoundsLeft = 0;
}//constructor

bool TPloughTask::Done() const
{
  return (RoundsLeft<=0);
}//func

bool TPloughTask::Execute()
{
  if (RoundsLeft>0)
  {
    RoundsLeft = RoundsLeft-1;
    target->MovesLeft = 0;
    if (RoundsLeft==0)
    {
      target->GiveTools(target->GetToolAmount()-20);
      m_Map->tiles[m_X][m_Y]->Plough();
    }
    return true;
  }//if
  else return false;
}//func

TTaskType TPloughTask::GetType() const
{
  return ttPlough;
}//func

// **** TRoadTask methods ****
TRoadTask::TRoadTask(const std::shared_ptr<TUnit> target_unit, Byte X, Byte Y, const std::shared_ptr<TMap> AMap)
: TTask(target_unit)
{
  m_X = X;
  m_Y = Y;
  m_Map = AMap;
  RoundsLeft = 2;
  if (target!=nullptr)
  {
    target->SetState(usCreateRoad);
    if (target->GetType() == utPioneer) RoundsLeft = 1;
  }
  if (AMap->tiles[m_X][m_Y]->HasRoad()) RoundsLeft = 0;
}//func

bool TRoadTask::Done() const
{
  return (RoundsLeft<=0);
}//func

bool TRoadTask::Execute()
{
  if (RoundsLeft>0)
  {
    RoundsLeft = RoundsLeft-1;
    target->MovesLeft = 0;
    if (RoundsLeft==0)
    {
      target->GiveTools(target->GetToolAmount()-20);
      m_Map->tiles[m_X][m_Y]->CreateRoad();
    }
    return true;
  }//if
  else return false;
}//func

TTaskType TRoadTask::GetType() const
{
  return ttRoad;
}//func

// **** TClearTask methods ****
TClearTask::TClearTask(const std::shared_ptr<TUnit> target_unit, Byte X, Byte Y, const std::shared_ptr<TMap> AMap)
: TTask(target_unit)
{
  m_X = X;
  m_Y = Y;
  m_Map = AMap;
  RoundsLeft = 6;
  if (target!=nullptr)
  {
    target->SetState(usPloughing);
    if (target->GetType() == utPioneer) RoundsLeft = 3;
  }
  if (!(AMap->tiles[m_X][m_Y]->HasForest()) RoundsLeft = 0;
}//func

bool TClearTask::Done() const
{
  return (RoundsLeft<=0);
}//func

bool TClearTask::Execute()
{
  if (RoundsLeft>0)
  {
    RoundsLeft = RoundsLeft-1;
    target->MovesLeft = 0;
    if (RoundsLeft==0)
    {
      target->GiveTools(target->GetToolAmount()-20);
      m_Map->tiles[m_X][m_Y]->ClearForest();
    }
    return true;
  }//if
  else return false;
}//func

TTaskType TClearTask::GetType() const
{
  return ttClear;
}//func

// go to task (pathfinding)
TGoToTask::TGoToTask(const std::shared_ptr<TUnit> target_unit, Byte ToX, Byte ToY, const std::shared_ptr<TMap> AMap, const Byte SpecialX, const Byte SpecialY)
: TTask(target_unit),
  m_Path(TCoordArr())
{
  m_X = ToX;
  m_Y = ToY;
  spec_X = SpecialX;
  spec_Y = SpecialY;
  m_Map = AMap;
  m_Path.clear();
  if (FindPath(target_unit->GetPosX(), target_unit->GetPosY(), ToX, ToY, target_unit->IsShip(), AMap, m_Path, SpecialX, SpecialY))
  {
    target_unit->SetState(usGoTo);
  }
  else m_Path.clear();
}//construc

bool TGoToTask::Done() const
{
  return (((target->GetPosX()==m_X) and (target->GetPosY()==m_Y)) or (m_Path.size()<1));
}//func

bool TGoToTask::Execute()
{
  #ifdef DEBUG_CODE
  std::cout << "GoTo.Execute called. Path len: " << m_Path.size() << "\n";
  #endif
  while ((target->MovesLeft>0) and (!m_Path.empty()))
  {
    Byte x = m_Path[m_Path.size()-1].x;
    Byte y = m_Path[m_Path.size()-1].y;
    TDirection direc = GetApplyingDirection(target->GetPosX(), target->GetPosY(), x,y);

    //debug only
    #ifdef DEBUG_CODE
    WriteDebugLn("-GoTo.Execute:");
    std::cout << "-- from: " << target->GetPosX() << "," << target->GetPosY() << "  to: " << (int) x << "," << (int) y << "\n";
    WriteLn('-- apply dir.: ', Ord(direc));
    #endif
    //end debug

    target.Move(direc, m_Map, nullptr);
    if ((target->GetPosX()!=x) or (target->GetPosY()!=y))
    {
      //check for special location
      if ((x==spec_X) and (y==spec_Y))
      {
        target->WarpToXY(spec_X, spec_Y, m_Map);
        target->MovesLeft = target->MovesLeft-1;
        if (target->MovesLeft<0) target->MovesLeft = 0;
      }
      else
      {
        //something went wrong here, abort the whole task
        m_Path.clear();
        //debug only
        std::cout << "-- direction application failed!\n";
        //end debug
        return false;
      }//else
    }//if
    else m_Path.pop_back(); //remove last waypoint
  }//while
  return true;
}//func

Byte TGoToTask::DestinationX() const
{
  return m_X;
}//func

Byte TGoToTask::DestinationY() const
{
  return m_Y;
}//func

TGoToTask::~TGoToTask()
{
  m_Path.clear();
  target->SetState(usNormal);
  //inherited Destroy;
}//destruc

TTaskType TGoToTask::GetType() const
{
  return ttGoTo;
}//func

// **** TGoToEuropeTask methods ****

TGoToEuropeTask::TGoToEuropeTask(const std::shared_ptr<TUnit> target_unit, Byte ToX, Byte ToY, const std::shared_ptr<TMap> AMap)
: TGoToTask(target_unit, ToX, ToY, AMap)
{
}//construc

TGoToEuropeTask::~TGoToEuropeTask()
{
  //inherited Destroy;
}//destruc

bool TGoToEuropeTask::Done() const
{
  return ((TGoToTask::Done()) and (target->GetLocation()==ulEurope));
}//func

bool TGoToEuropeTask::Execute()
{
  if (!TGoToTask::Done())
  {
    TGoToTask::Execute();
    return true;
  }
  else
  {
    if (target->GetLocation()==ulAmerica)
    {
      if (m_Map->tiles[target->GetPosX()][target->GetPosY()]->GetType()==ttOpenSea)
        target->SendToEurope();
      return true;
    }//if
    else return (target->GetLocation()==ulGoToEurope);
  }//else
}//func

TTaskType TGoToEuropeTask::GetType() const
{
  return ttGoToEurope;
}//func

// **** TFindLandForColonyTask methods ****

TFindLandForColonyTask::TFindLandForColonyTask(const std::shared_ptr<TUnit> target_unit, const std::shared_ptr<TMap> AMap, const void * dat)
{
  int target_coords_x, target_coords_y;
  TCoordArr path;

  target = target_unit;
  m_Map = AMap;
  m_Data = dat;
  //get grid
  TColonySiteEvaluationGrid grid = static_cast<TData>(m_Data).GetColonySiteEvaluation(true);
  //find best tile
  bool found = false;
  int step = 1;
  while ((!found) and (step<cMap_Y))
  {
    //check top row
    int i;
    for (i = target_unit->GetPosX()-step; i <= target_unit->GetPosX()+step; ++i)
      if (m_Map->IsValidMapPosition(i, target_unit->GetPosY()-step))
        //check, if it has a high enough value
        if (grid[i][target_unit->GetPosY()-step]>=8)
          //check if there would be a path to it
          if (FindPath(target_unit->GetPosX(), target_unit->GetPosY(), i, target_unit->GetPosY()-step,
                      true, m_Map, path, i, target_unit->GetPosY()-step))
          {
            //found a path to that square
            found = true;
            target_coords_x = i;
            target_coords_y = target_unit->GetPosY()-step;
            break;
          }//if
    if (found)
      break;
    //check bottom row
    for (i = target_unit->GetPosX()-step; i <= target_unit->GetPosX()+step; ++i)
      if (m_Map->IsValidMapPosition(i, target_unit->GetPosY()+step))
        //check, if it has a high enough value
        if (grid[i][target_unit->GetPosY()+step]>=8)
          //check if there would be a path to it
          if (FindPath(target_unit->GetPosX(), target_unit->GetPosY(), i, target_unit->GetPosY()+step,
                      true, m_Map, path, i, target_unit->GetPosY()+step))
          {
            //found a path to that square
            found = true;
            target_coords_x = i;
            target_coords_y = target_unit->GetPosY()+step;
            break;
          }//if
    if (found)
      break;
    //check left row
    for (i = target_unit->GetPosY()-step+1; i <= target_unit->GetPosY()+step-1; ++i)
      if (m_Map->IsValidMapPosition(target_unit->GetPosX-step, i))
        //check, if it has a high enough value
        if (grid[target_unit->GetPosX()-step][i]>=8)
          //check if there would be a path to it
          if (FindPath(target_unit->GetPosX(), target_unit->GetPosY(), target_unit->GetPosX()-step, i,
                      true, m_Map, path, target_unit->GetPosX()-step, i))
          {
            //found a path to that square
            found = true;
            target_coords_x = target_unit->GetPosX()-step;
            target_coords_y = i;
            break;
          }//if
    if (found)
      break;
    //check right row
    for (i = target_unit->GetPosY()-step+1; i <= target_unit->GetPosY()+step-1; ++i)
      if (m_Map->IsValidMapPosition(target_unit->GetPosX()+step, i))
        //check, if it has a high enough value
        if (grid[target_unit->GetPosX()+step][i]>=8)
          //check if there would be a path to it
          if (FindPath(target_unit->GetPosX(), target_unit->GetPosY(), target_unit->GetPosX()+step, i,
                      true, m_Map, path, target_unit->GetPosX()+step, i))
          {
            //found a path to that square
            found = true;
            target_coords_x = target_unit->GetPosX()+step;
            target_coords_y = i;
            break;
          }//if
    step = step+1;
  }//while

  // -- set data for goto task
  //found something?
  if (found)
  {
    m_X = target_coords_x;
    m_Y = target_coords_y;
    m_Path = path;
    m_BuildWhenDone = true;
  }//if
  else
  {
    //nothing found
    m_X = target_unit->GetPosX();
    m_Y = target_unit->GetPosY();
    m_Path.clear();
    m_BuildWhenDone = false;
  }//else
  spec_X = target_coords_x;
  spec_Y = target_coords_y;
}//construc

TFindLandForColonyTask::~TFindLandForColonyTask()
{
  m_Map = nullptr;
  //inherited Destroy;
}

bool TFindLandForColonyTask::Done() const
{
  if ((!target->IsShip()) or (target->EmbarkedPassengers()==0)) return true;
  else
  {
    return ((TGoToTask::Done()) or !m_BuildWhenDone);
    /*We are done, if the GoToTask is done or no colony will be build anyway. */
  }//else
}//func

bool TFindLandForColonyTask::Execute()
{
  if (target->EmbarkedPassengers()==0)
  {
    return false;
  }//if
  //execute the GoToTask
  bool Result = TGoToTask::Execute();
  //Are we at the destination?
  if ((target->GetPosX()==m_X) and (target->GetPosY()==m_Y))
  {
    std::shared_ptr<TUnit> founder = target->GetFirstEmbarkedPassenger();
    if (target->UnloadUnit(founder->GetType(), m_X, m_Y, m_Map))
    {
      //build new colony and set the founder into the upper, left field
      (static_cast<TData>(m_Data).NewColony(m_X, m_Y, founder->GetNation(),
        static_cast<TData>(m_Data).GetLang.GetColonyNames(founder->GetNation(),
        length(static_cast<TData>(m_Data).GetColonyList(founder->GetNation())))))->SetUnitInField(-1, -1, founder);
      //create road in colony square
      m_Map->tiles[m_X][m_Y]->CreateRoad();
      //try to unload other units, too
      LongInt tries = 0;
      while ((target->GetFirstEmbarkedPassenger()!=nullptr) and (tries<6))
      {
        target->UnloadUnit(target->GetFirstEmbarkedPassenger()->GetType(), m_X, m_Y, m_Map);
        tries = tries +1;
      }//while
      return true;
    }//if
  }//if at destination location
  return true;
}//func

TTaskType TFindLandForColonyTask::GetType() const
{
  return ttFindLand;
}//func


//general
TUnitType GetUnitForGood(const TGoodType AGood)
{
  switch (AGood)
  {
    case gtFood: return utFarmer;
    case gtSugar: return utSugarplanter;
    case gtTobacco: return utTobaccoplanter;
    case gtCotton: return utCottonplanter;
    case gtFur: return utFurHunter;
    case gtWood: return utWoodcutter;
    case gtOre: return utOreMiner;
    case gtSilver: return utSilverMiner;
    //case gtHorses: none
    case gtRum: return utDistiller;
    case gtCigar: return utTobacconist;
    case gtCloth: return utWeaver;
    case gtCoat: return utFurTrader;
    //case gtTradegoods: none
    case gtTool: return utSmith;
    case gtMusket: return utWeaponSmith;
    case gtHammer: return utCarpenter;
    case gtLibertyBell: return utStatesman;
    case gtCross: return utPreacher;
    default:
         return utCriminal;
  }//switch
}//func

bool HasExpertStatus(const TGoodType AGood, const TUnitType ut)
{
  return ((GetUnitForGood(AGood)==ut) and (ut!=utCriminal));
}//func
