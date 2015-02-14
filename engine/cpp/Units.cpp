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
             m_Location = ulAmerica;
             break;
        case ulGoToEurope:
             m_Location = ulEurope;
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
            TColony tempCol = TData(dat).GetColonyInXY(PosX, PosY);
            if (tempCol!=nullptr)
            {
              TUnitArr u_arr = TData(dat).GetAllUnitsInColony(tempCol);
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
    AI_Task.Destroy;
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

function TUnit.EmbarkedPassengers: Byte;
var i: Integer;
begin
  Result:= 0;
  for i:= 0 to 5 do
    if passengers[i]<>nil then Result:= Result+1;
end;//func

function TUnit.GetFirstEmbarkedPassenger: TUnit;
var i: Integer;
begin
  Result:= nil;
  for i:= 5 downto 0 do
  begin
    if passengers[i]<>nil then Result:= passengers[i];
  end;//for
end;//func

function TUnit.GetPassengerBySlot(const slot: Byte): TUnit;
begin
  if slot>5 then Result:= nil
  else Result:= passengers[slot];
end;//func

/*tries to load num units of good 'AGood'; maximum is 100
 TO-DO: function still uses a new slot for every good, even if there already is
 ****** an amount of the given good loaded (e.g. trying to load 20 food and
        then again less then 80 food will occupy two slots, even though a slot
        is able to store up to 100 units of a good.
*/
function TUnit.LoadGood(const AGood: TGoodType; const num: Byte): Boolean;
var slot: Byte;
begin
  if ((num>100) or (FreeCapacity=0)) then Result:= False
  else begin
    slot:=0;
    while ((cargo_load[slot].amount<>0) and (slot<5)) do
      slot:= slot+1;
    cargo_load[slot].amount:= num;
    cargo_load[slot].which:= AGood;
    Result:= True;
  end;//else
end;//func

//tries to unload 'num' units of good 'AGood' and returns number of unloaded units
function TUnit.UnloadGood(const AGood: TGoodType; const num: Byte): Byte;
var cap: Byte;
    slot: Integer;//needs to be signed type, because it can get negative (-1)
begin
  Result:=0;
  if FreightCapacity>0 then
  begin
    slot:=5;
    while (slot>=0) and (Result<num) do
    begin
      if cargo_load[slot].which=AGood then
      begin
        if cargo_load[slot].amount<num-Result then cap:= cargo_load[slot].amount
        else cap:= num-Result;
        Result:= Result+cap;
        cargo_load[slot].amount:= cargo_load[slot].amount - cap;
      end;//if
      slot:= slot-1;
    end;//while
  end;//if
end;//func

//tries to load unit 'AUnit' and returns True on success
function TUnit.LoadUnit(AUnit: TUnit): Boolean;
var slot: Byte;
begin
  if (FreeCapacity=0) or (AUnit=nil) or (UnitType=utConvoy) then Result:= False
  else if (AUnit.FreightCapacity>0) then Result:= False //no ships or convoys
  else begin
    slot:= 0;
    while (passengers[slot]<>nil) and (slot<5) do
      slot:= slot+1;
    passengers[slot]:= AUnit;
    AUnit.SetLocation(ulEmbarked);
    AUnit.SetState(usWaitingForShip);
    Result:= True;
  end;//else
end;//func

{tries to unload a unit and place it at the given coordinates
 -Return value: true on success, false otherwise
 -TODO: unloads first unit of given type, so if there are two ore more units of
  ===== the same type loaded, then it migth unload the wrong one}
function TUnit.UnloadUnit(const AType: TUnitType; const x,y: Byte; AMap: TMap): Boolean;
var i: Integer;
begin
  Result:= False;
  if FreightCapacity<=0 then exit;
  if ((sqr(x-GetPosX)>1) or (sqr(y-GetPosY)>1)) then Exit;
  for i:= 5 downto 0 do
  begin
    if passengers[i]<>nil then
      if passengers[i].GetType=AType then
        if passengers[i].WarpToXY(x,y,AMap) then
        begin
          passengers[i].SetLocation(self.GetLocation);
          passengers[i].SetState(usNormal);
          passengers[i]:= nil;
          Result:= True;
          break;
        end;//if
  end;//for
end;//func

procedure TUnit.DropAllPassengers;
var slot: Byte;
begin
  for slot:= 0 to 5 do
  begin
    if passengers[slot]<>nil then
    begin
      if m_Location = ulEurope then
        passengers[slot].SetLocation(ulEurope)
      else begin
        passengers[slot].SetLocation(ulAmerica);
        passengers[slot].WarpToXY(PosX, PosY, nil);
      end;//else
      passengers[slot].SetState(usNormal);
      passengers[slot]:= nil;
    end;//if
  end;//for
end;//proc

function TUnit.GetToolAmount: Byte;
begin
  Result:= (items and UNIT_TOOL_MASK)*20;
end;//func

procedure TUnit.GiveTools(const amount: Byte);
var temp: Byte;
begin
  temp:= amount div 20;
  if temp>5 then temp:=5;
  items:= temp or (items and (UNIT_HORSE_BIT or UNIT_MUSKET_BIT));
end;//proc

function TUnit.HasHorses: Boolean;
begin
  Result:= (items and UNIT_HORSE_BIT)>0;
end;//func

procedure TUnit.GiveHorses(const has: Boolean = True);
begin
  if has then
  begin
    items:= (items or UNIT_HORSE_BIT);
    if UnitType=utRegular then UnitType:= utDragoon;
    if UnitType=utBrave then UnitType:= utBraveOnHorse;
  end//if
  else begin
    items:= (items and (not UNIT_HORSE_BIT));
    if UnitType=utDragoon then UnitType:= utRegular;
    if UnitType=utBraveOnHorse then UnitType:= utBrave;
  end;//else
end;//proc

function Tunit.HasMuskets: Boolean;
begin
  Result:= (items and UNIT_MUSKET_BIT)>0;
end;//func

procedure TUnit.GiveMuskets(const has: Boolean = True);
begin
  if has then items:= (items or UNIT_MUSKET_BIT)
  else items:= (items and (not UNIT_MUSKET_BIT));
end;//proc

procedure TUnit.ChangeAllItems(const new_items: Byte);
begin
  items:= new_items;
end;//proc

{ saving function, returns true on success.
  Loading function is part of TData (to keep data integrity).}
function TUnit.SaveToStream(var fs: TFileStream): Boolean;
var i: Integer;
    pass_count: Byte;
begin
  if fs=nil then
  begin
    Result:= False;
    Exit;
  end;//if
  Result:= fs.Write(MovesLeft, sizeof(MovesLeft))=sizeof(MovesLeft);
  Result:= Result and (fs.Write(PosX, sizeof(PosX))=sizeof(PosX));
  Result:= Result and (fs.Write(PosY, sizeof(PosY))=sizeof(PosY));
  Result:= Result and (fs.Write(UnitType, sizeof(TUnitType))=sizeof(TUnitType));
  Result:= Result and (fs.Write(m_location, sizeof(TUnitLocation))=sizeof(TUnitLocation));
  Result:= Result and (fs.Write(m_State, sizeof(TUnitState))=sizeof(TUnitState));
  Result:= Result and (fs.Write(m_RoundsInOpenSea, sizeof(Byte))=sizeof(Byte));
  Result:= Result and (fs.Write(m_Nation, sizeof(LongInt))=sizeof(LongInt));
  Result:= Result and (fs.Write(items, sizeof(items))=sizeof(items));
  //save cargo
  for i:= 0 to 5 do
  begin
    Result:= Result and (fs.Write(cargo_load[i].amount, sizeof(Byte))=sizeof(Byte));
    Result:= Result and (fs.Write(cargo_load[i].which, sizeof(TGoodType))=sizeof(TGoodType));
  end;//func
  //save passengers
  pass_count:= EmbarkedPassengers;
  Result:= Result and (fs.Write(pass_count, sizeof(Byte))=sizeof(Byte));
  for i:= 0 to 5 do
    if passengers[i]<>nil then
      Result:= Result and passengers[i].SaveToStream(fs);
  //********* save tasks needs to be done yet! *********
end;//func

function TUnit.GetCargoAmountBySlot(const slot: Byte): Byte;
begin
  if slot<=5 then Result:= cargo_load[slot].amount
  else Result:= 0;
end;//func

function TUnit.GetCargoGoodBySlot(const slot: Byte): TGoodType;
begin
  if slot<=5 then Result:= cargo_load[slot].which
  else Result:= gtCross;
end;//func


//only used during loading routine
procedure TUnit.SetCargo(const slot: Byte; const new_amount: Byte; const AGood: TGoodType);
begin
  if ((FreightCapacity=0) or (slot>5) or (new_amount>100)) then Exit
  else begin
    cargo_load[slot].amount:= new_amount;
    cargo_load[slot].which:= AGood;
  end;//else
end;//func

//**** AI-related functions ****

constructor TTask.Create(const target_unit: TUnit);
begin
  inherited Create;
  target:= target_unit;
end;//construc

destructor TTask.Destroy;
begin
  if target<>nil then target.SetState(usNormal);
  inherited Destroy;
end;//destruc

function TTask.GetType: TTaskType;
begin
  Result:= ttGeneric;
end;//func

//**** TPloughTask methods ****

constructor TPloughTask.Create(const target_unit: TUnit; X, Y: Byte; const AMap: TMap);
begin
  inherited Create(target_unit);
  m_X:= X;
  m_Y:= Y;
  m_Map:= AMap;
  RoundsLeft:= 4;
  if target<>nil then
  begin
    target_unit.SetState(usPloughing);
    if target.GetType = utPioneer then RoundsLeft:= 2;
  end;
  if AMap.tiles[m_X, m_Y].IsPloughed then RoundsLeft:= 0;
end;//func

function TPloughTask.Done: Boolean;
begin
  Result:= (RoundsLeft<=0);
end;//func

function TPloughTask.Execute: Boolean;
begin
  if RoundsLeft>0 then
  begin
    RoundsLeft:= RoundsLeft-1;
    target.MovesLeft:= 0;
    if RoundsLeft=0 then
    begin
      target.GiveTools(target.GetToolAmount-20);
      m_Map.tiles[m_X, m_Y].Plough;
    end;
    Result:= True;
  end//if
  else Result:= False;
end;//func

function TPloughTask.GetType: TTaskType;
begin
  Result:= ttPlough;
end;//func

// **** TRoadTask methods ****
constructor TRoadTask.Create(const target_unit: TUnit; X, Y: Byte; const AMap: TMap);
begin
  inherited Create(target_unit);
  m_X:= X;
  m_Y:= Y;
  m_Map:= AMap;
  RoundsLeft:= 2;
  if target<>nil then
  begin
    target.SetState(usCreateRoad);
    if target.GetType = utPioneer then RoundsLeft:= 1;
  end;
  if AMap.tiles[m_X, m_Y].HasRoad then RoundsLeft:= 0;
end;//func

function TRoadTask.Done: Boolean;
begin
  Result:= (RoundsLeft<=0);
end;//func

function TRoadTask.Execute: Boolean;
begin
  if RoundsLeft>0 then
  begin
    RoundsLeft:= RoundsLeft-1;
    target.MovesLeft:= 0;
    if RoundsLeft=0 then
    begin
      target.GiveTools(target.GetToolAmount-20);
      m_Map.tiles[m_X, m_Y].CreateRoad;
    end;
    Result:= True;
  end//if
  else Result:= False;
end;//func

function TRoadTask.GetType: TTaskType;
begin
  Result:= ttRoad;
end;//func

// **** TClearTask methods ****
constructor TClearTask.Create(const target_unit: TUnit; X, Y: Byte; const AMap: TMap);
begin
  inherited Create(target_unit);
  m_X:= X;
  m_Y:= Y;
  m_Map:= AMap;
  RoundsLeft:= 6;
  if target<>nil then
  begin
    target.SetState(usPloughing);
    if target.GetType = utPioneer then RoundsLeft:= 3;
  end;
  if not (AMap.tiles[m_X, m_Y].HasForest) then RoundsLeft:= 0;
end;//func

function TClearTask.Done: Boolean;
begin
  Result:= (RoundsLeft<=0);
end;//func

function TClearTask.Execute: Boolean;
begin
  if RoundsLeft>0 then
  begin
    RoundsLeft:= RoundsLeft-1;
    target.MovesLeft:= 0;
    if RoundsLeft=0 then
    begin
      target.GiveTools(target.GetToolAmount-20);
      m_Map.tiles[m_X, m_Y].ClearForest;
    end;
    Result:= True;
  end//if
  else Result:= False;
end;//func

function TClearTask.GetType: TTaskType;
begin
  Result:= ttClear;
end;//func

// go to task (pathfinding)
constructor TGoToTask.Create(const target_unit: TUnit; ToX, ToY: Byte; const AMap: TMap; const SpecialX: Byte=250; const SpecialY: Byte=250);
begin
  inherited Create(target_unit);
  m_X:= ToX;
  m_Y:= ToY;
  spec_X:= SpecialX;
  spec_Y:= SpecialY;
  m_Map:= AMap;
  SetLength(m_Path, 0);
  if FindPath(target_unit.GetPosX, target_unit.GetPosY, ToX, ToY, target_unit.IsShip, AMap, m_Path, SpecialX, SpecialY) then
  begin
    target_unit.SetState(usGoTo);
  end
  else SetLength(m_Path, 0);
end;//construc

function TGoToTask.Done: Boolean;
begin
  Result:= ((target.GetPosX=m_X) and (target.GetPosY=m_Y)) or (length(m_Path)<1);
end;//func

function TGoToTask.Execute: Boolean;
var direc: TDirection;
    x,y: Byte;
begin
  {$IFDEF DEBUG_CODE}
  WriteLn('GoTo.Execute called. Path len: ', length(m_Path));
  {$ENDIF}
  Result:= True;
  while (target.MovesLeft>0) and (length(m_Path)>0) do
  begin
    x:= m_Path[High(m_Path)].x;
    y:= m_Path[High(m_Path)].y;
    direc:= GetApplyingDirection(target.GetPosX, target.GetPosY, x,y);

    //debug only
    {$IFDEF DEBUG_CODE}
    WriteDebugLn('-GoTo.Execute:');
    WriteLn('-- from: ',target.GetPosX,',',target.GetPosY,'  to: ',x,',',y);
    WriteLn('-- apply dir.: ', Ord(direc));
    {$ENDIF}
    //end debug

    target.Move(direc, m_Map, nil);
    if (target.GetPosX<>x) or (target.GetPosY<>y) then
    begin
      //check for special location
      if ((x=spec_X) and (y=spec_Y)) then
      begin
        target.WarpToXY(spec_X, spec_Y, m_Map);
        target.MovesLeft:= target.MovesLeft-1;
        if target.MovesLeft<0 then target.MovesLeft:=0;
      end
      else begin
        //something went wrong here, abort the whole task
        SetLength(m_Path, 0);
        Result:= False;
        //debug only
        WriteLn('-- direction application failed!');
        //end debug
        Exit;
      end;//else
    end//if
    else SetLength(m_Path, length(m_Path)-1); //remove last waypoint
  end;//while
end;//func

function TGoToTask.DestinationX: Byte;
begin
  Result:= m_X;
end;//func

function TGoToTask.DestinationY: Byte;
begin
  Result:= m_Y;
end;//func

destructor TGoToTask.Destroy;
begin
  SetLength(m_Path, 0);
  target.SetState(usNormal);
  inherited Destroy;
end;//destruc

function TGoToTask.GetType: TTaskType;
begin
  Result:= ttGoTo;
end;//func

// **** TGoToEuropeTask methods ****

constructor TGoToEuropeTask.Create(const target_unit: TUnit; ToX, ToY: Byte; const AMap: TMap);
begin
  inherited Create(target_unit, ToX, ToY, AMap);
end;//construc

destructor TGoToEuropeTask.Destroy;
begin
  inherited Destroy;
end;//destruc

function TGoToEuropeTask.Done: Boolean;
begin
  Result:= (inherited Done) and (target.GetLocation=ulEurope);
end;//func

function TGoToEuropeTask.Execute: Boolean;
begin
  if (not inherited Done) then
  begin
    inherited Execute;
    Result:= true;
  end
  else begin
    if target.GetLocation=ulAmerica then
    begin
      if (m_Map.tiles[target.GetPosX, target.GetPosY].GetType=ttOpenSea) then
        target.SendToEurope;
      Result:= true;
    end//if
    else Result:= target.GetLocation=ulGoToEurope;
  end;//else
end;//func

function TGoToEuropeTask.GetType: TTaskType;
begin
  Result:= ttGoToEurope;
end;//func

// **** TFindLandForColonyTask methods ****

constructor TFindLandForColonyTask.Create(const target_unit: TUnit; const AMap: TMap; const dat: Pointer);
var grid: TColonySiteEvaluationGrid;
    i, step: Integer;
    found: Boolean;
    target_coords_x, target_coords_y: Integer;
    path: TCoordArr;
begin
  target:= target_unit;
  m_Map:= AMap;
  m_Data:= dat;
  //get grid
  grid:= TData(m_Data).GetColonySiteEvaluation(true);
  //find best tile
  found:= false;
  step:= 1;
  while not found and (step<cMap_Y) do begin
    //check top row
    for i:= target_unit.GetPosX-step to target_unit.GetPosX+step do
      if m_Map.IsValidMapPosition(i, target_unit.GetPosY-step) then
        //check, if it has a high enough value
        if grid[i, target_unit.GetPosY-step]>=8 then
          //check if there would be a path to it
          if FindPath(target_unit.GetPosX, target_unit.GetPosY, i, target_unit.GetPosY-step,
                      true, m_Map, path, i, target_unit.GetPosY-step) then
          begin
            //found a path to that square
            found:= true;
            target_coords_x:= i;
            target_coords_y:= target_unit.GetPosY-step;
            break;
          end;//if
    if found then break;
    //check bottom row
    for i:= target_unit.GetPosX-step to target_unit.GetPosX+step do
      if m_Map.IsValidMapPosition(i, target_unit.GetPosY+step) then
        //check, if it has a high enough value
        if grid[i, target_unit.GetPosY+step]>=8 then
          //check if there would be a path to it
          if FindPath(target_unit.GetPosX, target_unit.GetPosY, i, target_unit.GetPosY+step,
                      true, m_Map, path, i, target_unit.GetPosY+step) then
          begin
            //found a path to that square
            found:= true;
            target_coords_x:= i;
            target_coords_y:= target_unit.GetPosY+step;
            break;
          end;//if
    if found then break;
    //check left row
    for i:= target_unit.GetPosY-step+1 to target_unit.GetPosY+step-1 do
      if m_Map.IsValidMapPosition(target_unit.GetPosX-step, i) then
        //check, if it has a high enough value
        if grid[target_unit.GetPosX-step, i]>=8 then
          //check if there would be a path to it
          if FindPath(target_unit.GetPosX, target_unit.GetPosY, target_unit.GetPosX-step, i,
                      true, m_Map, path, target_unit.GetPosX-step, i) then
          begin
            //found a path to that square
            found:= true;
            target_coords_x:= target_unit.GetPosX-step;
            target_coords_y:= i;
            break;
          end;//if
    if found then break;
    //check right row
    for i:= target_unit.GetPosY-step+1 to target_unit.GetPosY+step-1 do
      if m_Map.IsValidMapPosition(target_unit.GetPosX+step, i) then
        //check, if it has a high enough value
        if grid[target_unit.GetPosX+step, i]>=8 then
          //check if there would be a path to it
          if FindPath(target_unit.GetPosX, target_unit.GetPosY, target_unit.GetPosX+step, i,
                      true, m_Map, path, target_unit.GetPosX+step, i) then
          begin
            //found a path to that square
            found:= true;
            target_coords_x:= target_unit.GetPosX+step;
            target_coords_y:= i;
            break;
          end;//if
    step:= step+1;
  end;//while

  // -- set data for goto task
  //found something?
  if found then
  begin
    m_X:= target_coords_x;
    m_Y:= target_coords_y;
    m_Path:= path;
    m_BuildWhenDone:= true;
  end//if
  else begin
    //nothing found
    m_X:= target_unit.GetPosX;
    m_Y:= target_unit.GetPosY;
    SetLength(m_Path, 0);
    m_BuildWhenDone:= false;
  end;//else
  spec_X:= target_coords_x;
  spec_Y:= target_coords_y;
end;//construc

destructor TFindLandForColonyTask.Destroy;
begin
  m_Map:= nil;
  inherited Destroy;
end;

function TFindLandForColonyTask.Done: Boolean;
begin
  if ((not target.IsShip) or (target.EmbarkedPassengers=0)) then Result:= true
  else begin
      Result:= (inherited Done) or not m_BuildWhenDone; //We are done, if the
      //GoToTask is done or no colony will be build anyway.
  end;//else
end;//func

function TFindLandForColonyTask.Execute: Boolean;
var tries: LongInt;
    founder: TUnit;
begin
  if (target.EmbarkedPassengers=0) then
  begin
    Result:= false;
    exit;
  end;//if
  //execute the GoToTask
  Result:= inherited Execute;
  //Are we at the destination?
  if (target.GetPosX=m_X) and (target.GetPosY=m_Y) then
  begin
    founder:= target.GetFirstEmbarkedPassenger;
    if (target.UnloadUnit(founder.GetType, m_X, m_Y, m_Map)) then
    begin
      //build new colony and set the founder into the upper, left field
      (TData(m_Data).NewColony(m_X, m_Y, founder.GetNation,
        TData(m_Data).GetLang.GetColonyNames(founder.GetNation,
        length(TData(m_Data).GetColonyList(founder.GetNation))))).SetUnitInField(-1, -1, founder);
      //create road in colony square
      m_Map.tiles[m_X,m_Y].CreateRoad;
      //try to unload other units, too
      tries:= 0;
      while (target.GetFirstEmbarkedPassenger<>nil) and (tries<6) do
      begin
        target.UnloadUnit(target.GetFirstEmbarkedPassenger.GetType, m_X, m_Y, m_Map);
        tries:= tries +1;
      end;//while
      Result:= true;
      exit;
    end;//if
  end;//if at destination location
  Result:= true;
end;//func

function TFindLandForColonyTask.GetType: TTaskType;
begin
  Result:= ttFindLand;
end;//func


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

end.
