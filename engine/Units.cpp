#include "Units.h"
#include <iostream>
#include <cstdlib> //abs
#include "PascalFunctions.h"

int Ord(const TUnitType ut)
{
  return static_cast<int> (ut);
}

TUnitType High(const TUnitType ut)
{
  return utBraveOnHorse;
}

TUnitType Low(const TUnitType ut)
{
  return utCriminal;
}

TUnitType Succ(const TUnitType ut)
{
  if (ut<utBraveOnHorse) return static_cast<TUnitType> (ut+1);
  throw 42;
}

int Ord(const TUnitLocation ul)
{
  return static_cast<int> (ul);
}

TUnitLocation High(const TUnitLocation ul)
{
  return ulEmbarked;
}

TUnitLocation Low(const TUnitLocation ul)
{
  return ulAmerica;
}

TUnitLocation Succ(const TUnitLocation ul)
{
  if (ul<ulEmbarked) return static_cast<TUnitLocation> (ul+1);
  throw 42;
}

int Ord(const TUnitState us)
{
  return static_cast<int> (us);
}

TUnitState High(const TUnitState us)
{
  return usCreateRoad;
}

TUnitState Low(const TUnitState us)
{
  return usNormal;
}

TUnitState Succ(const TUnitState us)
{
  if (us<usCreateRoad) return static_cast<TUnitState> (us+1);
  throw 42;
}

//workaround for built-in Pascal functions
int Ord(const TDirection dir)
{
  return static_cast<int> (dir);
}

//look-up "table" for prices
LongInt cShipPrices(const TUnitType ut)
{
  switch (ut)
  {
    case utCaravel: return 1000;
    case utTradingShip: return 2000;
    case utGalleon: return 3000;
    case utPrivateer: return 2000;
    case utFrigate: return 5000;
    default: throw 43;
  }//swi
}

//look-up "table" for unit prices in Europe
LongInt cUnitPrices(const TUnitType ut)
{
  switch(ut)
  {
    case utFarmer: return 1100; //utFarmer
    case utFisher: return 1000; //utFisher
    case utFurHunter: return -1; //utFurHunter
    case utSilverMiner: return 900; //utSilverMiner
    case utWoodcutter: return 700; //utWoodcutter
    case utOreMiner: return 600; //utOreMiner
    case utSugarplanter: return -1; //utSugarplanter
    case utCottonplanter: return -1; //utCottonplanter
    case utTobaccoplanter: return -1; //utTobaccoplanter
    case utPreacher: return 1500; //utPreacher
    case utStatesman: return 1900; //utStatesman
    case utCarpenter: return 1000; //utCarpenter
    case utDistiller: return 1100; //utDistiller
    case utWeaver: return 1300; //utWeaver
    case utTobacconist: return 1200; //utTobacconist
    case utFurTrader: return 950; //utFurTrader
    case utSmith: return 1050; //utSmith
    case utWeaponSmith: return 850; //utWeaponSmith
    case utScout: return  -1; //utScout
    case utPioneer: return 1200; //utPioneer
    case utMissionary: return 1400; //utMissionary
    case utRegular: return 2000; //utRegular
    default: throw 43;
  } //swi
}

//helper procedure
void ApplyDir(Byte* x, Byte* y, const  TDirection dir)
{
  switch (dir)
  {
    case dirW:
    case dirSW:
    case dirNW:
         if (*x>0)  *x = *x-1;
         break;
    case dirE:
    case dirNE:
    case dirSE:
         if (*x<cMap_X-1) *x = *x+1;
         break;
  }//switch
  switch (dir)
  {
    case dirNW:
    case dirN:
    case dirNE:
         if (*y>0) *y = *y-1;
         break;
    case dirSW:
    case dirS:
    case dirSE:
         if (*y<cMap_Y-1) *y = *y+1;
         break;
  }//switch
}//proc

TDirection GetApplyingDirection(const Byte from_x, const Byte from_y, const Byte to_x, const Byte to_y)
{
  if ((abs(from_x-to_x)>1) or (abs(from_y-to_y)>1)) return dirNone;

  switch (to_x-from_x)
  {
    case 1:
         switch(to_y-from_y)
         {
           case -1: return dirNE;
           case 0: return dirE;
           case 1: return dirSE;
         }//swi
         break;
    case 0:
         switch(to_y-from_y)
         {
           case -1: return dirN;
           case 0: return dirNone;
           case 1: return dirS;
         }//swi
         break;
    case -1:
         switch(to_y-from_y)
         {
           case -1: return dirNW;
           case 0: return dirW;
           case 1: return dirSW;
         }//swi
         break;
   }//switch
   throw 99; //logic error - you should never reach that line
}//func

// ***************
// *TUnit methods*
// ***************

TUnit::TUnit(const TUnitType TypeOfUnit, const LongInt ANation, const LongInt X, const LongInt Y)
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
  AI_Task = NULL;
  if (TypeOfUnit == utPioneer) GiveTools(100);
  else if ((TypeOfUnit==utRegular) or (TypeOfUnit==utDragoon)) GiveMuskets(true);
  if ((TypeOfUnit==utScout) or (TypeOfUnit==utDragoon) or (TypeOfUnit==utBraveOnHorse)) GiveHorses(true);
  LongInt i;
  for (i=0; i<6; ++i)
  {
    passengers[i] = NULL;
    cargo_load[i].amount = 0;
    cargo_load[i].which = gtFood;
  }//for
}//construc

TUnit::~TUnit()
{
  //inherited Destroy;
}//destruc

void TUnit::NewRound()
{
  //regain moves
  MovesLeft = MovesPerRound();
  //check for passage over the pond
  if (m_RoundsInOpenSea>0)
  {
    m_RoundsInOpenSea = m_RoundsInOpenSea-1;
    if (m_RoundsInOpenSea==0)
    {
      switch (m_location)
      {
        case ulGoToNewWorld:
             m_location = ulAmerica;
             break;
        case ulGoToEurope:
             m_location = ulEurope;
             DropAllPassengers();
             break;
        default:
             throw 99; //logic error - m_location should never be something
                       //else than ulGoToNewWorld or ulGoToEurope here.
      }//case
    }//if
  }//if

  //check for task and execute, if present
  if (AI_Task!=NULL)
  {
    std::cout <<"New Round: Exec calling\n";
    AI_Task->Execute();

    if (AI_Task->Done())
    {
      delete AI_Task;
      AI_Task = NULL;
    }
  }
}//proc

bool TUnit::Move(const TDirection direction, TMap& AMap)
{
  if (MovesLeft<=0) return false;

  LongInt newX, newY;
  switch(direction)
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
  }//swi
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
  }//swi
  bool allow = true;
  //check if we are out of map
  if ((newX<0) or (newY<0) or (newX>=cMap_X) or (newY>=cMap_Y))
  {
    allow = false;
  }
  else
  {
    //if AMap<>nil then
    if (AMap.tiles[newX][newY]!=NULL)
    {
      allow = (IsShip()==AMap.tiles[newX][newY]->IsWater());
    }//if
  }//else

  if (allow)
  {
    //check ships for european route
    if (IsShip() /*and (AMap<>nil)*/ and (AI_Task==NULL))//no european route for non-ships or AI tasks
      if ((AMap.tiles[PosX][PosY]->GetType()==ttOpenSea) and (AMap.tiles[newX][newY]->GetType()==ttOpenSea))
      {
        SendToEurope();
        return true;
      }//if
    if (direction!=dirNone)
    {
      MovesLeft = MovesLeft -1;
      PosX = newX;
      PosY = newY;
    }//if
    if (/*(AMap<>nil) and*/ (m_Nation!=0)) AMap.DiscoverSurroundingTiles(newX, newX, m_Nation, UnitType==utScout);
    return true;
  }//if
  return false;
}//func

bool TUnit::WarpToXY(const Byte x, const Byte y, TMap& AMap)
{
  if ((x>=cMap_X) or (y>=cMap_Y)) return false;
  PosX = x;
  PosY = y;
  if (/*(AMap<>nil) and*/ (m_Nation!=0))
    AMap.DiscoverSurroundingTiles(x,y, m_Nation, UnitType==utScout);
  return true;
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
  if ((!IsShip()) and (UnitType!=utConvoy))
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
  return ((UnitType==utCaravel) or (UnitType==utTradingShip)
           or (UnitType==utGalleon) or (UnitType==utPrivateer)
           or (UnitType==utFrigate) or (UnitType==utMan_o_War));
}//func

LongInt TUnit::MovesPerRound() const
{
  switch(UnitType)
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
  }//swi
}//func

LongInt TUnit::AttackStrength() const
{
  LongInt Result;
  switch(UnitType)
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
         Result= 1;
         break;
  }//swi
  if (m_State==usFortified) Result = (Result*3) / 2;
  return Result;
}//func

TTask* TUnit::GetTask() const
{
  return AI_Task;
}//func

void TUnit::SetTask(TTask* new_task, const bool ImmediateExecute)
{
  if (AI_Task!=NULL) delete AI_Task;
  AI_Task = new_task;
  if ((AI_Task!=NULL) and ImmediateExecute) AI_Task->Execute();
}//proc

bool TUnit::SendToEurope()
{
  if ((m_location!=ulAmerica) or (!IsShip())) return false;
  m_RoundsInOpenSea = 2;
  MovesLeft = 0;
  m_location = ulGoToEurope;
  return true;
}//func

bool TUnit::SendToNewWorld()
{
  if ((m_location!=ulEurope) or (!IsShip())) return false;
  m_RoundsInOpenSea = 2;
  MovesLeft = 0;
  m_location = ulGoToNewWorld;
  return true;
}//func

bool TUnit::CallBackToEurope()
{
  if ((m_location!=ulGoToNewWorld) or (!IsShip())) return false;
  m_RoundsInOpenSea = 2-m_RoundsInOpenSea;
  MovesLeft = 0;
  m_location = ulGoToEurope;
  if (m_RoundsInOpenSea<=0)
  {
    m_location = ulEurope;
    DropAllPassengers();
  }//if
  return true;
}//func

bool TUnit::CallBackToNewWorld()
{
  if ((m_location!=ulGoToEurope) or (!IsShip())) return false;
  m_RoundsInOpenSea = 2-m_RoundsInOpenSea;
  MovesLeft = 0;
  m_location = ulGoToNewWorld;
  if (m_RoundsInOpenSea<=0) m_location = ulAmerica;
  return true;
}//func

Byte TUnit::FreightCapacity() const
{
  switch(UnitType)
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
  }//swi
}//func

Byte TUnit::FreeCapacity() const
{
  if (FreightCapacity()==0) return 0;
  Byte occupied = 0;
  Byte i;
  for (i= 0; i<6; ++i)
  {
    if (passengers[i]!=NULL) occupied = occupied+1;
    if (cargo_load[i].amount>0) occupied = occupied+1;
  }//for
  if (FreightCapacity()<=occupied) return 0;
  return FreightCapacity() - occupied;
}//func

Byte TUnit::EmbarkedPassengers() const
{
  Byte Result = 0;
  Byte i;
  for (i= 0; i<6; ++i)
  {
    if (passengers[i]!=NULL) Result = Result+1;
  }
  return Result;
}//func

TUnit* TUnit::GetFirstEmbarkedPassenger() const
{
  LongInt i;
  for (i=5; i>=0; --i)
  {
    if (passengers[i]!=NULL) return passengers[i];
  }//for
  return NULL;
}//func

TUnit* TUnit::GetPassengerBySlot(const Byte slot) const
{
  if (slot>5) return NULL;
  return passengers[slot];
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
  Byte slot =0;
  while ((cargo_load[slot].amount!=0) and (slot<5))
    slot = slot+1;
  cargo_load[slot].amount = num;
  cargo_load[slot].which = AGood;
  return true;
}//func

//tries to unload 'num' units of good 'AGood' and returns number of unloaded units
Byte TUnit::UnloadGood(const TGoodType AGood, const Byte num)
{
  Byte Result =0;
  if (FreightCapacity()>0)
  {
    Byte cap;
    LongInt slot =5;//needs to be signed type, because it can get negative (-1)
    while ((slot>=0) and (Result<num))
    {
      if (cargo_load[slot].which==AGood)
      {
        if (cargo_load[slot].amount<num-Result) cap = cargo_load[slot].amount;
        else cap = num-Result;
        Result = Result+cap;
        cargo_load[slot].amount = cargo_load[slot].amount - cap;
      }//if
      slot = slot-1;
    }//while
  }//if
  return Result;
}//func

//tries to load unit 'AUnit' and returns True on success
bool TUnit::LoadUnit(TUnit* AUnit)
{
  if ((FreeCapacity()==0) or (AUnit==NULL) or (UnitType==utConvoy)) return false;
  if (AUnit->FreightCapacity()>0) return false; //no ships or convoys
  Byte slot = 0;
  while ((passengers[slot]!=NULL) and (slot<5))
    slot = slot+1;
  passengers[slot] = AUnit;
  AUnit->SetLocation(ulEmbarked);
  AUnit->SetState(usWaitingForShip);
  return true;
}//func

/*tries to unload a unit and place it at the given coordinates
 -Return value: true on success, false otherwise
 -TODO: unloads first unit of given type, so if there are two ore more units of
  ===== the same type loaded, then it migth unload the wrong one*/
bool TUnit::UnloadUnit(const TUnitType AType, const Byte x, const Byte y, TMap& AMap)
{
  if (FreightCapacity()<=0) return false;
  if ((sqr(x-GetPosX())>1) or (sqr(y-GetPosY())>1)) return false;
  LongInt i;
  for (i= 5; i>=0; --i)
  {
    if (passengers[i]!=NULL)
      if (passengers[i]->GetType()==AType)
        if (passengers[i]->WarpToXY(x,y,AMap))
        {
          passengers[i]->SetLocation(this->GetLocation());
          passengers[i]->SetState(usNormal);
          passengers[i] = NULL;
          return true;
        }//if
  }//for
  return false;
}//func

void TUnit::DropAllPassengers()
{
  Byte slot;
  for (slot=0; slot<6; ++slot)
  {
    if (passengers[slot]!=NULL)
    {
      if (m_location==ulEurope)
        passengers[slot]->SetLocation(ulEurope);
      else
      {
        passengers[slot]->SetLocation(ulAmerica);
        passengers[slot]->WarpToXY(PosX, PosY, TMap::GetSingleton());
      }//else
      passengers[slot]->SetState(usNormal);
      passengers[slot] = NULL;
    }//if
  }//for
}//proc

Byte TUnit::GetToolAmount() const
{
  return (items & UNIT_TOOL_MASK)*20;
}//func

void TUnit::GiveTools(const Byte amount)
{
  Byte temp = amount / 20;
  if (temp>5) temp=5;
  items = (temp | (items & (UNIT_HORSE_BIT | UNIT_MUSKET_BIT)));
}//proc

bool TUnit::HasHorses() const
{
  return ((items & UNIT_HORSE_BIT)>0);
}//func

void TUnit::GiveHorses(const bool has)
{
  if (has)
  {
    items = (items | UNIT_HORSE_BIT);
    if (UnitType==utRegular) UnitType = utDragoon;
  }//if
  else
  {
    items = (items & (!UNIT_HORSE_BIT));
    if (UnitType==utDragoon) UnitType = utRegular;
  }//else
}//proc

bool TUnit::HasMuskets() const
{
  return ((items & UNIT_MUSKET_BIT)>0);
}//func

void TUnit::GiveMuskets(const bool has)
{
  if (has) items = (items | UNIT_MUSKET_BIT);
  else items = (items & (!UNIT_MUSKET_BIT));
}//proc

void TUnit::ChangeAllItems(const Byte new_items)
{
  items = new_items;
}//proc

/* saving function, returns true on success.
  Loading function is part of TData (to keep data integrity).*/
bool TUnit::SaveToStream(std::ofstream& fs) const
{
  if (!fs.good())
    return false;
  fs.write((char*) &MovesLeft, sizeof(MovesLeft));
  if (!fs.good()) return false;
  fs.write((char*) &PosX, sizeof(PosX));
  if (!fs.good()) return false;
  fs.write((char*) &PosY, sizeof(PosY));
  if (!fs.good()) return false;
  fs.write((char*) &UnitType, sizeof(TUnitType));
  if (!fs.good()) return false;
  fs.write((char*) &m_location, sizeof(TUnitLocation));
  if (!fs.good()) return false;
  fs.write((char*) &m_State, sizeof(TUnitState));
  if (!fs.good()) return false;
  fs.write((char*) &m_RoundsInOpenSea, sizeof(Byte));
  if (!fs.good()) return false;
  fs.write((char*) &m_Nation, sizeof(LongInt));
  if (!fs.good()) return false;
  fs.write((char*) &items, sizeof(items));
  if (!fs.good()) return false;
  //save cargo
  LongInt i;
  for (i= 0; i<6; ++i)
  {
    fs.write((char*) &(cargo_load[i].amount), sizeof(Byte));
    fs.write((char*) &(cargo_load[i].which), sizeof(TGoodType));
    if (!fs.good()) return false;
  }//func
  //save passengers
  const Byte pass_count = EmbarkedPassengers();
  fs.write((char*) &pass_count, sizeof(Byte));
  if (!fs.good()) return false;
  for (i= 0; i<6; ++i)
    if (passengers[i]!=NULL)
    {
      if (!passengers[i]->SaveToStream(fs)) return false;
    }//if
  //********* save tasks needs to be done yet! *********
  return fs.good();
}//func

Byte TUnit::GetCargoAmountBySlot(const Byte slot) const
{
  if (slot<=5) return cargo_load[slot].amount;
  return 0;
}//func

TGoodType TUnit::GetCargoGoodBySlot(const Byte slot) const
{
  if (slot<=5) return cargo_load[slot].which;
  return gtCross;
}//func


//only used during loading routine
void TUnit::SetCargo(const Byte slot, const Byte new_amount, const TGoodType AGood)
{
  if ((FreightCapacity()==0) or (slot>5) or (new_amount>100)) return;
  cargo_load[slot].amount = new_amount;
  cargo_load[slot].which = AGood;
}//func

//**** AI-related functions ****

TTask::TTask(TUnit* target_unit)
{
  //inherited Create;
  target = target_unit;
}//construc

TTask::~TTask()
{
  if (target!=NULL) target->SetState(usNormal);
  //inherited Destroy;
}//destruc

TTaskType TTask::GetType() const
{
  return ttGeneric;
}//func

//**** TPloughTask methods ****

TPloughTask::TPloughTask(TUnit* target_unit, const Byte X, const Byte Y)
 : TTask(target_unit)
{
  //inherited Create(target_unit);
  m_X = X;
  m_Y = Y;
  RoundsLeft = 4;
  if (target!=NULL)
  {
    target_unit->SetState(usPloughing);
    if (target->GetType()== utPioneer) RoundsLeft = 2;
  }
  if (TMap::GetSingleton().tiles[m_X][m_Y]->IsPloughed()) RoundsLeft = 0;
}//construc

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
      TMap::GetSingleton().tiles[m_X][m_Y]->Plough();
    }
    return true;
  }//if
  return false;
}//func

TTaskType TPloughTask::GetType() const
{
  return ttPlough;
}//func

// **** TRoadTask methods ****
TRoadTask::TRoadTask(TUnit* target_unit, const Byte X, const Byte Y)
  : TTask(target_unit)
{
  //inherited Create(target_unit);
  m_X = X;
  m_Y = Y;
  RoundsLeft = 2;
  if (target!=NULL)
  {
    target->SetState(usCreateRoad);
    if (target->GetType()==utPioneer) RoundsLeft = 1;
  }
  if (TMap::GetSingleton().tiles[m_X][m_Y]->HasRoad()) RoundsLeft = 0;
}//create

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
      TMap::GetSingleton().tiles[m_X][m_Y]->CreateRoad();
    }
    return true;
  }//if
  return false;
}//func

TTaskType TRoadTask::GetType() const
{
  return ttRoad;
}//func

// **** TClearTask methods ****
TClearTask::TClearTask(TUnit* target_unit, const Byte X, const Byte Y)
  : TTask(target_unit)
{
  //inherited Create(target_unit);
  m_X = X;
  m_Y = Y;
  RoundsLeft = 6;
  if (target!=NULL)
  {
    target->SetState(usPloughing);
    if (target->GetType()==utPioneer) RoundsLeft = 3;
  }
  if (!(TMap::GetSingleton().tiles[m_X][m_Y]->HasForest())) RoundsLeft = 0;
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
      TMap::GetSingleton().tiles[m_X][m_Y]->ClearForest();
    }
    return true;
  }//if
  return false;
}//func

TTaskType TClearTask::GetType() const
{
  return ttClear;
}//func

// go to task (pathfinding)
TGoToTask::TGoToTask(TUnit* target_unit, const Byte ToX, const Byte ToY, const Byte SpecialX, const Byte SpecialY)
  : TTask(target_unit)
{
  //inherited Create(target_unit);
  m_X = ToX;
  m_Y = ToY;
  spec_X = SpecialX;
  spec_Y = SpecialY;
  m_Path.clear();
  if (FindPath(target_unit->GetPosX(), target_unit->GetPosY(), ToX, ToY, target_unit->IsShip(), TMap::GetSingleton(), m_Path, SpecialX, SpecialY))
  {
    target_unit->SetState(usGoTo);
  }
  else m_Path.clear();
}//construc

bool TGoToTask::Done() const
{
  return (((target->GetPosX()==m_X) and (target->GetPosY()==m_Y)) or (m_Path.empty()));
}//func

bool TGoToTask::Execute()
{
  std::cout<< "GoTo.Execute called. Path len: "<<m_Path.size()<<"\n";
  while ((target->MovesLeft>0) and (m_Path.size()>0))
  {
    const Byte x = m_Path[m_Path.size()-1].x;
    const Byte y = m_Path[m_Path.size()-1].y;
    const TDirection direc = GetApplyingDirection(target->GetPosX(), target->GetPosY(), x,y);

    //debug only
    std::cout<<"-GoTo.Execute:\n";
    std::cout<<"-- from: "<<target->GetPosX()<<","<<target->GetPosY()<<"  to: "<<x<<","<<y<<"\n";
    std::cout<<"-- apply dir.: "<<Ord(direc)<<"\n";
    //end debug

    target->Move(direc, TMap::GetSingleton());
    if ((target->GetPosX()!=x) or (target->GetPosY()!=y))
    {
      //check for special location
      if ((x==spec_X) and (y==spec_Y))
      {
        target->WarpToXY(spec_X, spec_Y, TMap::GetSingleton());
        target->MovesLeft = target->MovesLeft-1;
        if (target->MovesLeft<0) target->MovesLeft=0;
      }
      else
      {
        //something went wrong here, abort the whole task
        m_Path.clear();
        //debug only
        std::cout<<"-- direction application failed!\n";
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
  if (target!=NULL) target->SetState(usNormal);
  //inherited Destroy;
}//destruc

TTaskType TGoToTask::GetType() const
{
  return ttGoTo;
}//func

//general
TUnitType GetUnitForGood(const TGoodType AGood)
{
  switch(AGood)
  {
    case gtFood: return utFarmer;
    case gtSugar: return utSugarplanter;
    case gtTobacco: return utTobaccoplanter;
    case gtCotton: return utCottonplanter;
    case gtFur: return utFurHunter;
    case gtWood: return utWoodcutter;
    case gtOre: return utOreMiner;
    case gtSilver: return utSilverMiner;
    //gtHorses: none
    case gtRum: return utDistiller;
    case gtCigar: return utTobacconist;
    case gtCloth: return utWeaver;
    case gtCoat: return utFurTrader;
    //gtTradegoods: none
    case gtTool: return utSmith;
    case gtMusket: return utWeaponSmith;
    case gtHammer: return utCarpenter;
    case gtLibertyBell: return utStatesman;
    case gtCross: return utPreacher;
    default: return utCriminal;
  }//swi
}//func

bool HasExpertStatus(const TGoodType AGood, const TUnitType ut)
{
  return ((GetUnitForGood(AGood)==ut) and (ut!=utCriminal));
}//func
