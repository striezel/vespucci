#include "Colony.h"
#include "Helper.h"

//workaround for built-in Pascal functions
int Ord(const TBuildingType bt)
{
  return static_cast<int> (bt);
}

TBuildingType High(const TBuildingType bt)
{
  return btBlacksmith;
}

TBuildingType Low(const TBuildingType bt)
{
  return btNone;
}

TBuildingType Succ(const TBuildingType bt)
{
  if (bt<btBlacksmith) return static_cast<TBuildingType>(bt+1);
  throw 42;
}


Byte GetMaxBuildingLevel(const TBuildingType bt)
{
  switch (bt)
  {
    case btNone: return 0;
    case btTownHall:
    case btStable:
    case btCustomHouse:
         return 1;
    case btWarehouse:
    case btPress:
    case btCarpenter:
    case btChurch:
         return 2;
    default:
         return 3;
  }//swi
}//func

TGoodType GetProducedGood(const TBuildingType bt)
{
  switch (bt)
  {
    case btArmory: return gtMusket;
    case btTownHall: return gtLibertyBell;
    case btWeaver: return gtCloth;
    case btTobacconist: return gtCigar;
    case btDistiller: return gtRum;
    case btFurTrader: return gtCoat;
    case btCarpenter: return gtHammer;
    case btChurch: return gtCross;
    case btBlacksmith: return gtTool;
    default: return gtFood; //gtFood here means: nothing. Food cannot be produced in buildings.
  }//swi
}//func

void GetBuildingCost(const TBuildingType bt, const Byte level, Word* Hammers, Word* Tools)
{
  *Hammers = 0;
  *Tools = 0;
  if ((bt!=btNone) and (level>0) and (level<=GetMaxBuildingLevel(bt)))
  {
    switch (bt)
    {
      case btFort:
           switch (level)
           { 
             case 1: *Hammers = 64; break;
             case 2: *Hammers = 120; *Tools = 100; break;
             case 3: *Hammers = 320; *Tools = 200; break;
           }//swi
           break;
      case btDock:
           switch (level)
           {
             case 1: *Hammers = 52; break;
             case 2: *Hammers = 80; *Tools = 50; break;
             case 3: *Hammers = 240; *Tools = 100; break;
           }//swi
           break;
      case btWarehouse:
           switch (level)
           {
             case 1: *Hammers = 80; break;
             case 2: *Hammers = 80; *Tools = 20; break;
           }//swi
           break;
      case btStable: if (level==1) *Hammers = 64;
           break;
      case btCustomHouse:
           if (level==1)
           {
             *Hammers = 160;
             *Tools = 50;
           }
           break;
      case btPress: 
           switch (level)
           {
             case 1: *Hammers = 52; *Tools = 20; break;
             case 2: *Hammers = 120; *Tools = 50; break;
           }//swi
           break;
      case btSchool:
           switch (level)
           {
             case 1: *Hammers = 64; break;
             case 2: *Hammers = 160; *Tools = 50; break;
             case 3: *Hammers = 200; *Tools = 100; break;
           }//swi
           break;
      case btArmory:
           switch (level)
           {
             case 1: *Hammers = 52; break;
             case 2: *Hammers = 120; *Tools = 50; break;
             case 3: *Hammers = 240; *Tools = 100; break;
           }//case
           break;
      case btWeaver:
      case btTobacconist:
      case btDistiller:
           switch (level)
           {
             case 2: *Hammers = 64; *Tools = 20; break;
             case 3: *Hammers = 160; *Tools = 100; break;
           }//case
           break;
      case btFurTrader:
           switch (level)
           {
             case 2: *Hammers = 56; *Tools= 20; break;
             case 3: *Hammers = 160; *Tools= 100; break;
           }//swi
           break;
      case btCarpenter:
           if (level==2) *Hammers = 52;
           break;
      case btChurch:
           switch (level)
           {
             case 1: *Hammers = 64; break;
             case 2: *Hammers = 176; *Tools = 100; break;
           }//swi
           break;
      case btBlacksmith:
           switch (level)
           {
             case 2: *Hammers = 64; *Tools = 20; break;
             case 3: *Hammers = 240; *Tools = 100; break;
           }//case
           break;
    }//swi
  }//if
}//proc

// **** TColony functions ****

TColony::TColony(const LongInt X, const LongInt Y, const LongInt ANation, const std::string& AName)
  : TSettlement (X, Y, ANation)
{
  //sets position and nation
  //inherited Create(X, Y, ANation);
  //set name
  m_Name = AName;
  //set all goods to zero
  TGoodType gt = gtFood;
  while (gt<High(gtFood))
  {
    Store[gt] = 0;
    gt = Succ(gt);
  }//while
  Store[High(gtFood)] = 0;
  //clear all buildings
  TBuildingType bt = Low(btNone);
  while (bt<High(btNone))
  {
    Buildings[bt] = 0;
    bt = Succ(bt);
    //clear units in buildings
    if ((bt>=btArmory) and (bt<=btBlacksmith))
    {
      UnitsInBuilding[bt][0] = NULL;
      UnitsInBuilding[bt][1] = NULL;
      UnitsInBuilding[bt][2] = NULL;
    }//if
  }//while
  Buildings[High(btNone)] = 0;
  //set initial buildins
  Buildings[btTownHall] = 1;
  Buildings[btCarpenter] = 1;
  Buildings[btBlacksmith] = 1;
  Buildings[btTobacconist] = 1;
  Buildings[btWeaver] = 1;
  Buildings[btDistiller] = 1;
  Buildings[btFurTrader] = 1;

  //value "btNone" indicates no construction in progress
  CurrentConstruction = btNone;
  //units in fields
  LongInt i,j;
  for (i=0;i<3;++i)
    for (j=0;j<3;++j)
    {
      UnitsInFields[i][j].u = NULL;
      UnitsInFields[i][j].GoesFor = gtFood;
    }//for
}//create

TColony::~TColony()
{
  //what shall we do?
  //set Units in fields free
  LongInt i, j;
  for (i=0; i<3; ++i)
    for (j=0; j<3; ++j)
      if (UnitsInFields[i][j].u!=NULL)
      {
        UnitsInFields[i][j].u->WarpToXY(GetPosX(), GetPosY(), TMap::GetSingleton());
        UnitsInFields[i][j].u->SetLocation(ulAmerica);
        UnitsInFields[i][j].u = NULL;
      }//if
  //set units in buildings free
  for (i= Ord(btArmory); i<=Ord(btBlacksmith); ++i)
    for (j= 0; j<=2; ++j)
    {
      if (UnitsInBuilding[TBuildingType(i)][j]!=NULL)
      {
        UnitsInBuilding[TBuildingType(i)][j]->WarpToXY(GetPosX(), GetPosY(), TMap::GetSingleton());
        UnitsInBuilding[TBuildingType(i)][j]->SetLocation(ulAmerica);
        UnitsInBuilding[TBuildingType(i)][j] = NULL;
      }//func
    }
  //inherited Destroy;
}//destruc

std::string TColony::GetName() const
{
  return m_Name;
}//func

void TColony::SetName(const std::string& new_name)
{
  if (Trim(new_name)!="") m_Name = Trim(new_name);
}//func

Word TColony::GetStore(const TGoodType AGood) const
{
  return Store[AGood];
}//func

Word TColony::RemoveFromStore(const TGoodType AGood, const Word amount)
{
  if (Store[AGood]>=amount)
  {
    Store[AGood] = Store[AGood]-amount;
    return amount;
  }
  const Word Result = Store[AGood];
  Store[AGood] = 0;
  return Result;
}//func

void TColony::AddToStore(const TGoodType AGood, const Word amount)
{
  //no function, it always succeeds. However, storage amount is cut to maximum
  // storage capacity during next call to TColony.NewRound.
  Store[AGood] = Store[AGood]+amount;
}//func

void TColony::SetStore(const TGoodType AGood, const Word new_amount)
{
  Store[AGood] = new_amount;
}//proc

Byte TColony::GetBuildingLevel(const TBuildingType bt) const
{
  return Buildings[bt];
}//func

void TColony::SetBuildingLevel(const TBuildingType bt, const Byte new_level)
{
  if (new_level>GetMaxBuildingLevel(bt)) Buildings[bt] = GetMaxBuildingLevel(bt);
  else Buildings[bt] = new_level;
}//proc

TBuildingType TColony::GetCurrentConstruction() const
{
  return CurrentConstruction;
}//func

void TColony::SetCurrentConstruction(const TBuildingType bt)
{
  CurrentConstruction = bt;
}//proc

void TColony::ConstructNextLevel()
{
  if (Buildings[CurrentConstruction]<GetMaxBuildingLevel(CurrentConstruction))
    Buildings[CurrentConstruction] = Buildings[CurrentConstruction]+1;
}//proc

LongInt TColony::GetProduction(const TBuildingType bt, const TUnitType ut) const
{
  LongInt Result;
  switch (ut)
  {
    case utCriminal: Result = 1; break;
    case utServant: Result = 2; break;
    default: Result = 3; break;
  }//swi

  //consider level of building
  if ((bt>=btArmory)  and (bt<=btBlacksmith))
    Result = Result * Buildings[bt];
  else return 0;

  //check for specialist's bonus
  switch (bt)
  {
    case btArmory: if (ut==utWeaponSmith) Result = Result*2; break;
    case btTownHall:
         if (ut==utStatesman) Result = Result*2;
         if (Buildings[btPress]==2) Result = (Result*9) / 4;
         else if (Buildings[btPress]==1) Result = (Result*3) / 2;
    case btWeaver: if (ut==utWeaver) Result = Result*2; break;
    case btTobacconist: if (ut==utTobacconist) Result = Result*2; break;
    case btDistiller: if (ut==utDistiller) Result = Result*2; break;
    case btFurTrader: if (ut==utFurTrader) Result = Result*2; break;
    case btCarpenter: if (ut==utCarpenter) Result = Result*2; break;
    case btChurch: if (ut==utPreacher) Result = Result*2; break;
    case btBlacksmith: if (ut==utSmith) Result = Result*2; break;
    default: throw 97; //should never happen here
  }//swi
  return Result;
}//func

//only calculates the good changes due to production in buildings;
// and production in fields (i.e. by farmers)
void TColony::NewRound(const TMap& AMap)
{
    //calculate production of units in surrounding fields
  LongInt i,j;
  for (i=-1; i<=1; ++i)
    for (j=-1; j<=1; ++j)
      if (UnitsInFields[i+1][j+1].u!=NULL)
        if ((this->PosX+i>=0) and (this->PosX+i<cMap_X) and (this->PosY+j>=0) and (this->PosY+j<cMap_Y))
          Store[UnitsInFields[i+1][j+1].GoesFor] = Store[UnitsInFields[i+1][j+1].GoesFor] +
           AMap.tiles[this->PosX+i][this->PosY+j]->GetGoodProduction(UnitsInFields[i+1][j+1].GoesFor,
                    HasExpertStatus(UnitsInFields[i+1][j+1].GoesFor, UnitsInFields[i+1][j+1].u->GetType()));
  //calculate production in base (=central) field
  Store[gtFood] = Store[gtFood]+AMap.tiles[PosX][PosY]->GetColonyFood();
  Store[AMap.tiles[PosX][PosY]->GetColonyGoodType()] =
        Store[AMap.tiles[PosX][PosY]->GetColonyGoodType()] +AMap.tiles[PosX][PosY]->GetColonyGoodAmount();

  //calculate production of all buildings
  //church first
  LongInt prod = 0;
  for (i=0; i<=2; ++i)
    if (UnitsInBuilding[btChurch][i]!=NULL)
      prod =prod+GetProduction(btChurch, UnitsInBuilding[btChurch][i]->GetType());
  Store[gtCross] = Store[gtCross]+prod;
  //town hall second
  prod = 0;
  for (i=0; i<3; ++i)
    if (UnitsInBuilding[btTownHall][i]!=NULL)
      prod =prod+GetProduction(btTownHall, UnitsInBuilding[btTownHall][i]->GetType());
  Store[gtLibertyBell] = Store[gtLibertyBell]+prod;
  //carpenter third
  prod = 0;
  for (i=0; i<3; ++i)
    if (UnitsInBuilding[btCarpenter][i]!=NULL)
      prod =prod+GetProduction(btCarpenter, UnitsInBuilding[btCarpenter][i]->GetType());
  if (Store[gtWood]<prod) prod = Store[gtWood];
  Store[gtWood] = Store[gtWood]-prod;
  Store[gtHammer] = Store[gtHammer]+prod;
  //Blacksmith fourth
  prod = 0;
  for (i=0; i<3; ++i)
    if (UnitsInBuilding[btBlacksmith][i]!=NULL)
      prod =prod+GetProduction(btBlacksmith, UnitsInBuilding[btBlacksmith][i]->GetType());
  if (Store[gtOre]<prod) prod = Store[gtOre];
  Store[gtOre] = Store[gtOre]-prod;
  Store[gtTool] = Store[gtTool]+prod;
  //armory fifth
  prod = 0;
  for (i=0; i<3; ++i)
    if (UnitsInBuilding[btArmory][i]!=NULL)
      prod =prod+GetProduction(btArmory, UnitsInBuilding[btArmory][i]->GetType());
  if (Store[gtTool]<prod) prod = Store[gtTool];
  Store[gtTool] = Store[gtTool]-prod;
  Store[gtMusket] = Store[gtMusket]+prod;
  //fur traders sixth
  prod = 0;
  for (i=0; i<3; ++i)
    if (UnitsInBuilding[btFurTrader][i]!=NULL)
      prod =prod+GetProduction(btFurTrader, UnitsInBuilding[btFurTrader][i]->GetType());
  if (Store[gtFur]<prod) prod = Store[gtFur];
  Store[gtFur] = Store[gtFur]-prod;
  Store[gtCoat] = Store[gtCoat]+prod;
  //weaver seventh
  prod = 0;
  for (i=0; i<3; ++i)
    if (UnitsInBuilding[btWeaver][i]!=NULL)
      prod =prod+GetProduction(btWeaver, UnitsInBuilding[btWeaver][i]->GetType());
  if (Store[gtCotton]<prod) prod = Store[gtCotton];
  Store[gtCotton] = Store[gtCotton]-prod;
  Store[gtCloth] = Store[gtCloth]+prod;
  //tobacconist eight
  prod = 0;
  for (i=0; i<3; ++i)
    if (UnitsInBuilding[btTobacconist][i]!=NULL)
      prod =prod+GetProduction(btTobacconist, UnitsInBuilding[btTobacconist][i]->GetType());
  if (Store[gtTobacco]<prod) prod = Store[gtTobacco];
  Store[gtTobacco] = Store[gtTobacco]-prod;
  Store[gtCigar] = Store[gtCigar]+prod;
  //distiller ninth
  prod = 0;
  for (i=0; i<3; ++i)
    if (UnitsInBuilding[btDistiller][i]!=NULL)
      prod =prod+GetProduction(btDistiller, UnitsInBuilding[btDistiller][i]->GetType());
  if (Store[gtSugar]<prod) prod = Store[gtSugar];
  Store[gtSugar] = Store[gtSugar]-prod;
  Store[gtRum] = Store[gtRum]+prod;

  //cut the amount we can't store off
  for (i=Ord(Low(gtFood)); i<=Ord(High(gtFood)); ++i)
    if ((TGoodType(i)!=gtFood) and (TGoodType(i)!=gtHammer) and (TGoodType(i)!=gtLibertyBell) and (TGoodType(i)!=gtCross)
         and (Store[TGoodType(i)]>(1+Buildings[btWarehouse])*100))
      Store[TGoodType(i)] = (1+Buildings[btWarehouse])*100;

  //check for inhabitants and food needed
  i = GetInhabitants()*2;
  if (RemoveFromStore(gtFood, i)!=i)
  {
    //not enough food - we should put a message and probably let an inhabitant
    // starve from hunger here.
  }

  //check for buildings
  if (CurrentConstruction!=btNone)
  {
    Word h, t;
    GetBuildingCost(CurrentConstruction, Buildings[CurrentConstruction]+1, &h, &t);
    if ((Store[gtHammer]>=h) and (Store[gtTool]>=t) and (h+t>0))
    {
      //enough material for new building
      ConstructNextLevel();
      CurrentConstruction = btNone;
      RemoveFromStore(gtHammer, h);
      RemoveFromStore(gtTool, t);
    }//if
  }//if

}//func

TUnit* TColony::GetUnitInField(const LongInt x_shift, const LongInt y_shift) const
{
  if ((abs(x_shift)>1) or (abs(y_shift)>1)) return NULL;
  return UnitsInFields[x_shift+1][y_shift+1].u;
}//func

TGoodType TColony::GetUnitInFieldGood(const LongInt x_shift, const LongInt y_shift) const
{
  if ((abs(x_shift)>1) or (abs(y_shift)>1)) return gtFood;
  return UnitsInFields[x_shift+1][y_shift+1].GoesFor;
}//func

void TColony::SetUnitInField(const LongInt x_shift, const LongInt y_shift, TUnit* AUnit, const TGoodType AGood)
{
  if ((abs(x_shift)>1) or (abs(y_shift)>1)) return;
  //remove old unit
  if (UnitsInFields[x_shift+1][y_shift+1].u!=NULL)
  {
    UnitsInFields[x_shift+1][y_shift+1].u->WarpToXY(GetPosX(), GetPosY(), TMap::GetSingleton());
    UnitsInFields[x_shift+1][y_shift+1].u->SetLocation(ulAmerica);
    UnitsInFields[x_shift+1][y_shift+1].u->SetState(usNormal);
  }//if
  //place new unit
  UnitsInFields[x_shift+1][y_shift+1].u = AUnit;
  UnitsInFields[x_shift+1][y_shift+1].GoesFor = AGood;
  if (AUnit!=NULL)
  {
    UnitsInFields[x_shift+1][y_shift+1].u->SetLocation(ulInColony);
    UnitsInFields[x_shift+1][y_shift+1].u->SetState(usNormal);
  }//if
}//proc

TUnit* TColony::GetUnitInBuilding(const TBuildingType bt, const Byte place) const
{
  if ((place>2) or (!((bt>=btArmory) and (bt<=btBlacksmith)))) return NULL;
  return UnitsInBuilding[bt][place];
}//func

void TColony::SetUnitInBuilding(const TBuildingType bt, const Byte place, TUnit* AUnit)
{
  if ((place>2) or (!((bt>=btArmory) and (bt<=btBlacksmith)))) return;
  //remove old unit
  if (UnitsInBuilding[bt][place]!=NULL)
  {
    UnitsInBuilding[bt][place]->WarpToXY(GetPosX(), GetPosY(), TMap::GetSingleton());
    UnitsInBuilding[bt][place]->SetLocation(ulAmerica);
    UnitsInBuilding[bt][place]->SetState(usNormal);
  }//if
  //place new unit
  UnitsInBuilding[bt][place] = AUnit;
  if (AUnit!=NULL)
  {
    UnitsInBuilding[bt][place]->SetLocation(ulInColony);
    UnitsInBuilding[bt][place]->SetState(usNormal);
  }//if
}//proc

void TColony::RealignUnitsInBuilding(const TBuildingType bt)
{
  if ((bt>=btArmory) and (bt<=btBlacksmith))
  {
    Byte i;
    for (i= 0; i<=1; ++i)
    {
      if (UnitsInBuilding[bt][i]==NULL)
      {
        UnitsInBuilding[bt][i] = UnitsInBuilding[bt][i+1];
        UnitsInBuilding[bt][i+1] = NULL;
      }//if
    }//for
  }//if
}//proc

ShortInt TColony::GetFirstFreeBuildingSlot(const TBuildingType bt) const
{
  if ((bt>=btArmory) and (bt<=btBlacksmith))
  {
    ShortInt i = 0;
    while (i<=2)
    {
      if (UnitsInBuilding[bt][i]==NULL)
      {
        return i;
      }//if
      i = i+1;
    }//while
  }//if
  return -1;
}//func

Word TColony::GetInhabitants() const
{
  Word Result =0;
  LongInt i, j;
  for (i=0; i<3; ++i)
    for (j=0; j<3; ++j)
      if (UnitsInFields[i][j].u!=NULL) Result = Result+1;
  for (i= Ord(btArmory); i<=Ord(btBlacksmith); ++i)
    for (j= 0; j<3; ++j)
      if (UnitsInBuilding[TBuildingType(i)][j]!=NULL) Result = Result+1;
  return Result;
}//func

bool TColony::AdjacentWater(const TMap& AMap) const
{
  //if AMap=nil then Exit;
  LongInt i, j;
  for (i = PosX-1; i<=PosX+1; ++i)
    for (j= PosY-1; j<=PosY+1; ++j)
    {
      if ((i>=0) and (i<cMap_X) and (j>=0) and (j<cMap_Y))
      {
        if (AMap.tiles[i][j]->IsWater())
        {
          return true;
        }//if
      }
    }//for
  return false;
}//func

bool TColony::SaveToStream(std::ofstream& fs) const
{
  if (!fs.good()) return false;
  fs.write((char*) &m_Nation, sizeof(LongInt));
  if (!fs.good()) return false;
  fs.write((char*) &PosX, sizeof(LongInt));
  if (!fs.good()) return false;
  fs.write((char*) &PosY, sizeof(LongInt));
  if (!fs.good()) return false;
  //name
  LongInt i = m_Name.length();
  fs.write((char*) &i, sizeof(LongInt));
  fs.write(m_Name.c_str(), m_Name.length());
  if (!fs.good()) return false;
  //store
  for (i= Ord(Low(gtFood)); i<=Ord(High(gtFood)); ++i)
  {
    fs.write((char*) &(Store[TGoodType(i)]), sizeof(Word));
    if (!fs.good()) return false;
  }
  //buildings
  for (i= Ord(Low(btNone)); i<=Ord(High(btNone)); ++i)
  {
    fs.write((char*) &(Buildings[TBuildingType(i)]), sizeof(Byte));
    if (!fs.good()) return false;
  }
  //current building under construction
  fs.write((char*) &CurrentConstruction, sizeof(TBuildingType));
  if (!fs.good()) return false;
  //save units in fields
  //-- count them
  LongInt j;
  Byte field_count = 0;
  for (i= -1; i<=1; ++i)
    for (j= -1; j<= 1; ++j)
      if (UnitsInFields[i][j].u!=NULL) field_count = field_count+1;
  fs.write((char*) &field_count, sizeof(Byte));
  if (!fs.good()) return false;
  // -- save them
  for (i= -1; i<=1; ++i)
    for (j= -1; j<= 1; ++j)
      if (UnitsInFields[i][j].u!=NULL)
      {
        fs.write((char*) &i, sizeof(LongInt));
        fs.write((char*) &j, sizeof(LongInt));
        if (!fs.good()) return false;
        if (!(UnitsInFields[i][j].u->SaveToStream(fs))) return false;
        fs.write((char*) &(UnitsInFields[i][j].GoesFor), sizeof(TGoodType));
        if (!fs.good()) return false;
      }//if
  //save units in buildings
  // -- count them
  field_count = 0;
  for (i=Ord(btArmory); i<=Ord(btBlacksmith); ++i)
    for (j=0; j<=2; ++j)
      if (UnitsInBuilding[TBuildingType(i)][j]!=NULL) field_count = field_count+1;
  fs.write((char*) &field_count, sizeof(Byte));
  if (!fs.good()) return false;
  // -- save them
  TBuildingType bt;
  for (i=Ord(btArmory); i<=Ord(btBlacksmith); ++i)
    for (field_count= 0; field_count<=2; ++field_count)
      if (UnitsInBuilding[TBuildingType(i)][field_count]!=NULL)
      {
        bt = TBuildingType(i);
        fs.write((char*) &bt, sizeof(TBuildingType));
        fs.write((char*) &field_count, sizeof(Byte));
        if (!(UnitsInBuilding[TBuildingType(i)][field_count]->SaveToStream(fs))) return false;
      }//if
  return fs.good();
}//func

