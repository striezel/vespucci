#include "Data.h"
#include <iostream>
#include <cstdlib>
#include "PascalFunctions.h"
#include "ParamStr.h"

TData::TData(const LongInt NumNation_Player)
{
  base_dir = "";
  if ((NumNation_Player>=cMinEuropean) and (NumNation_Player<=cMaxEuropean))
    player_nation = NumNation_Player;
  else player_nation = cNationEngland;
  Year = 1492;
  Autumn = false;
  //nations
  LongInt i;
  for (i=cMin_Nations; i<=cMaxIndian; ++i)
    Nations[i] = NULL;
  InitializeNations();
  //units
  m_Units.clear();
  Unit_max = -1;
  //colonies
  m_Colonies.clear();
  Colony_max = -1;
  //tribes
  m_Tribes.clear();
  Tribe_max = -1;
  InitializeMap();
}//construc

TData::~TData()
{
  LongInt i;
  for (i= cMin_Nations; i<=cMaxIndian; ++i)
    if (Nations[i]!=NULL)
    {
      delete Nations[i];
      Nations[i] = NULL;
    }
  DeInitColonies();
  DeInitTribes();
  DeInitUnits();
}//destruc

TData& TData::GetSingleton()
{
  static TData Instance = TData(cNationEngland);
  return Instance;
}

void TData::SetPlayerNation(const LongInt count)
{
  if ((count>=cMinEuropean) and (count<=cMaxEuropean))
    player_nation = count;
  else player_nation = cNationEngland;
}

void TData::InitializeNations()
{
  const TLanguage& lang = TLanguage::GetSingleton();
  Nations[cNationEngland] = new TEuropeanNation(cNationEngland, lang.GetNationName(cNationEngland), "Walter Raleigh");
  Nations[cNationFrance] = new TEuropeanNation(cNationFrance, lang.GetNationName(cNationFrance), "Jacques Cartier");
  Nations[cNationSpain] = new TEuropeanNation(cNationSpain, lang.GetNationName(cNationSpain), "Christoph Columbus");
  Nations[cNationHolland] = new TEuropeanNation(cNationHolland, lang.GetNationName(cNationHolland), "Michiel De Ruyter");
  LongInt i;
  for (i=cMinIndian; i<=cMaxIndian; ++i)
    Nations[i] = new TIndianNation(i, lang.GetNationName(i));
}//proc

void TData::InitTribes_America()
{
  LongInt i;
  for (i= 0; i<cTribeLocationsCount; ++i)
  {
    NewTribe(cTribeLocations[i].x, cTribeLocations[i].y, cTribeLocations[i].Nation, utSugarplanter);
  }//for
}//proc

void TData::InitializeMap()
{
  TMap& ref_Map = TMap::GetSingleton();
  if (FileExists(GetPathBase()+america_map_path))
  {
    if (ref_Map.LoadFromFile(GetPathBase()+america_map_path))
    {
      std::cout << "Map \""+GetPathBase()+america_map_path+"\" successfully loaded.\n";
      InitTribes_America();
    }//if
    else
    {
      std::cout << "Couldn't load map file \""+GetPathBase()+america_map_path+"\" properly. Using generation routine instead.\n";
      ref_Map.Generate(0.7);
    }
  }
  else
  {
    std::cout << "Couldn't find map file \""+GetPathBase()+america_map_path+"\". Using generation routine instead.\n";
    ref_Map.Generate(0.7);
  }
  ref_Map.GenerateSpecials();
}//proc

void TData::DeInitColonies()
{
  LongInt i;
  for (i= m_Colonies.size()-1; i>=0; --i)
  {
    if (m_Colonies[i]!=NULL)
    {
      delete m_Colonies[i];
      m_Colonies[i] = NULL;
    }
    m_Colonies.pop_back();
  }
  m_Colonies.clear();
  Colony_max = -1;
}//proc

void TData::DeInitTribes()
{
  LongInt i;
  for (i= m_Tribes.size()-1; i>=0; --i)
  {
    if (m_Tribes[i]!=NULL)
    {
      delete m_Tribes[i];
      m_Tribes[i] = NULL;
    }
    m_Tribes.pop_back();
  }
  m_Tribes.clear();
  Tribe_max = -1;
}//proc

void TData::DeInitUnits()
{
  LongInt i;
  for (i= m_Units.size()-1; i>=0; --i)
  {
    if (m_Units[i]!=NULL)
    {
      delete m_Units[i];
      m_Units[i] = NULL;
    }
    m_Units.pop_back();
  }
  m_Units.clear();
  Unit_max = -1;
}//proc

LongInt TData::GetYear() const
{
  return Year;
}//func

bool TData::IsAutumn() const
{
  return Autumn;
}//func

void TData::AdvanceYear()
{
  if (Year<1600) Year = Year+1;
  else
  {
    if (Autumn)
    {
      //if we have autumn, start next year and set season to spring
      Year = Year+1;
      Autumn = false;
    }
    else Autumn = true;
  }//else
}//proc

LongInt TData::PlayerNation() const
{
  return player_nation;
}//func

TNation* TData::GetNation(const LongInt count) const
{
  if ((count>cMaxIndian) or (count<cMin_Nations))
    return NULL;
  else
    return Nations[count];
}//func

TUnit* TData::NewUnit(const TUnitType TypeOfUnit, const LongInt ANation, const LongInt X, const LongInt Y)
{
  TUnit* temp = new TUnit(TypeOfUnit, ANation, X, Y);
  m_Units.push_back(temp);
  return temp;
}//func

TUnit* TData::GetFirstUnitInXY(const LongInt x, const LongInt y, const bool OnlyAmerica) const
{
  LongInt i;
  for (i= 0; i<m_Units.size(); ++i)
  {
    if ((m_Units[i]->GetPosX()==x) and (m_Units[i]->GetPosY()==y))
    {
      if ((m_Units[i]->GetLocation()==ulAmerica) or (!OnlyAmerica))
      {
        return m_Units[i];
        break;
      }//if
    }//if
  }
  return NULL;
}//func

TUnit* TData::GetFirstLazyUnit(const LongInt num_Nation) const
{
  LongInt i;
  for (i= 0; i<m_Units.size(); ++i)
    if (m_Units[i]!=NULL)
    {
      if ((m_Units[i]->MovesLeft>0) and (m_Units[i]->GetNation()==num_Nation) and (m_Units[i]->GetLocation()==ulAmerica))
      {
        return m_Units[i];
        break;
      }//if
    }//if
  return NULL;
}//func

TUnitArr TData::GetAllShips(const LongInt numNation) const
{
  TUnitArr Result;
  Result.clear();
  LongInt i;
  for (i= 0; i<m_Units.size(); ++i)
    if (m_Units[i]!=NULL)
    {
      if ((m_Units[i]->GetNation()==numNation) and (m_Units[i]->IsShip()))
      {
        Result.push_back(m_Units[i]);
      }//if
    }
  return Result;
}//func

TUnitArr TData::GetAllShipsInXY(const LongInt x, const LongInt y, const bool OnlyAmerica) const
{
  TUnitArr Result;
  Result.clear();
  LongInt i;
  for (i= 0; i<m_Units.size(); ++i)
    if (m_Units[i]!=NULL)
    {
      if ((m_Units[i]->GetPosX()==x) and (m_Units[i]->GetPosY()==y) and (m_Units[i]->FreightCapacity()>0))
      {
        if ((m_Units[i]->GetLocation()==ulAmerica) or (!OnlyAmerica))
        {
          Result.push_back(m_Units[i]);
        }//if
      }//if
    }//if <>nil
  return Result;
}//func

TUnitArr TData::GetAllShipsInEurope(const LongInt num_nation) const
{
  TUnitArr Result;
  Result.clear();
  LongInt i;
  for (i= 0; i<m_Units.size(); ++i)
    if (m_Units[i]!=NULL)
    {
      if ((m_Units[i]->GetLocation()==ulEurope) and (m_Units[i]->IsShip()) and(m_Units[i]->GetNation()==num_nation))
      {
        Result.push_back(m_Units[i]);
      }//if
    }//if <>nil
  return Result;
}//func

TUnitArr TData::GetAllNonShipsInEurope(const LongInt num_nation) const
{
  TUnitArr Result;
  Result.clear();
  LongInt i;
  for (i= 0; i<m_Units.size(); ++i)
    if (m_Units[i]!=NULL)
    {
      if ((m_Units[i]->GetLocation()==ulEurope) and(m_Units[i]->GetNation()==num_nation) and (!m_Units[i]->IsShip()))
      {
        Result.push_back(m_Units[i]);
      }//if
    }//if <>nil
  return Result;
}//func

TUnitArr TData::GetAllShipsGoingToEurope(const LongInt num_nation) const
{
  TUnitArr Result;
  Result.clear();
  LongInt i;
  for (i= 0; i<m_Units.size(); ++i)
    if (m_Units[i]!=NULL)
    {
      if ((m_Units[i]->GetLocation()==ulGoToEurope) and(m_Units[i]->GetNation()==num_nation))
      {
        Result.push_back(m_Units[i]);
      }//if
    }//if <>nil
  return Result;
}//func

TUnitArr TData::GetAllShipsGoingToNewWorld(const LongInt num_nation) const
{
  TUnitArr Result;
  Result.clear();
  LongInt i;
  for (i= 0; i< m_Units.size(); ++i)
    if (m_Units[i]!=NULL)
    {
      if ((m_Units[i]->GetLocation()==ulGoToNewWorld) and(m_Units[i]->GetNation()==num_nation))
      {
        Result.push_back(m_Units[i]);
      }//if
    }//if <>nil
  return Result;
}//func

void TData::GetEuropeanQuartett(const LongInt num_nation, TUnitArr& Ships, TUnitArr& People, TUnitArr& ExpectedSoon, TUnitArr& ToNewWorld) const
{
  Ships.clear();
  People.clear();
  ExpectedSoon.clear();
  ToNewWorld.clear();
  LongInt i;
  for (i= 0; i<m_Units.size(); ++i)
    if (m_Units[i]!=NULL)
    {
      if (m_Units[i]->GetNation()==num_nation)
      {
        switch (m_Units[i]->GetLocation())
        {
          case ulEurope:
               if (m_Units[i]->FreightCapacity()>0)
               {
                 Ships.push_back(m_Units[i]);
               }//if
               else
               {
                 People.push_back(m_Units[i]);
               }//else
               break;//ulEurope
          case ulGoToEurope:
               ExpectedSoon.push_back(m_Units[i]);
               break;//ulGoToEurope
          case ulGoToNewWorld:
               ToNewWorld.push_back(m_Units[i]);
               break;//ulGoToNewWorld
        }//swi
      }//if
    }//if
}//proc

TUnitArr TData::GetAllUnitsInColony(const TColony* ACol) const
{
  TUnitArr Result;
  Result.clear();
  if (ACol!=NULL)
  {
    LongInt i;
    for (i= 0; i<m_Units.size(); ++i)
      if (m_Units[i]!=NULL)
        if ((m_Units[i]->GetLocation()==ulAmerica) and (m_Units[i]->GetPosX()==ACol->GetPosX()) and (m_Units[i]->GetPosY()==ACol->GetPosY()) and (m_Units[i]->FreightCapacity()==0))
        {
          Result.push_back(m_Units[i]);
        }//if
  }//if
  return Result;
}//func

TColony* TData::NewColony(const Byte x, const Byte y, const LongInt num_Nation, const std::string& AName)
{
  TColony* temp = new TColony(x, y, num_Nation, AName);
  m_Colonies.push_back(temp);
  return temp;
}//func

TColony* TData::GetColonyInXY(const Byte x, const Byte y) const
{
  LongInt i;
  for (i= 0; i<m_Colonies.size(); ++i)
    if (m_Colonies[i]!=NULL)
      if ((m_Colonies[i]->GetPosX()==x) and (m_Colonies[i]->GetPosY()==y))
      {
        return m_Colonies[i];
        break;
      }//if
  return NULL;
}//func

TColonyArr TData::GetColonyList(const LongInt num_nation) const
{
  TColonyArr Result;
  Result.clear();
  LongInt i;
  for (i= 0; i<m_Colonies.size(); ++i)
    if (m_Colonies[i]!=NULL)
    {
      if (m_Colonies[i]->GetNation()==num_nation)
      {
        Result.push_back(m_Colonies[i]);
      }//if
    }//if <>nil
  return Result;
}//func

bool TData::DeleteColony(const Byte x, const Byte y)
{
  LongInt i;
  for (i= 0; i<m_Colonies.size(); ++i)
    if (m_Colonies[i]!=NULL)
      if ((m_Colonies[i]->GetPosX()==x) and (m_Colonies[i]->GetPosY()==y))
      {
        delete m_Colonies[i];
        m_Colonies[i] = NULL;
        m_Colonies[i] = m_Colonies[m_Colonies.size()-1];
        m_Colonies[m_Colonies.size()-1] = NULL;
        m_Colonies.pop_back();
        return true;
        break;
      }//if
  return false;
}//func

TTribe* TData::NewTribe(const Byte x, const Byte y, const LongInt num_Nation, const TUnitType Teaches)
{
  TTribe* temp = new TTribe(x, y, num_Nation, Teaches);
  m_Tribes.push_back(temp);
  return temp;
}//func

TTribe* TData::GetTribeInXY(const Byte x, const Byte y) const
{
  LongInt i;
  for (i= 0; i<m_Tribes.size(); ++i)
    if (m_Tribes[i]!=NULL)
      if ((m_Tribes[i]->GetPosX()==x) and (m_Tribes[i]->GetPosY()==y))
      {
        return m_Tribes[i];
        break;
      }//if
  return NULL;
}//func

bool TData::FreeForSettlement(const Byte x, const Byte y) const
{
  LongInt i,j;
  for (i= x-2; i<=x+2; ++i)
    for (j= y-2; j<=y+2; ++j)
      if ((i>=0) and (j>=0))
        if (GetColonyInXY(i,j)!=NULL)
        {
          return false;
        }//if
  if (GetTribeInXY(x,y)!=NULL) return false;
  return true;
}//func

void TData::NewRound(const LongInt num_Nation)
{
  //call NewRound method for every unit of that nation
  LongInt i;
  for (i= 0; i<m_Units.size(); ++i)
    if (m_Units[i]!=NULL)
      if (m_Units[i]->GetNation()==num_Nation)
        m_Units[i]->NewRound();
  //call NewRound method for every colony
  if (not Autumn) //only in spring we produce, to avoid production twice a year
    for (i= 0; i<=m_Colonies.size(); ++i)
      if (m_Colonies[i]!=NULL)
        if (m_Colonies[i]->GetNation()==num_Nation)
        {
          m_Colonies[i]->NewRound(TMap::GetSingleton());
          //following should be implemented in TColony and not here
          if (m_Colonies[i]->GetStore(gtFood)>=200)
          {
            //time for new inhabitant
            m_Colonies[i]->RemoveFromStore(gtFood, 200);
            //creates new unit and sets its location to America
            NewUnit(utColonist, num_Nation, m_Colonies[i]->GetPosX(), m_Colonies[i]->GetPosY())->SetLocation(ulAmerica);
          }//if
        }//if
}//proc

bool TData::SaveData(const Word n, std::string& err)
{
  /* files:
      data<n>.vdd - simple data
      map<n>.vmd - map itself
      units<n>.vud - all units
      colony<n>.vcd - all colonies
      nations<n>.vnd - european nations
  */
  err = "no error";
  if (!DirectoryExists(GetPathBase()+save_path))
    if (!ForceDirectories(GetPathBase()+save_path))
    {
      err = "TData::SaveData(): could not create directory \""+GetPathBase()+save_path+"\" for saves.";
      return false;
    }//if

  std::ofstream fs;
  fs.open((GetPathBase()+save_path +"data"+IntToStr(n)+".vdd").c_str());
  if (!fs)
  {
    err = "TData::SaveData: could not create file \""+GetPathBase()+save_path +"data"+IntToStr(n)+".vdd"+"\".";
    return false;
  }

  fs.write(cDataFileHeader.c_str(), cDataFileHeader.length());
  fs.write((char*) &Year, sizeof(Year));
  fs.write((char*) &Autumn, sizeof(Autumn));
  fs.write((char*) &player_nation, sizeof(player_nation));
  if (!fs.good()) { fs.close(); return false; }
  //write player's name
  std::string temp_str = (static_cast<TEuropeanNation*> (GetNation(player_nation)))->GetLeaderName();
  LongInt temp = temp_str.length();
  fs.write((char*) &temp, sizeof(LongInt));
  fs.write(temp_str.c_str(), temp);
  fs.close();
  if (not fs.good())
  {
    err = "TData::SaveData: Error while writing data file \""+GetPathBase()+save_path +"data"+IntToStr(n)+".vdd";
    return false;
  }//if

  //map

  if (!TMap::GetSingleton().SaveToFile(GetPathBase()+save_path +"map"+IntToStr(n)+".vmd"))
  {
    err = "TData::SaveData: Error while writing map file \""+GetPathBase()+save_path +"map"+IntToStr(n)+".vmd";
    return false;
  }//if

  //units
  temp = 0;
  LongInt i;
  for (i=0; i<m_Units.size(); ++i)
    if (m_Units[i]!=NULL)
      if ((m_Units[i]->GetLocation()!=ulEmbarked) and (m_Units[i]->GetLocation()!=ulInColony)) temp = temp+1;

  fs.open((GetPathBase()+save_path +"units"+IntToStr(n)+".vud").c_str());
  if (!fs)
  {
    err = "TData::SaveData: could not create unit file \""+GetPathBase()+save_path +"units"+IntToStr(n)+".vud\".";
    return false;
  }
  fs.write(cUnitFileHeader.c_str(), cUnitFileHeader.length());
  fs.write((char*) &temp, sizeof(LongInt));
  if (!fs.good()) { fs.close(); return false; }
  for (i= 0; i<m_Units.size(); ++i)
    if (m_Units[i]!=NULL)
      if ((m_Units[i]->GetLocation()!=ulEmbarked) and (m_Units[i]->GetLocation()!=ulInColony))
        if (!m_Units[i]->SaveToStream(fs))
        {
          err = "TData::SaveData: Error while saving units.";
          fs.close();
          return false;
        }
  fs.close();
  if (!fs.good())
  {
    err = "TData::SaveData: Error while writing unit file \""+GetPathBase()+save_path +"units"+IntToStr(n)+".vud";
    return false;
  }//if

  //colonies
  temp = 0;
  for (i =0; i<m_Colonies.size(); ++i)
    if (m_Colonies[i]!=NULL) temp = temp+1;

  fs.open((GetPathBase()+save_path +"colony"+IntToStr(n)+".vcd").c_str());
  if (!fs) return false;
  fs.write(cColonyFileHeader.c_str(), cColonyFileHeader.length());
  fs.write((char*) &temp, sizeof(LongInt));
  if (!fs.good()) { fs.close(); return false; }
  for (i= 0; i<m_Colonies.size(); ++i)
    if (m_Colonies[i]!=NULL)
    {
      if (!m_Colonies[i]->SaveToStream(fs))
      {
        fs.close();
        err = "TData::SaveData: error while writing colony file.";
        return false;
      }
    }
  fs.close();
  if (not fs.good())
  {
    err = "TData.SaveData: Error while writing colony file \""+GetPathBase()+save_path +"colony"+IntToStr(n)+".vcd\".";
    return false;
  }

  //nations
  fs.open((GetPathBase()+save_path +"nations"+IntToStr(n)+".vnd").c_str());
  if (!fs)
  {
    return false;
  }

  for (i= cMinEuropean; i<=cMaxEuropean; ++i)
    if (!Nations[i]->SaveToStream(fs))
    {
      fs.close();
      err = "TData::SaveData: Error while writing nation file.";
      return false;
    }
  fs.close();
  if (!fs.good())
  {
    err = "TData::SaveData: Error while writing nation file \""+GetPathBase()+save_path +"nations"+IntToStr(n)+".vnd\".";
    return false;
  }
  return fs.good();
}//func SaveData

bool TData::LoadData(const Word n, std::string& err)
{
  DeInitTribes();
  //if m_Map=nil then InitializeMap;
  if (!DirectoryExists(GetPathBase()+save_path))
  {
    err = "TData::LoadData: could not find directory \""+GetPathBase()+save_path+"\".";
    return false;
  }//if
  /* files:
      data<n>.vdd - simple data
      map<n>.vmd - map itself
      units<n>.vud - all units
      colony<n>.vcd - all colonies
      nations<n>.vnd - european nations
  */
  if (! (FileExists(GetPathBase()+save_path+"data"+IntToStr(n)+".vdd") and
         FileExists(GetPathBase()+save_path+"map"+IntToStr(n)+".vmd") and
         FileExists(GetPathBase()+save_path+"units"+IntToStr(n)+".vud") and
         FileExists(GetPathBase()+save_path+"colony"+IntToStr(n)+".vcd")))
  {
    err = "TData::LoadData: could not find one or more of the needed files.";
    return false;
  }//if
  err = "no error";

  //data file

  std::ifstream fs;
  fs.open((GetPathBase()+save_path +"data"+IntToStr(n)+".vdd").c_str());
  if (!fs)
  {
    err = "TData::LoadData: could not open file \""+GetPathBase()+save_path +"data"+IntToStr(n)+".vdd\" for reading.";
    return false;
  }

  char buffer[256];
  fs.read(buffer, cDataFileHeader.length());
  buffer[cDataFileHeader.length()] = '\0';
  std::string temp_str = std::string(buffer);
  if (temp_str!=cDataFileHeader)
  {
    fs.close();
    err = "TData.LoadData: invalid data file header.";
    return false;
  }//if
  fs.read((char*) &Year, sizeof(Year));
  fs.read((char*) &Autumn, sizeof(Autumn));
  fs.read((char*) &player_nation, sizeof(player_nation));
  if (!fs.good()) { fs.close(); return false; }
  if (player_nation<0)
  {
    err = "TData::LoadData: got invalid nation count from data file.";
    fs.close();
    return false;
  }//if
  //read player's name
  LongInt temp =0;
  fs.read((char*) &temp, sizeof(LongInt));
  if ((temp<1) or (temp>255))
  {
    fs.close();
    err = "TData::LoadData: got invalid string length from data file.";
    return false;
  }//if

  fs.read(buffer, temp);
  buffer[temp] = '\0';
  temp_str = std::string(buffer);
  if (!fs.good())
  {
    err = "TData::LoadData: Error while reading data file \""+GetPathBase()+save_path +"data"+IntToStr(n)+".vdd\".";
    fs.close();
    return false;
  }
  fs.close();

  //nations
  LongInt i;
  for (i= cMin_Nations; i<=cMaxIndian; ++i)
    if (Nations[i]!=NULL)
    {
      delete Nations[i];
      Nations[i] = NULL;
    }//if
  InitializeNations();
  TNation* temp_nat = GetNation(player_nation);
  if (temp_nat!=NULL)
  {
    if (temp_nat->IsEuropean())
      (static_cast<TEuropeanNation*>(temp_nat))->ChangeLeaderName(temp_str);
    else
    {
      err = "TData.LoadData: got Indian nation for player.";
      return false;
    }//else
  }//if

  //load the map
  if (!TMap::GetSingleton().LoadFromFile(GetPathBase()+save_path+"map"+IntToStr(n)+".vmd"))
  {
    err = "TData::LoadData: error while loading map from \""+GetPathBase()+save_path+"map"+IntToStr(n)+".vmd\".";
    return false;
  }//if

  //load units
  DeInitUnits();

  fs.open((GetPathBase()+save_path +"units"+IntToStr(n)+".vud").c_str());
  if (!fs)
  {
    err = "TData::LoadData: could not open unit file \""+GetPathBase()+save_path +"units"+IntToStr(n)+".vud\" for reading.";
    return false;
  }//if

  fs.read(buffer, cUnitFileHeader.length());
  buffer[cUnitFileHeader.length()] = '\0';
  temp_str = std::string(buffer);
  if (temp_str!=cUnitFileHeader)
  {
    fs.close();
    err = "TData::LoadData: got invalid unit file header.";
    return false;
  }//if
  temp =0;
  fs.read((char*) &temp, sizeof(LongInt));
  if ((temp<0) or (!fs.good()))
  {
    fs.close();
    err = "TData::LoadData: got invalid unit count.";
    return false;
  }//if

  TUnit* temp_unit = NULL;
  for (i= 1; i<=temp; ++i)
  {
    temp_unit = NewUnit(utCriminal, cNationEngland, 1,1);
    if (!LoadUnitFromStream(temp_unit, fs))
    {
      fs.close();
      err = "TData::LoadData: error while reading unit file \""+GetPathBase()+save_path +"units"+IntToStr(n)+".vud\".";
      return false;
    }
  }//for
  fs.close();

  if (!fs.good())
  {
    err = "TData::LoadData: error while reading unit file \""+GetPathBase()+save_path +"units"+IntToStr(n)+".vud\".";
    return false;
  }//if

  //load colonies
  DeInitColonies();
  fs.open((GetPathBase()+save_path +"colony"+IntToStr(n)+".vcd").c_str());
  if (!fs)
  {
    err = "TData::LoadData: could not open colony file \""+GetPathBase()+save_path +"colony"+IntToStr(n)+".vcd\".";
    return false;
  }//tryxcept
  buffer[0] = '\0';
  fs.read(buffer, cColonyFileHeader.length());
  buffer[cColonyFileHeader.length()] = '\0';
  temp_str = std::string(buffer);
  if ((temp_str!=cColonyFileHeader) or (!fs.good()))
  {
    fs.close();
    err = "TData::LoadData: invalid colony file header.";
    return false;
  }//if
  fs.read((char*) temp, sizeof(LongInt)); //colony count
  if ((temp<0) or (!fs.good()))
  {
    fs.close();
    err = "TData.LoadData: got invalid colony count.";
    return false;
  }//if

  TColony* temp_colony = NULL;
  for (i= 1; i<=temp; ++i)
  {
    temp_colony = NewColony(1,1, cNationEngland, "new colony "+IntToStr(i));
    if (!LoadColonyFromStream(temp_colony, fs))
    {
      fs.close();
      return false;
    }
  }//for
  fs.close();

  if (!fs.good())
  {
    err = "TData.LoadData: error while loading colonies.";
    return false;
  }//if

  //nations
  for (i= cMin_Nations; i<=cMaxIndian; ++i)
    if (Nations[i]!=NULL)
    {
      delete Nations[i];
      Nations[i] = NULL;
    }//if
  InitializeNations();

  fs.open((GetPathBase()+save_path +"nations"+IntToStr(n)+".vnd").c_str());
  if (!fs)
  {
    err = "TData.LoadData: could not open file \""+GetPathBase()+save_path +"nations"+IntToStr(n)+".vnd\" for reading.";
    return false;
  }//if
  for (i= cMinEuropean; i<=cMaxEuropean; ++i)
  {
    if (!LoadNationFromStream(Nations[i], fs))
    {
      fs.close();
      return false;
    }
    if (Nations[i]->GetCount()!=i)
    {
      fs.close();
      return false;
    }
  }//for
  fs.close();

  if (!fs.good())
  {
    err = "TData.LoadData: error while loading nations.";
    return false;
  }//if
  return true;
}//func LoadData

bool TData::LoadUnitFromStream(TUnit* AUnit, std::ifstream& fs)
{
  if ((!fs.good()) or (AUnit==NULL)) return false;
  fs.read((char*) &(AUnit->MovesLeft), sizeof(AUnit->MovesLeft));
  LongInt px, py;
  fs.read((char*) &px, sizeof(LongInt));
  fs.read((char*) &py, sizeof(LongInt));
  if (!fs.good()) return false;
  AUnit->WarpToXY(px, py, TMap::GetSingleton());
  TUnitType ut;
  fs.read((char*) &ut, sizeof(TUnitType));
  if (!fs.good()) return false;
  AUnit->ChangeType(ut);
  TUnitLocation ul;
  fs.read((char*) &ul, sizeof(TUnitLocation));
  if (!fs.good()) return false;
  AUnit->SetLocation(ul);
  TUnitState us;
  fs.read((char*) &us, sizeof(TUnitState));
  if (!fs.good()) return false;
  AUnit->SetState(us);
  Byte count = 0;
  fs.read((char*) &count, sizeof(Byte));
  if (!fs.good()) return false;
  AUnit->SetRoundsInOpenSea(count);
  LongInt i;
  fs.read((char*) &i, sizeof(LongInt));
  if (!fs.good()) return false;
  AUnit->ChangeNation(i);
  fs.read((char*) &count, sizeof(Byte));
  if (!fs.good()) return false;
  AUnit->ChangeAllItems(count);
  //cargo load
  TGoodType gt;
  for (i= 0; i<6; ++i)
  {
    fs.read((char*) &count, sizeof(Byte));
    fs.read((char*) &gt, sizeof(TGoodType));
    if (!fs.good()) return false;
    AUnit->SetCargo(i, count, gt);
  }//func
  //load passengers
  AUnit->DropAllPassengers();
  count = 0;
  fs.read((char*) &count, sizeof(Byte));
  if ((count>6) or (!fs.good()))
  {
    return false;
  };//if
  TUnit* temp_unit = NULL;
  for (i= 0; i<=count; ++i)
  {
    temp_unit = NewUnit(utCriminal, cNationEngland, 1,1);
    if (!LoadUnitFromStream(temp_unit, fs))
    {
      return false;
    }
    if (!AUnit->LoadUnit(temp_unit)) return false;
  }//for
  //tasks are not yet saved, and thus not loaded
  AUnit->SetTask(NULL);
  return true;
}//func

bool TData::LoadColonyFromStream(TColony* AColony, std::ifstream& fs)
{
  if ((!fs.good()) or (AColony==NULL)) return false;
  LongInt i, j;
  fs.read((char*) &i, sizeof(LongInt));
  AColony->ChangeNation(i);
  fs.read((char*) &i, sizeof(LongInt));
  fs.read((char*) &j, sizeof(LongInt));
  AColony->SetPosition(i, j);
  //name
  fs.read((char*) &i, sizeof(LongInt));
  if ((!fs.good()) or (i<1) or (i>255)) return false;
  char buffer[256];
  fs.read(buffer, i);
  buffer[i] = '\0';
  if (!fs.good()) return false;
  AColony->SetName(std::string(buffer));
  //store
  Word temp_Word;
  for (i= Ord(Low(gtFood)); i<=Ord(High(gtFood)); ++i)
  {
    fs.read((char*) &temp_Word, sizeof(Word));
    if (!fs.good()) return false;
    AColony->SetStore(TGoodType(i), temp_Word);
  }//for
  //buildings
  Byte temp_b;
  for (i= Ord(Low(btNone)); i<=Ord(High(btNone)); ++i)
  {
    fs.read((char*) &temp_b, sizeof(Byte));
    AColony->SetBuildingLevel(TBuildingType(i), temp_b);
  }//for
  //current building under construction
  TBuildingType bt;
  fs.read((char*) &bt, sizeof(TBuildingType));
  if (!fs.good()) return false;
  AColony->SetCurrentConstruction(bt);

  //*** units in buildings and units in fields ***
  //fields
  for (i= -1; i<=1; ++i)
    for (j= -1; j<=1; ++j)
      AColony->SetUnitInField(i,j, NULL, gtFood);
  //load fields
  // -- count
  Byte count=255;
  fs.read((char*) &count, sizeof(Byte));
  if (count>8) return false;
  LongInt f_x, f_y;
  TUnit* temp_unit;
  TGoodType gt;
  for (i= 1; i<=count; ++i)
  {
    fs.read((char*) &f_x, sizeof(LongInt));
    fs.read((char*) &f_y, sizeof(LongInt));
    if ((abs(f_x)>1) or (abs(f_y)>1) or (AColony->GetUnitInField(f_x,f_y)!=NULL))
    {
      //invalid x/y-values or unit in field is already present
      return false;
    }//if
    if (!fs.good()) return false;
    temp_unit = NewUnit(utCriminal, cNationEngland, 1,1);
    if (!LoadUnitFromStream(temp_unit, fs))
    {
      return false;
    }
    fs.read((char*) &gt, sizeof(TGoodType));
    if (!fs.good()) return false;
    AColony->SetUnitInField(f_x,f_y, temp_unit, gt);
  }//for
  //buildings
  for (i= Ord(btArmory); i<=Ord(btBlacksmith); ++i)
    for (j= 0; j<3; ++j)
      AColony->SetUnitInBuilding(TBuildingType(i),j,NULL);
  //load units in buildings
  // -- count
  fs.read((char*) &count, sizeof(Byte));
  if (!fs.good()) return false;
  for (i= 1; i<=count; ++i)
  {
    temp_unit = NewUnit(utCriminal, cNationEngland, 1,1);
    fs.read((char*) &bt, sizeof(TBuildingType));
    fs.read((char*) &temp_b, sizeof(Byte));
    if (!fs.good()) return false;
    if ((bt<btArmory) or (bt>btBlacksmith) or (temp_b>2) or (AColony->GetUnitInBuilding(bt, temp_b)!=NULL))
    {
      return false;
    }//if
    if(!LoadUnitFromStream(temp_unit, fs)) return false;
    AColony->SetUnitInBuilding(bt, temp_b, temp_unit);
  }//for
  return fs.good();
}//func

bool TData::LoadNationFromStream(TNation* ANat, std::ifstream& fs)
{
  if ((!fs.good()) or (ANat==NULL)) return false;
  LongInt i;
  fs.read((char*) &i, sizeof(LongInt));
  if (!fs.good()) return false;
  ANat->ChangeCount(i);
  fs.read((char*) &i, sizeof(LongInt));
  if ((i<=0) or (i>255))
    return false; //string to short or to long
  char buffer[256];
  fs.read(buffer, i);
  if ((!fs.good()) or (fs.gcount()!=i))
    return false;
  buffer[i] = '\0';
  ANat->ChangeName(Trim(std::string(buffer)));
  if (ANat->IsEuropean())
  {
    fs.read((char*) &i, sizeof(LongInt));
    if ((i<=0) or (i>255))
      return false; //string to short or to long
    fs.read(buffer, i);
    buffer[i] = '\0';
    if (!fs.good()) return false;
    (static_cast<TEuropeanNation*> (ANat))->ChangeLeaderName(Trim(std::string(buffer)));
    fs.read((char*) &i, sizeof(LongInt));
    if (!fs.good()) return false;
    (static_cast<TEuropeanNation*> (ANat))->IncreaseGold(i-(static_cast<TEuropeanNation*> (ANat))->GetGold());
    Byte tr;
    fs.read((char*) &tr, sizeof(Byte));
    if (!fs.good()) return false;
    (static_cast<TEuropeanNation*> (ANat))->ChangeTaxRate(tr);
    bool Boycott;
    for (i= Ord(Low(gtFood)); i<= Ord(High(gtFood)); ++i)
    {
      fs.read((char*) &Boycott, sizeof(bool));
      if (!fs.good()) return false;
      if (Boycott) (static_cast<TEuropeanNation*> (ANat))->DoBoycott(TGoodType(i));
      else (static_cast<TEuropeanNation*> (ANat))->UndoBoycott(TGoodType(i));
      fs.read((char*) &tr, sizeof(Byte));
      if (!fs.good()) return false;
      (static_cast<TEuropeanNation*> (ANat))->ChangePrice(TGoodType(i), tr);
    }//for
  }//if
  return fs.good();
}//func

std::string TData::GetSaveInfo(const Word n)
{
  if ((n==0) or !FileExists(GetPathBase()+save_path +"data"+IntToStr(n)+".vdd"))
    return "("+TLanguage::GetSingleton().GetOthers(osEmpty)+")";

  bool status = true;
  std::ifstream fs;
  fs.open((GetPathBase()+save_path +"data"+IntToStr(n)+".vdd").c_str());
  if (!fs)
  {
    return "("+TLanguage::GetSingleton().GetOthers(osEmpty)+")";
  }//if
  char buffer[256];
  fs.read(buffer, cDataFileHeader.length());
  buffer[cDataFileHeader.length()] = '\0';
  std::string temp_str = std::string(buffer);
  if ((!fs.good()) or (temp_str!=cDataFileHeader))
  {
    fs.close();
    return "("+TLanguage::GetSingleton().GetOthers(osEmpty)+")";
  }//if
  LongInt temp_Year, temp_nation;
  fs.read((char*) &temp_Year, sizeof(temp_Year));
  bool temp_Autumn;
  fs.read((char*) &temp_Autumn, sizeof(temp_Autumn));
  fs.read((char*) &temp_nation, sizeof(player_nation));
  if (!fs.good())
  {
    fs.close();
    return "("+TLanguage::GetSingleton().GetOthers(osEmpty)+")";
  }
  //read player's name
  LongInt temp_len = -1;
  fs.read((char*) &temp_len, sizeof(LongInt));
  if ((!fs.good()) or (temp_len>255) or (temp_len<0))
  {
    fs.close();
    return "("+TLanguage::GetSingleton().GetOthers(osEmpty)+")";
  }
  fs.read(buffer, temp_len);
  fs.close();
  if (fs.good())
    return temp_str+", "+TLanguage::GetSingleton().GetNationName(temp_nation)+", "
           +TLanguage::GetSingleton().GetSeason(temp_Autumn)+" "+IntToStr(temp_Year);
  else return "("+TLanguage::GetSingleton().GetOthers(osEmpty)+")";
}//func

TStringArr TData::GetSaveSlots()
{
  /*data<n>.vdd - simple data
      map<n>.vmd - map itself
      units<n>.vud - all units
      colony<n>.vcd - all colonies
      nations<n>.vnd - european nations*/

  TStringArr Result = TStringArr(10, "");
  LongInt i;
  for (i=1; i<=10; ++i)
  {
    if (FileExists(GetPathBase()+save_path +"data"+IntToStr(i)+".vdd") and FileExists(GetPathBase()+save_path +"map"+IntToStr(i)+".vmd") and FileExists(GetPathBase()+save_path +"units"+IntToStr(i)+".vud") and FileExists(GetPathBase()+save_path +"colony"+IntToStr(i)+".vcd") and FileExists(GetPathBase()+save_path +"nations"+IntToStr(i)+".vnd"))
    Result[i-1] = GetSaveInfo(i);
    else Result[i-1] = "("+TLanguage::GetSingleton().GetOthers(osEmpty)+")";
  }//for
  return Result;
}//func

std::string TData::GetPathBase()
{
  if (base_dir=="")
  {
    base_dir = ParamStr(0);
    LongInt i;
    i = base_dir.length()-1;
    while (i>=0)
    {
      if (base_dir[i]==path_delimiter[0]) break;
      i = i-1;
    }//while
    base_dir = base_dir.substr(0, i+1); //copy(base_dir, 1, i);
  }//if
  return base_dir;
}//func

TStringArr TData::GetJobList(const ShortInt x_shift, const ShortInt y_shift, const TUnitType UnitType, const TColony* ACol) const
{
  TStringArr Result = TStringArr(Ord(gtSilver)-Ord(gtFood)+1, TLanguage::GetSingleton().GetOthers(osEmpty));
  if ((abs(x_shift)>1) or (abs(y_shift)>1) or (ACol==NULL) /*or (m_Map=nil)*/) return Result;

  LongInt i;
  TLanguage& lang = TLanguage::GetSingleton();
  TMap& ref_Map = TMap::GetSingleton();
  for (i= Ord(gtFood); i<=Ord(gtSilver); ++i)
  {
    Result[i] = lang.GetUnitName(GetUnitForGood(TGoodType(i)))+":  "
               +IntToStr(ref_Map.tiles[ACol->GetPosX()+x_shift][ACol->GetPosY()+y_shift]->GetGoodProduction(TGoodType(i), HasExpertStatus(TGoodType(i), UnitType)))
               +" "+lang.GetGoodName(TGoodType(i));
  }//for
  return Result;
}//func

