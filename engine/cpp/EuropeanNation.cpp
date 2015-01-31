/* ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2010, 2011  Dirk Stolle

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

#include "EuropeanNation.hpp"
#include <cstring>
#include <iostream>
#include "PascalFunctions_Goods.hpp"
#include "PascalFunctions_FoundingFathers.hpp"
#include "Randomizer.hpp"

//**** functions of TEuropeanNation ****

TEuropeanNation::TEuropeanNation(const LongInt num, const std::string& NameStr,
                    const std::string& NameOfLeader)
: TNation(num, NameStr),
  m_Leader(NameOfLeader),
  m_Gold(1000),
  m_TaxRate(0)

/*var gt: TGoodType;
    ff: TFoundingFathers;
    i: Integer; */
{
  //inherited Create(num, NameStr);
  //check number and pick some default value to make sure it's European
  if ((num<cMinEuropean) || (num>cMaxEuropean))
    m_count = cNationEngland;
  /*m_Leader = NameOfLeader;
  m_Gold = 1000;
  m_TaxRate = 0;
  Randomize;*/
  TGoodType gt = Pascal::TGoodType::Low();
  while (gt<Pascal::TGoodType::High())
  {
    m_Boycotted[gt] = false;//no boycotts at beginning
    m_Prices[gt] = Randomizer::get().random(cGoodPrices[gt].start_min, cGoodPrices[gt].start_max);
    gt = Pascal::TGoodType::Succ(gt);
  }
  m_Boycotted[Pascal::TGoodType::High()] = false;
  m_VillagesBurned = 0;
  //no founding fathers at beginning
  TFoundingFathers ff = Pascal::TFoundingFathers::Low();
  while (ff<Pascal::TFoundingFathers::High())
  {
    m_FoundingFathers[ff] = false;
    ff = Pascal::TFoundingFathers::Succ(ff);
  }//while
  m_FoundingFathers[Pascal::TFoundingFathers::High()] = false;
  //next founding father
  m_NextFoundingFather = ffNone; //none selected yet
  m_LibertyBells = 0; //new nation has no liberty bells yet
  //initialize diplomatic state as undefinded for all European nations
  int i;
  for (i = cMinEuropean; i <= cMaxEuropean; ++i)
  {
    m_Diplomatic[i] = dsUndefined;
  }//for
  //set spawnpoint to (-1;-1) to indicate that it is not set yet
  m_SpawnX = -1;
  m_SpawnY = -1;
} //construc

TEuropeanNation::~TEuropeanNation()
{
  //inherited Destroy;
}

bool TEuropeanNation::IsIndian() const
{
  return false;
}

bool TEuropeanNation::IsEuropean() const
{
  return true;
}

const std::string& TEuropeanNation::GetLeaderName() const
{
  return m_Leader;
}

void TEuropeanNation::ChangeLeaderName(const std::string& NameOfLeader)
{
  if (!NameOfLeader.empty())
    m_Leader = NameOfLeader;
}//proc

void TEuropeanNation::SetSpawnpoint(const LongInt x, const LongInt y)
{
  if ((x>=0) && (y>=0))
  {
    //location seems to be valid, set it
    m_SpawnX = x;
    m_SpawnY = y;
  }
  else
  {
    //invalid location given, set to (-1; -1)
    m_SpawnX = -1;
    m_SpawnY = -1;
  }
} //proc

LongInt TEuropeanNation::GetSpawnpointX() const
{
  return m_SpawnX;
}//func

LongInt TEuropeanNation::GetSpawnpointY() const
{
  return m_SpawnY;
}//func

bool TEuropeanNation::HasValidSpawnpoint() const
{
  return ((m_SpawnX>=0) && (m_SpawnY>=0));
}//func

Byte TEuropeanNation::GetTaxRate() const
{
  return m_TaxRate;
}//func

void TEuropeanNation::IncreaseTax(const Byte AddedPercentage)
{
  if ((AddedPercentage<100) && (m_TaxRate+AddedPercentage<100))
    m_TaxRate += AddedPercentage;
}//proc

void TEuropeanNation::ChangeTaxRate(const Byte NewPercentage)
{
  if (NewPercentage<100)
    m_TaxRate = NewPercentage;
} //proc

bool TEuropeanNation::IsBoycotted(const TGoodType AGood) const
{
  return m_Boycotted[AGood];
}//func

void TEuropeanNation::DoBoycott(const TGoodType AGood)
{
  if ((AGood != gtLibertyBell) && (AGood != gtHammer))
    m_Boycotted[AGood] = true;
} //proc

void TEuropeanNation::UndoBoycott(const TGoodType AGood)
{
  m_Boycotted[AGood] = false;
} //proc

void TEuropeanNation::UndoAllBoycotts()
{
  TGoodType gt = Pascal::TGoodType::Low();
  while (gt < Pascal::TGoodType::High())
  {
    m_Boycotted[gt] = false;
    gt = Pascal::TGoodType::Succ(gt);
  }
  m_Boycotted[Pascal::TGoodType::High()] = false;
} //proc

LongInt TEuropeanNation::GetGold() const
{
  return m_Gold;
}//func

void TEuropeanNation::DecreaseGold(const LongInt amount)
{
  if (m_Gold>amount)
    m_Gold = m_Gold - amount;
  else
    m_Gold = 0;
} //proc

void TEuropeanNation::IncreaseGold(const LongInt amount)
{
  if (amount>=0)
    m_Gold = m_Gold + amount;
  else
    DecreaseGold(-amount);
} //proc

Byte TEuropeanNation::GetPrice(const TGoodType AGood, const bool low) const
{
  if (low)
    return m_Prices[AGood];
  else
    return m_Prices[AGood]+cGoodPrices[AGood].diff;
} //func

void TEuropeanNation::AdvancePrice(const TGoodType AGood)
{
  if (m_Prices[AGood]+cGoodPrices[AGood].diff<cGoodPrices[AGood].max)
  {
    m_Prices[AGood] = m_Prices[AGood] + 1;
    //TODO: should display message to player
  } //if
} //proc

void TEuropeanNation::DropPrice(const TGoodType AGood)
{
  if (m_Prices[AGood] > cGoodPrices[AGood].min)
  {
    m_Prices[AGood] = m_Prices[AGood] - 1;
    //TODO: should display a message to the player
  } //if
} //proc

void TEuropeanNation::ChangePrice(const TGoodType AGood, const Byte NewPrice)
{
  if (NewPrice<cGoodPrices[AGood].min)
    m_Prices[AGood] = cGoodPrices[AGood].min;
  else if (NewPrice+cGoodPrices[AGood].diff>cGoodPrices[AGood].max)
    m_Prices[AGood] = cGoodPrices[AGood].max;
  else m_Prices[AGood] = NewPrice;
} //proc

//tries to "buy" goods, but only does the money related stuff; returns true on success
bool TEuropeanNation::BuyGood(const TGoodType AGood, const Byte num)
{
  if (IsBoycotted(AGood) || (m_Gold<num*GetPrice(AGood, false)))
    return false;
  else
  {
    m_Gold = m_Gold - GetPrice(AGood, false) * num;
    //TODO: should display message about cost to the player -> GUI
    return true;
  } //else
} //func

//tries to "sell" goods, but only does the money related stuff; returns true on success
bool TEuropeanNation::SellGood(const TGoodType AGood, const Byte num)
{
  if (IsBoycotted(AGood))
    return false;
  else
  {
    int tax_amount = (GetPrice(AGood, true) * num * GetTaxRate()) / 100;
    m_Gold = m_Gold + GetPrice(AGood, true) * num - tax_amount;
    //TODO: should display message about gain & tax to the player -> GUI
    return true;
  } //else
} //func

LongInt TEuropeanNation::GetVillagesBurned() const
{
  return m_VillagesBurned;
} //func

void TEuropeanNation::SetVillagesBurned(const LongInt villages)
{
  if (villages>=0)
    m_VillagesBurned = villages;
} //proc

bool TEuropeanNation::HasFoundingFather(const TFoundingFathers ff) const
{
  return m_FoundingFathers[ff];
} //func

void TEuropeanNation::SetFoundingFather(const TFoundingFathers ff, const bool present)
{
  //none is not an acceptable value and will always be false
  if (ff==ffNone)
  {
    m_FoundingFathers[ffNone] = false;
    return;
  } //if none
  //Will a new founding father join, and is this the same one as the one that is
  // expected to be the next?
  if (present and (ff==m_NextFoundingFather) and (m_NextFoundingFather!=ffNone) and
      !m_FoundingFathers[ff])
  {
    //Then set the new ff and adjust the amount of bells accordingly.
    m_FoundingFathers[ff] = true;
    if (m_LibertyBells >= GetRequiredLibertyBells(GetPresentFoundingFathers()))
      m_LibertyBells = m_LibertyBells-GetRequiredLibertyBells(GetPresentFoundingFathers());
    else
      m_LibertyBells = 0;
    //...and set the next ff to none. (Player or AI should choose a new one.)
    m_NextFoundingFather = ffNone;
  }
  //otherwise just set the value
  else
    m_FoundingFathers[ff] = present;
} //proc

Byte TEuropeanNation::GetPresentFoundingFathers() const
{
  Byte Result = 0;
  int i;
  for (i = Pascal::TFoundingFathers::Ord(Pascal::TFoundingFathers::Low()); i<= Pascal::TFoundingFathers::Ord(Pascal::TFoundingFathers::High()); ++i)
    if (m_FoundingFathers[static_cast<TFoundingFathers>(i)]) Result = Result+1;
  return Result;
} //func

Word TEuropeanNation::GetLibertyBells() const
{
  return m_LibertyBells;
} //func

void TEuropeanNation::AddLibertyBells(const Word lb)
{
  m_LibertyBells = m_LibertyBells+lb;
} //func

/* void TEuropeanNation::SetLibertyBells(const Word total_lb)
{
  m_LibertyBells = total_lb;
} //proc */

TFoundingFathers TEuropeanNation::GetNextFoundingFather() const
{
  return m_NextFoundingFather;
} //func

void TEuropeanNation::SetNextFoundingFather(const TFoundingFathers ff)
{
  if (ff != ffNone)
  {
    if (!HasFoundingFather(ff))
      m_NextFoundingFather = ff;
  }
  else m_NextFoundingFather = ffNone;
} //proc

TFoundingFatherArray TEuropeanNation::GetFoundingFatherSelection() const
{
  TFoundingFatherArray Result;
  int i;
  for (i = 0; i <= 4; ++i)
    Result[i] = ffNone;
  int next_index = 0;
  //trade
  i = Pascal::TFoundingFathers::Ord(ffSmith) + Randomizer::get().random(0, 4);
  if (!HasFoundingFather(static_cast<TFoundingFathers>(i)))
  {
    Result[0] = TFoundingFathers(i);
    next_index = 1;
  }
  else
  {
    for (i = Pascal::TFoundingFathers::Ord(ffSmith); i<= Pascal::TFoundingFathers::Ord(ffDeWitt); ++i)
      if (!HasFoundingFather(static_cast<TFoundingFathers>(i)))
      {
        Result[0] = static_cast<TFoundingFathers>(i);
        next_index = 1;
        break;
      } //if
  } //else
  //exploration
  i = Pascal::TFoundingFathers::Ord(ffCoronado) + Randomizer::get().random(0, 4);
  if (!HasFoundingFather(static_cast<TFoundingFathers>(i)))
  {
    Result[next_index] = static_cast<TFoundingFathers>(i);
    next_index = next_index + 1;
  }
  else
  {
    for (i = Pascal::TFoundingFathers::Ord(ffCoronado); i<= Pascal::TFoundingFathers::Ord(ffDeSoto); ++i)
      if (!HasFoundingFather(static_cast<TFoundingFathers>(i)))
      {
        Result[next_index] = static_cast<TFoundingFathers>(i);
        next_index = next_index + 1;
        break;
      } //if
  }//else
  //military
  i = Pascal::TFoundingFathers::Ord(ffCortes) + Randomizer::get().random(0, 4);
  if (!HasFoundingFather(static_cast<TFoundingFathers>(i)))
  {
    Result[next_index] = static_cast<TFoundingFathers>(i);
    next_index = next_index+1;
  }
  else
  {
    for (i = Pascal::TFoundingFathers::Ord(ffCortes); i<= Pascal::TFoundingFathers::Ord(ffWashington); ++i)
      if (!HasFoundingFather(static_cast<TFoundingFathers>(i)))
      {
        Result[next_index] = static_cast<TFoundingFathers>(i);
        next_index = next_index+1;
        break;
      } //if
  }//else
  //political
  i = Pascal::TFoundingFathers::Ord(ffBolivar) + Randomizer::get().random(0, 4);
  if (!HasFoundingFather(static_cast<TFoundingFathers>(i)))
  {
    Result[next_index] = static_cast<TFoundingFathers>(i);
    next_index = next_index+1;
  }
  else
  {
    for (i = Pascal::TFoundingFathers::Ord(ffBolivar); i<= Pascal::TFoundingFathers::Ord(ffPocahontas); ++i)
      if (!HasFoundingFather(TFoundingFathers(i)))
      {
        Result[next_index] = static_cast<TFoundingFathers>(i);
        next_index = next_index+1;
        break;
      } //if
  } //else
  //religious
  i = Pascal::TFoundingFathers::Ord(ffBrebeuf) + Randomizer::get().random(0, 4);
  if (!HasFoundingFather(static_cast<TFoundingFathers>(i)))
  {
    Result[next_index] = static_cast<TFoundingFathers>(i);
    next_index = next_index+1;
  }
  else
  {
    for (i = Pascal::TFoundingFathers::Ord(ffBrebeuf); i <= Pascal::TFoundingFathers::Ord(ffSepulveda); ++i)
      if (!HasFoundingFather(static_cast<TFoundingFathers>(i)))
      {
        Result[next_index] = static_cast<TFoundingFathers>(i);
        next_index = next_index+1;
        break;
      } //if
  } //else
  return Result;
} //func

TDiplomaticStatus TEuropeanNation::GetDiplomatic(const LongInt other_nation) const
{
  if ((cMinEuropean <= other_nation) && (other_nation <= cMaxEuropean))
    return m_Diplomatic[other_nation];
  else return dsUndefined;
} //func

void TEuropeanNation::SetDiplomatic(const LongInt other_nation, const TDiplomaticStatus new_status)
{
  if ((cMinEuropean <= other_nation) && (other_nation <= cMaxEuropean))
  {
    if (other_nation!=GetCount()) m_Diplomatic[other_nation] = new_status;
    else m_Diplomatic[other_nation] = dsUndefined;
  }//if
} //proc

bool TEuropeanNation::SaveToStream(std::ofstream& fs) const
{
  if (!fs.good()) return false;
  fs.write(reinterpret_cast<const char*>(&m_count), sizeof(LongInt));
  LongInt i = m_NameStr.size();
  fs.write(reinterpret_cast<const char*>(&i), sizeof(LongInt));
  fs.write(m_NameStr.c_str(), m_NameStr.size());
  i = m_Leader.size();
  fs.write(reinterpret_cast<const char*>(&i), sizeof(LongInt));
  fs.write(m_Leader.c_str(), m_Leader.size());
  fs.write(reinterpret_cast<const char*>(&m_Gold), sizeof(LongInt));
  fs.write(reinterpret_cast<const char*>(&m_TaxRate), sizeof(Byte));
  for (i = Pascal::TGoodType::Ord(Pascal::TGoodType::Low()); i <= Pascal::TGoodType::Ord(Pascal::TGoodType::High()); ++i)
  {
    fs.write(reinterpret_cast<const char*>(&m_Boycotted[TGoodType(i)]), sizeof(bool));
    fs.write(reinterpret_cast<const char*>(&m_Prices[TGoodType(i)]), sizeof(Byte));
  } //for
  //burned villages
  fs.write(reinterpret_cast<const char*>(&m_VillagesBurned), sizeof(LongInt));
  //founding fathers
  for (i = Pascal::TFoundingFathers::Ord(Pascal::TFoundingFathers::Low()); i<= Pascal::TFoundingFathers::Ord(Pascal::TFoundingFathers::High()); ++i)
  {
    fs.write(reinterpret_cast<const char*>(&m_FoundingFathers[static_cast<TFoundingFathers>(i)]),
             sizeof(bool));
  }//for
  //next founding father
  fs.write(reinterpret_cast<const char*>(&m_NextFoundingFather), sizeof(TFoundingFathers));
  //liberty bells produced
  fs.write(reinterpret_cast<const char*>(&m_LibertyBells), sizeof(Word));
  //diplomatic status
  for (i = cMinEuropean; i<= cMaxEuropean; ++i)
    fs.write(reinterpret_cast<const char*>(&m_Diplomatic[i]), sizeof(TDiplomaticStatus));
  //spawnpoint's coordinates
  fs.write(reinterpret_cast<const char*>(&m_SpawnX), sizeof(LongInt));
  fs.write(reinterpret_cast<const char*>(&m_SpawnY), sizeof(LongInt));
  return fs.good();
} //func

bool TEuropeanNation::LoadFromStream(std::ifstream& fs)
{
  if (!fs.good()) return false;
  LongInt i = 0;
  fs.read(reinterpret_cast<char*>(&i), sizeof(LongInt));
  if (!fs.good())
  {
    std::cout << "TEuropeanNation.LoadFromStream: Error while reading count.\n";
    return false;
  }//if
  ChangeCount(i);
  fs.read(reinterpret_cast<char*>(&i), sizeof(LongInt));
  if ((i<=0) || (i>255))
  {
    //string to short or to long
    std::cout << "TEuropeanNation.LoadFromStream: Error: name string does not meet "
              << "the length requirements.\n";
    return false;
  } //if
  char buffer[256];
  memset(buffer, '\0', 256);
  fs.read(buffer, i);
  if (!fs.good())
  {
    std::cout << "TEuropeanNation.LoadFromStream: Error while reading name.\n";
    return false;
  } //if
  ChangeName(std::string(buffer));
  //--- European part starts here ----
  fs.read(reinterpret_cast<char*>(&i), sizeof(LongInt));
  if ((i<=0) || (i>255))
  {
    //string to short or to long
    std::cout << "TEuropeanNation.LoadFromStream: Error: leader name string does "
              << "not meet the length requirements.\n";
    return false;
  } //if
  memset(buffer, '\0', 256);
  fs.read(buffer, i);
  if (!fs.good())
  {
    std::cout << "TEuropeanNation.LoadFromStream: Error while reading leader's name.\n";
    return false;
  }//if
  ChangeLeaderName(std::string(buffer));
  fs.read(reinterpret_cast<char*>(&i), sizeof(LongInt));
  if ((!fs.good()) or (i<0))
  {
    std::cout << "TEuropeanNation.LoadFromStream: Error while reading gold amount.\n";
    return false;
  }//if
  m_Gold = i;
  //tax rate
  Byte tr = 255;
  fs.read(reinterpret_cast<char*>(&tr), sizeof(Byte));
  if ((!fs.good()) || (tr>100))
  {
    std::cout << "TEuropeanNation.LoadFromStream: Error while reading tax rate.\n";
    return false;
  } //if
  ChangeTaxRate(tr);
  for (i = Pascal::TGoodType::Ord(Pascal::TGoodType::Low()); i <= Pascal::TGoodType::Ord(Pascal::TGoodType::High()); ++i)
  {
    //boycott status
    bool Boycott = false;
    fs.read(reinterpret_cast<char*>(&Boycott), sizeof(bool));
    m_Boycotted[static_cast<TGoodType>(i)] = Boycott;
    //price of good
    fs.read(reinterpret_cast<char*>(&tr), sizeof(Byte));
    ChangePrice(static_cast<TGoodType>(i), tr);
  }//for
  //burned villages
  fs.read(reinterpret_cast<char*>(&i), sizeof(LongInt));
  if ((!fs.good()) or (i<0))
  {
    std::cout << "TEuropeanNation.LoadFromStream: Error while reading number of "
              << "villages burned.\n";
    return false;
  }//if
  SetVillagesBurned(i);
  //founding fathers
  for (i = Pascal::TFoundingFathers::Ord(Pascal::TFoundingFathers::Low()); i <= Pascal::TFoundingFathers::Ord(Pascal::TFoundingFathers::High()); ++i)
  {
    bool present = false;
    fs.read(reinterpret_cast<char*>(&present), sizeof(bool));
    m_FoundingFathers[static_cast<TFoundingFathers>(i)] = present;
  }//for
  m_FoundingFathers[ffNone] = false;
  //next founding father
  fs.read(reinterpret_cast<char*>(&m_NextFoundingFather), sizeof(TFoundingFathers));
  //liberty bells produced
  fs.read(reinterpret_cast<char*>(&m_LibertyBells), sizeof(Word));
  if (!fs.good())
  {
    std::cout << "TEuropeanNation.LoadFromStream: Error while reading information "
              << "about founding fathers.\n";
    return false;
  }//if
  //diplomatic status
  TDiplomaticStatus diplomatic = dsUndefined;
  for (i = cMinEuropean; i <= cMaxEuropean; ++i)
  {
    fs.read(reinterpret_cast<char*>(&diplomatic), sizeof(TDiplomaticStatus));
    SetDiplomatic(i, diplomatic);
  }//for
  if (!fs.good())
  {
    std::cout << "TEuropeanNation.LoadFromStream: Error while reading diplomatic "
              << "status.\n";
    return false;
  }//if
  //spawnpoint's coordinates
  i = -1;
  LongInt j = -1;
  fs.read(reinterpret_cast<char*>(&i), sizeof(LongInt));
  fs.read(reinterpret_cast<char*>(&j), sizeof(LongInt));
  SetSpawnpoint(i, j);
  if (!fs.good())
  {
    std::cout << "TEuropeanNation.LoadFromStream: Error while reading spawnpoint.\n";
    SetSpawnpoint(-1, -1); //reset spawnpoint
    return false;
  } //if
  return true;
} //func
