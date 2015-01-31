/* **************************************************************************

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

#include "IndianNation.hpp"
#include <fstream>

TIndianNation::TIndianNation(const LongInt num, const std::string& NameStr)
: TNation(num, NameStr),
  m_Spawned(false)
{
  // inherited Create(num, NameStr);
  //check number and pick a default in case of invalidity
  if ((m_count<cMinIndian) or (m_count>cMaxIndian))
    m_count = cNationArawak;
  int i;
  for (i = cMinEuropean; i<= cMaxEuropean; ++i)
  {
    m_Contact[i] = false;
    m_Attitude[i] = iaPleased;
  } //for
  switch (m_count)
  {
    case cNationArawak:
    case cNationCherokee:
    case cNationIroquois:
         m_TechLevel = tlAgricultural;
         break;
    case cNationAztec:
         m_TechLevel = tlDeveloped;
         break;
    case cNationInca:
         m_TechLevel = tlCivilised;
         break;
    default:
         m_TechLevel = tlNomadic; //Tupi, Sioux, and Apache
  } //swi
  m_Spawned = false;
}//construc

bool TIndianNation::IsIndian() const
{
  return true;
}

bool TIndianNation::IsEuropean() const
{
  return false;
}

TTechLevel TIndianNation::GetTechLevel() const
{
  return m_TechLevel;
}//func

bool TIndianNation::HasContactWith(const LongInt num_EuroNat) const
{
  if ((num_EuroNat >= cMinEuropean) && (num_EuroNat <= cMaxEuropean))
    return m_Contact[num_EuroNat];
  else return false;
}//func

void TIndianNation::SetContactWith(const LongInt num_EuroNat, const bool newContact)
{
  if ((num_EuroNat >= cMinEuropean) && (num_EuroNat <= cMaxEuropean))
    m_Contact[num_EuroNat] = newContact;
}//proc

TIndianAttitude TIndianNation::GetAttitude(const LongInt num_EuroNat) const
{
  if ((cMinEuropean <= num_EuroNat) && (num_EuroNat <= cMaxEuropean))
    return m_Attitude[num_EuroNat];
  else return iaPleased;
}//func

void TIndianNation::SetAttitude(const LongInt num_EuroNat, const TIndianAttitude newAttitude)
{
  if ((cMinEuropean <= num_EuroNat) && (num_EuroNat <= cMaxEuropean))
    m_Attitude[num_EuroNat] = newAttitude;
}//proc

bool TIndianNation::GetSpawnStatus() const
{
  return m_Spawned;
}//func

void TIndianNation::SetSpawnStatus(const bool new_stat)
{
  m_Spawned = new_stat;
}//proc

bool TIndianNation::SaveToStream(std::ofstream& fs) const
{
  //try to save inherited data from TNation
  const bool Result = TNation::SaveToStream(fs);
  //If that failed, exit.
  if (!Result) return false;
  //save bits from Indian nation
  // -- tech level
  fs.write(reinterpret_cast<const char*>(&m_TechLevel), sizeof(TTechLevel));
  // -- contacts and attitudes
  int i;
  for (i = cMinEuropean; i<= cMaxEuropean; ++i)
  {
    fs.write(reinterpret_cast<const char*>(&m_Contact[i]), sizeof(bool));
    fs.write(reinterpret_cast<const char*>(&m_Attitude[i]), sizeof(TIndianAttitude));
  } //for
  // -- spawning status
  fs.write(reinterpret_cast<const char*>(&m_Spawned), sizeof(bool));
  return fs.good();
}//func

bool TIndianNation::LoadFromStream(std::ifstream& fs)
{
  //try to load inherited data from TNation
  const bool Result = TNation::LoadFromStream(fs);
  //If that failed, exit.
  if (!Result) return false;
  //load bits from Indian nation
  // -- tech level
  fs.read(reinterpret_cast<char*>(&m_TechLevel), sizeof(TTechLevel));
  // -- contacts and attitudes
  int i;
  for (i = cMinEuropean; i<= cMaxEuropean; ++i)
  {
    fs.read(reinterpret_cast<char*>(&m_Contact[i]), sizeof(bool));
    fs.read(reinterpret_cast<char*>(&m_Attitude[i]), sizeof(TIndianAttitude));
  }//for
  // -- spawning status
  fs.read(reinterpret_cast<char*>(&m_Spawned), sizeof(bool));
  return fs.good();
}//func
