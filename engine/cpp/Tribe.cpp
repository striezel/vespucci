/* **************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2009, 2010, 2011  Thoronador

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

#include "Tribe.hpp"
#include <iostream>
#include "PascalFunctions_TUnitType.hpp"

// **** TTribe functions ****

TTribe::TTribe(const int X, const int Y, const LongInt ANation, const TUnitType KnownFor)
: TSettlement(X, Y, ANation)
{
  //sets Nation and position
  //inherited Create(X, Y, ANation);
  LongInt i;
  for (i = cMinEuropean; i <= cMaxEuropean; ++i)
    m_HasTought[i] = false;
  //set the unit type the tribe is known for
  m_KnownFor = KnownFor;
}//construc

void TTribe::Teach(TUnit& AUnit)
{
  //only servants or free colonists can learn something
  if ((AUnit.GetType()!=utServant) and (AUnit.GetType()!=utColonist))
  {
    if ((Pascal::TUnitType::Ord(utFarmer) <= Pascal::TUnitType::Ord(AUnit.GetType()))
        and (Pascal::TUnitType::Ord(AUnit.GetType()) <= Pascal::TUnitType::Ord(utPioneer)))
    {
      //Show message: "It's a pleasure to see a skilled <whatever>..."
      // Still ToDo();
    }
    else
    {
      //Show message, informing player about inappropriate unit type
      // Still ToDo();
    }//else
  }
  //only European Units can learn from Indians
  else if ((cMinEuropean <= AUnit.GetNation()) && (AUnit.GetNation() <= cMaxEuropean))
  {
    //check, if Indians already did teach that nation's units a new skill
    if (m_HasTought[AUnit.GetNation()])
    {
      //Show message, something like "Ihr habt schon etwas von uns gelernt"
      // Still ToDo();
    }//if
    else
    {
      //actually teach the unit something
      AUnit.ChangeType(m_KnownFor);
      m_HasTought[AUnit.GetNation()] = true;
    }//else
  }//if
}//proc

bool TTribe::IsCapital() const
{
  return m_Capital;
}//func

Byte TTribe::GetAttitudeLevel(const LongInt NumNation) const
{
  if ((cMinEuropean <= NumNation) && (NumNation <= cMaxEuropean))
    return m_AttitudeLevel[NumNation];
  else return 0; //assume best
}//func

void TTribe::SetAttitudeLevel(const LongInt NumNation, const Byte new_level)
{
  if ((cMinEuropean <= NumNation) && (NumNation <= cMaxEuropean))
    m_AttitudeLevel[NumNation] = new_level;
}//proc

bool TTribe::SaveToStream(std::ofstream& fs) const
{
  //write inherited data
  bool Result = TSettlement::SaveToStream(fs);
  if (!Result)
  {
    std::cout << "TTribe.SaveToStream: Error: could not write inherited data.\n";
    return Result;
  }//if
  //write TTribe stuff
  //write special unit type
  fs.write(reinterpret_cast<const char*>(&m_KnownFor), sizeof(TUnitType));
  //write teaching status
  int i;
  for (i = cMinEuropean; i<=cMaxEuropean; ++i)
    fs.write(reinterpret_cast<const char*>(&m_HasTought[i]), sizeof(bool));
  //write capital status
  fs.write(reinterpret_cast<const char*>(&m_Capital), sizeof(bool));
  //write attitude levels
  for (i = cMinEuropean; i <= cMaxEuropean; ++i)
    fs.write(reinterpret_cast<const char*>(&m_AttitudeLevel[i]), sizeof(Byte));
  return (Result and fs.good());
}//func

bool TTribe::LoadFromStream(std::ifstream& fs)
{
  //read inherited data
  bool Result = TSettlement::LoadFromStream(fs);
  if (!Result)
  {
    std::cout << "TTribe.LoadFromStream: Error: could not read inherited data.\n";
    return false;
  }//if
  //check nation index
  if ((GetNation()<cMinIndian) || (GetNation()>cMaxIndian))
  {
    std::cout << "TTribe.LoadFromStream: Error: got invalid nation index.\n";
    return false;
  }//if
  //read TTribe stuff
  //read special unit type
  fs.read(reinterpret_cast<char*>(&m_KnownFor), sizeof(TUnitType));
  //write teaching status
  LongInt i;
  for (i = cMinEuropean; i <= cMaxEuropean; ++i)
    fs.read(reinterpret_cast<char*>(&m_HasTought[i]), sizeof(bool));
  //read capital status
  fs.read(reinterpret_cast<char*>(&m_Capital), sizeof(bool));
  //read attitude levels
  for (i = cMinEuropean; i <= cMaxEuropean; ++i)
    fs.read(reinterpret_cast<char*>(&m_AttitudeLevel[i]), sizeof(Byte));
  return fs.good();
}//func
