/* **************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2009, 2010, 2011  Dirk Stolle

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

#include "Settlement.hpp"
#include <iostream>
#include "Nation.hpp"

// **** TSettlement functions ****

TSettlement::TSettlement(const LongInt X, const LongInt Y, const LongInt ANation)
: m_Nation(ANation),
  PosX(1),
  PosY(1)
{
  //no settlements outside of range or at border row/column (index: 0) allowed
  if (X>0) PosX = X; else PosX = 1;
  if (Y>0) PosY = Y; else PosY = 1;
  m_Nation = ANation;
}

TSettlement::~TSettlement()
{
  //empty
}

LongInt TSettlement::GetNation() const
{
  return m_Nation;
}

void TSettlement::ChangeNation(const LongInt new_nation)
{
  if (new_nation >= 0)
    m_Nation = new_nation;
}

LongInt TSettlement::GetPosX() const
{
  return PosX;
}

LongInt TSettlement::GetPosY() const
{
  return PosY;
}

void TSettlement::SetPosition(const LongInt x, const LongInt y)
{
  if (x > 0)
    PosX = x;
  else PosX = 1;
  if (y > 0)
    PosY = y;
  else PosY = 1;
}

bool TSettlement::SaveToStream(std::ofstream& fs) const
{
  //write nation ID
  fs.write(reinterpret_cast<const char*>(&m_Nation), sizeof(LongInt));
  //write position
  fs.write(reinterpret_cast<const char*>(&PosX), sizeof(LongInt));
  fs.write(reinterpret_cast<const char*>(&PosY), sizeof(LongInt));
  return fs.good();
}

bool TSettlement::LoadFromStream(std::ifstream& fs)
{
  LongInt i, j;
  //read nation ID
  i = -1;
  fs.read(reinterpret_cast<char*>(&i), sizeof(LongInt));
  if ((!fs.good()) || (fs.gcount() != sizeof(LongInt)))
  {
    std::cout << "TSettlement.LoadFromStream: Error: could not read nation index.\n";
    return false;
  }

  if ((i < cMinNations) || (cMaxNations < i))
  {
    std::cout << "TSettlement.LoadFromStream: Error: invalid nation index.\n";
    return false;
  }
  ChangeNation(i);
  fs.read(reinterpret_cast<char*>(&i), sizeof(LongInt));
  fs.read(reinterpret_cast<char*>(&j), sizeof(LongInt));
  if (!fs.good())
  {
    std::cout << "TSettlement.LoadFromStream: Error: could not read position.\n";
    return false;
  }
  SetPosition(i, j);
  return ((i>=0) && (j>=0));
}
