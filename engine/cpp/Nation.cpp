/* ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2009, 2010  Dirk Stolle

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

#include "Nation.hpp"
#include <iostream>
#include <fstream>
#include <cstring>

//**** functions of TNation ****

TNation::TNation(const LongInt num, const std::string& NameStr)
: m_count(num),
  m_NameStr(NameStr)
{

}

TNation::~TNation()
{
  //empty
}

const std::string& TNation::GetName() const
{
  return m_NameStr;
}

void TNation::ChangeName(const std::string& new_name)
{
  if (!new_name.empty())
    m_NameStr = new_name;
}

LongInt TNation::GetCount() const
{
  return m_count;
}

void TNation::ChangeCount(const LongInt new_num)
{
  m_count = new_num;
}

bool TNation::SaveToStream(std::ofstream& fs) const
{
  LongInt i;
  fs.write(reinterpret_cast<const char*>(&m_count), sizeof(LongInt));
  i = m_NameStr.size();
  fs.write(reinterpret_cast<const char*>(&i), sizeof(LongInt));
  fs.write(m_NameStr.c_str(), m_NameStr.size());
  return fs.good();
}//func

bool TNation::LoadFromStream(std::ifstream& fs)
{
  LongInt i;

  i = 0;
  fs.read(reinterpret_cast<char*>(&i), sizeof(LongInt));
  if (!fs.good())
  {
    std::cout << "TNation.LoadFromStream: Error while reading count.\n";
    return false;
  }

  ChangeCount(i);
  fs.read(reinterpret_cast<char*>(&i), sizeof(LongInt));
  if ((i<=0) || (i>255))
  {
    //string to short or to long
    std::cout << "TNation.LoadFromStream: Error: name string does not meet the "
              << std::string("length requirements.\n");
    return false;
  }

  char buffer[i+1];
  memset(buffer, '\0', i+1);
  fs.read(buffer, i);
  if (!fs.good())
  {
    std::cout << "TNation.LoadFromStream: Error while reading name.\n";
    return false;
  }
  ChangeName(std::string(buffer));
  return true;
}//func
