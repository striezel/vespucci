/* **************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2010  Thoronador

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

#include "Helper.hpp"
#include <algorithm> //for std::min()
#include <sstream>

//helper functions
std::string IntToStr(const int i)
{
  std::ostringstream stream;
  stream << i;
  return stream.str();
} //func

Byte IntegerLength(const LongInt i)
{
  if (i < 0)
    return IntegerLength(-i)+1;
  else
  {
    if (i <= 9) return 1;
    if (i <= 99) return 2;
    if (i <= 999) return 3;
    if (i <= 9999) return 4;
    if (i <= 99999) return 5;
    if (i <= 999999) return 6;
    if (i <= 9999999) return 7;
    if (i <= 99999999) return 8;
    if (i <= 999999999) return 9;
    // ca. 2 billion is the maximum of singned 32bit int, so this will be the last case
    return 10;
  }//else
}//func

TShortStrArr ToShortStrArr(const std::string& s1, const std::string& s2)
{
  TShortStrArr result;
  result.push_back(s1);
  result.push_back(s2);
  return std::move(result);
}//func

TShortStrArr ToShortStrArr(const std::string& s1, const std::string& s2, const std::string& s3)
{
  TShortStrArr result;
  result.push_back(s1);
  result.push_back(s2);
  result.push_back(s3);
  return result;
}//func

TShortStrArr ToShortStrArr(const std::string& s1, const std::string& s2, const std::string& s3, const std::string& s4)
{
  TShortStrArr result;
  result.push_back(s1);
  result.push_back(s2);
  result.push_back(s3);
  result.push_back(s4);
  return result;
}//func

TShortStrArr ToShortStrArr(const std::string& s1, const std::string& s2, const std::string& s3, const std::string& s4, const std::string& s5)
{
  TShortStrArr result;
  result.push_back(s1);
  result.push_back(s2);
  result.push_back(s3);
  result.push_back(s4);
  result.push_back(s5);
  return result;
}//func

TShortStrArr ToShortStrArr(const std::string& s1, const TShortStrArr& arr)
{
  std::string::size_type i;
  TShortStrArr Result;
  Result.push_back(s1);
  for (i = 0; i<arr.size(); ++i)
    Result.push_back(arr[i]);
  return Result;
}//func

std::string SpaceString(const Byte len)
{
  return std::string(' ', len);
}

std::string Trim(const std::string& str1)
{
  std::string result(str1);

  while (!result.empty() and (result[0] <= ' '))
    result.erase(0, 1);

  while (!result.empty() and (result[result.size()-1] <= ' '))
    result.erase(result.size()-1, 1);

  return result;
}//func

std::string StretchTo59(const std::string& s1, const std::string& s2)
{
  const std::string Result = Trim(s1);
  const std::string trimmed_s2 = Trim(s2);
  if (Result.size() + trimmed_s2.size() < 59)
    return Result + SpaceString(59-Result.size()-trimmed_s2.size())+trimmed_s2;
  else
    return Result + ' ' + trimmed_s2;
}//func

std::string StretchTo60(const std::string& s1, const std::string& s2)
{
  const std::string Result = Trim(s1);
  const std::string trimmed_s2 = Trim(s2);
  if (Result.size()+trimmed_s2.size() < 60)
    return Result+SpaceString(60-Result.size()-trimmed_s2.size())+trimmed_s2;
  else
    return Result +' '+trimmed_s2;
}//func

int Min(const int a, const int b)
{
  return std::min(a, b);
}//func

double Min(const double a, const double b)
{
  return std::min(a, b);
}
