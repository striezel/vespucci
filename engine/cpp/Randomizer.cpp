/* ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2010  Dirk Stolle

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

#include "Randomizer.hpp"
#include <ctime>
#include <cstdlib>

Randomizer::Randomizer()
{
  srand(time(NULL));
}

Randomizer::~Randomizer()
{
  //empty
}

Randomizer& Randomizer::get()
{
  static Randomizer Instance;
  return Instance;
}

Byte Randomizer::random(const Byte a, const Byte b)
{
  if (a==b) return a;
  const float res = rand()/static_cast<float>(RAND_MAX);
  if (a<b)
    return a + static_cast<Byte>((b-a+1) * res);
  //else ---> b<a
  return b + static_cast<Byte>((a-b+1) * res);
}

LongInt Randomizer::random(const LongInt a, const LongInt b)
{
  if (a==b) return a;
  const float res = rand()/static_cast<float>(RAND_MAX);
  if (a<b)
    return a + static_cast<int>((b-a+1) * res);
  //else ---> b<a
  return b + static_cast<int>((a-b+1) * res);
}
