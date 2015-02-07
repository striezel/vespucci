/* ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2010  Thoronador

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

Randomizer::Randomizer()
: mt_rng(std::mt19937(time(NULL)))
{
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
  if (a>b)
    return random(b, a);
  std::uniform_int_distribution<Byte> intDistrib(a, b);
  return intDistrib(mt_rng);
}

LongInt Randomizer::random(const LongInt a, const LongInt b)
{
  if (a==b) return a;
  if (a>b)
    return random(b, a);
  std::uniform_int_distribution<LongInt> intDistrib(a, b);
  return intDistrib(mt_rng);
}

double Randomizer::random()
{
  std::uniform_real_distribution<double> floatingPointDistrib(0.0, 1.0);
  return floatingPointDistrib(mt_rng);
}

int Randomizer::random(const unsigned int n)
{
  if (n<=1)
    return 0;
  std::uniform_int_distribution<unsigned int> intDistrib(0, n-1);
  return intDistrib(mt_rng);
}
