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

#ifndef RANDOMIZER_HPP
#define RANDOMIZER_HPP

#include "PascalTypes.hpp"
#include <random>

class Randomizer
{
  public:
    /* virtual destructor */
    virtual ~Randomizer();

    /* Singleton access function */
    static Randomizer& get();

    /* generates a random number between a and b (including a and b) */
    Byte random(const Byte a, const Byte b);

    /* generates a random number between a and b (including a and b) */
    LongInt random(const LongInt a, const LongInt b);

    /* generates a floating point random number in [0;1) */
    double random();

    /* generates a random integer number in [0;n), n>0 */
    int random(const unsigned int n);
  private:
    std::mt19937 mt_rng;

    //constructor
    Randomizer();
    //empty copy constructor
    Randomizer(const Randomizer& op) {}
};//class

#endif // RANDOMIZER_HPP

