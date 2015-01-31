/* **************************************************************************

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

#ifndef FOUNDINGFATHERS_HPP
#define FOUNDINGFATHERS_HPP

#include <array>
#include "PascalTypes.hpp"

/* enumeration type for founding fathers */
 enum TFoundingFathers {//trade
                        ffSmith, ffFugger, ffMinuit, ffStuyvesant, ffDeWitt,
                        //exploration
                        ffCoronado, ffHudson, ffLaSalle, ffMagellan, ffDeSoto,
                        //military
                        ffCortes, ffDrake, ffJones, ffRevere, ffWashington,
                        //political
                        ffBolivar, ffFranklin, ffJefferson, ffPaine, ffPocahontas,
                        //religious
                        ffBrebeuf, ffBrewster, ffLasCasas, ffPenn, ffSepulveda,
                        //none
                        ffNone};

//array type
typedef std::array<TFoundingFathers, 5> TFoundingFatherArray;

/* enumeration type for area of founding fathers */
enum TFoundingFatherType {fftTrade, fftExploration, fftMilitary, fftPolitical,
                          fftReligious};
typedef TFoundingFatherType TFoundingType; //alias to ease coding a bit

/* returns the type/ area a founding father belongs to

   parameters:
       ff - enumeration value that identifies the founding father
*/
TFoundingType GetFoundingFatherType(const TFoundingFathers ff);

/* returns the amount of liberty bells that are required for the n-th founding
   father to join the congress

   parameters:
       n - number of the next founding father (1-based, that is)
*/
Word GetRequiredLibertyBells(const Byte n);

#endif // FOUNDINGFATHERS_HPP
