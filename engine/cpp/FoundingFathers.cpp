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

#include "FoundingFathers.hpp"

TFoundingType GetFoundingFatherType(const TFoundingFathers ff)
{
  switch (ff)
  {
    case ffSmith:
    case ffFugger:
    case ffMinuit:
    case ffStuyvesant:
    case ffDeWitt:
        return fftTrade;
    case ffCoronado:
    case ffHudson:
    case ffLaSalle:
    case ffMagellan:
    case ffDeSoto:
         return fftExploration;
    case ffCortes:
    case ffDrake:
    case ffJones:
    case ffRevere:
    case ffWashington:
         return fftMilitary;
    case ffBolivar:
    case ffFranklin:
    case ffJefferson:
    case ffPaine:
    case ffPocahontas:
         return fftPolitical;
    default:
         return fftReligious;
  } //switch
}//func

Word GetRequiredLibertyBells(const Byte n)
{
  if (n>0) return 80*n-50; //usual formula
  else return 0; // prevents negative result in case of invalid parameter
}
