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

#ifndef TRIBE_HPP
#define TRIBE_HPP

#include <array>

#include "PascalTypes.hpp"
#include "Settlement.hpp"
#include "Nation.hpp"
#include "Units.hpp"


/* Constant array that holds the locations of Indian tribes that are present
  in America at the start of a new game.

  remarks/ to do:
      still needs to be extended
*/
struct tla
{
  LongInt Nation;
  Byte x, y;
}; //struct
const std::array<tla, 13> cTribeLocationsAmerica
    ={{
      {cNationAztec, 11, 23},
      {cNationAztec, 16, 26},
      {cNationAztec, 23, 27},
      {cNationAztec, 25, 33},

      {cNationInca, 26, 42},
      {cNationInca, 34, 59},
      {cNationInca, 34, 65},
      {cNationInca, 35, 51},
      {cNationInca, 36, 55},

      {cNationCherokee, 20, 20},
      {cNationCherokee, 21, 17},
      {cNationCherokee, 24, 19},
      {cNationCherokee, 27, 22}
    }};


/* ********
   **** TTribe class
   ****
   **** purpose: represents a Indian settlement within the game.
   ****          Specialised version of TSettlement for Indians.
   *******
*/
class TTribe: public TSettlement
{
  public:
    /* constructor

       parameters:
           x, y     - position of the tribe
           ANation  - integer identifier of the Indian nation
           KnownFor - the skill/job the Indians of that tribe can teach to
                      unskilled colonists
    */
    TTribe(const int X, const int Y, const LongInt ANation, const TUnitType KnownFor);

    /* teaches the given unit the special skill of that tribe

       parameters:
           AUnit - the unit that shall learn the skill

       remarks:
           Only unskilled units, i.e. servants or colonists, can learn a skill
           from a tribe. Every European nation can only learn once from the
           same tribe.
    */
    void Teach(TUnit& AUnit);

    /* returns true, if this tribe represents a capital of the Indians */
    bool IsCapital() const;

    /* returns an integer representing this tribe's attitude towards the
       given European Nation. Lower values mean friendlier attitude.

       parameters:
           NumNation - ID of the European Nation
    */
    Byte GetAttitudeLevel(const LongInt NumNation) const;

    /* sets an integer representing this tribe's attitude towards the
       given European Nation. Lower values mean friendlier attitude.

       parameters:
           NumNation - ID of the European Nation
           new_level - the new value for the attitude level
    */
    void SetAttitudeLevel(const LongInt NumNation, const Byte new_level);

    /* tries to save the tribe to a stream and returns true in case of success

       parameters:
           fs - the file stream the tribe has to be saved to

       remarks:
           The file stream already has to be opened and has to be ready for
           writing.
    */
    bool SaveToStream(std::ofstream& fs) const override;

    /* tries to load a tribe from a stream and returns true in case of success

       parameters:
           fs - the file stream the tribe has to be loaded from

       remarks:
           The file stream already has to be opened and has to be ready for
           reading.
    */
    bool LoadFromStream(std::ifstream& fs) override;
  private:
      TUnitType m_KnownFor;
      std::array<bool, cMaxEuropean+1> m_HasTought;
      //capital flag
      bool m_Capital;
      //attitude level - basically used for internal calculations
      std::array<Byte, cMaxEuropean+1> m_AttitudeLevel;
};//class

#endif // TRIBE_HPP
