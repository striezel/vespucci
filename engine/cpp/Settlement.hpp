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

#ifndef SETTLEMENT_HPP
#define SETTLEMENT_HPP

#include "PascalTypes.hpp"
#include <fstream>

/* ********
   **** TSettlement class
   ****
   **** purpose: represents a settlement within the game, i.e. a colony or a
   ****          tribe of the Indians. However, there are more specialised
   ****          classes for both, derived from TSettlement.
   *******
*/
class TSettlement
{
  public:
    /* constructor

       parameters:
           X, Y    - x- and y-compoment of settlement's position
           ANation - nation that founded the settlement
    */
    TSettlement(const LongInt X, const LongInt Y, const LongInt ANation);

    /* destructor */
    virtual ~TSettlement();

    /* returns the nation that owns this settlement */
    LongInt GetNation() const;

    /* changes the nation that owns the settlement

       parameters:
           new_nation - ID of the new nation
    */
    void ChangeNation(const LongInt new_nation);

    /* returns the x-component of settlement's position */
    LongInt GetPosX() const;

    /* returns the x-component of settlement's position */
    LongInt GetPosY() const;

    /* sets a new map position for the settlement

       parameters:
           x, y - x- and y-component of the new position

       remarks:
           Both values, x and y, have to be greater than zero. If they aren't,
           then the procedure behaves as if they had the value of 1.
    */
    void SetPosition(const LongInt x, const LongInt y);

    /* tries to save the settlement to a stream and returns true in case of
       success

       parameters:
           fs - the file stream the settlement has to be saved to

       remarks:
           The file stream already has to be openend and has to be ready for
           writing.
    */
    virtual bool SaveToStream(std::ofstream& fs) const;

    /* tries to load a settlement from a stream and returns true in case of
       success

       parameters:
           fs - the file stream the settlement has to be loaded from

       remarks:
           The file stream already has to be openend and has to be ready for
           reading.
    */
    virtual bool LoadFromStream(std::ifstream& fs);
  private:
    //index of the nations that owns the settlement
    LongInt m_Nation;
    //settlement's position
    LongInt PosX, PosY;
}; //class

#endif // SETTLEMENT_HPP
