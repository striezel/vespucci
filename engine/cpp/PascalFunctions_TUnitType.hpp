/* ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2015  Thoronador

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

#ifndef PASCALFUNCTIONS_TUNITTYPE_HPP
#define PASCALFUNCTIONS_TUNITTYPE_HPP

#include "Units.hpp" //for TUnitType

//define equivalents to some built-in Pascal functions
namespace Pascal
{
  namespace TUnitType
  {
    /*
    static ::TUnitType High()
    {
      return utBraveOnHorse;
    }

    static ::TUnitType Low()
    {
      return utCriminal;
    }

    static ::TUnitType Succ(const ::TUnitType ut)
    {
      return static_cast< ::TUnitType>(static_cast<int>(ut)+1);
    }

    static ::TUnitType Pred(const ::TUnitType ut)
    {
      return static_cast< ::TUnitType>(static_cast<int>(ut)-1);
    }
    */

    static int Ord(const ::TUnitType ut)
    {
      return static_cast<int>(ut);
    }
  } //namespace TUnitType
} //namespace Pascal


#endif // PASCALFUNCTIONS_TUNITTYPE_HPP
