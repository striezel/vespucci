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

#ifndef PASCALFUNCTIONS_GOODS_HPP
#define PASCALFUNCTIONS_GOODS_HPP

#include "Goods.hpp"

//define equivalents to some built-in Pascal functions
namespace Pascal
{
  namespace TGoodType
  {
    static ::TGoodType High()
    {
      return gtCross;
    }

    static ::TGoodType Low()
    {
      return gtFood;
    }

    static ::TGoodType Succ(const ::TGoodType gt)
    {
      return static_cast< ::TGoodType>(static_cast<int>(gt)+1);
    }

    /*
    static ::TGoodType Pred(const ::TGoodType gt)
    {
      return static_cast< ::TGoodType>(static_cast<int>(gt)-1);
    }
    */

    static int Ord(const ::TGoodType gt)
    {
      return static_cast<int>(gt);
    }
  } //namespace TGoodType
} //namespace Pascal

#endif // PASCALFUNCTIONS_GOODS_HPP
