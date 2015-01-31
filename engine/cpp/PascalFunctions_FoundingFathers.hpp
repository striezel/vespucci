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

#ifndef PASCALFUNCTIONS_FOUNDINGFATHERS_HPP
#define PASCALFUNCTIONS_FOUNDINGFATHERS_HPP

#include "FoundingFathers.hpp"

//define equivalents to some built-in Pascal functions
namespace Pascal
{
  namespace TFoundingFathers
  {
    static ::TFoundingFathers High()
    {
      return ffNone;
    }

    static ::TFoundingFathers Low()
    {
      return ffSmith;
    }

    static ::TFoundingFathers Succ(const ::TFoundingFathers ff)
    {
      return static_cast< ::TFoundingFathers>(static_cast<int>(ff)+1);
    }

    /*
    static ::TFoundingFathers Pred(const ::TFoundingFathers ff)
    {
      return static_cast< ::TFoundingFathers>(static_cast<int>(ff)-1);
    }
    */

    static int Ord(const ::TFoundingFathers ff)
    {
      return static_cast<int>(ff);
    }
  } //namespace TFoundingFathers
} //namespace Pascal

#endif // PASCALFUNCTIONS_FOUNDINGFATHERS_HPP
