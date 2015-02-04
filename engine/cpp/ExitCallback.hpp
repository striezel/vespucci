/* **************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2011, 2012  Thoronador

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

#ifndef EXITCALLBACK_HPP
#define EXITCALLBACK_HPP

#include "BasicCallback.hpp"

/* integer constant that identifies the type of a callback record */
const int CBT_EXIT = 1;

class TExitCallback final: public TBasicCallback
{
  public:
    /* function to handle the callback, i.e. perform all necessary steps after
       the player has made his/her choice. Should return true on success, false
       on failure

       remarks:
           Derived classes have to implement their own version of that function.
    */
    virtual bool Handle() override;

    /* function to return the callback's type */
    virtual int GetType() override;
}; //class

#endif // EXITCALLBACK_HPP
