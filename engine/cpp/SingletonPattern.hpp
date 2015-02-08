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

#ifndef SINGLETONPATTERN_HPP
#define SINGLETONPATTERN_HPP

template<class T>
class Singleton: public T
{
  public:
    virtual ~Singleton() {}

    /* singleton access method */
    static Singleton& get();
  private:
    /* constructor */
    Singleton() {}

    /* empty copy constructor */
    Singleton(const Singleton& op) {}
}; //class


template<typename T>
Singleton<T>& Singleton<T>::get()
{
  static Singleton<T> Instance;
  return Instance;
}

#endif // SINGLETONPATTERN_HPP
