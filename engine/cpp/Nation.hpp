/* ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2009, 2010  Thoronador

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

#ifndef NATIONS_HPP
#define NATIONS_HPP

#include "PascalTypes.hpp"
#include <array>
#include <string>

const LongInt cMinNations = 1;
const LongInt cMinEuropean = 1;
const LongInt cMaxEuropean = 4;
const LongInt cMinIndian = 5;
const LongInt cMaxIndian = 12;
const LongInt cMaxNations = 12;

/* integer constant representing a nation */
//Europeans
const LongInt cNationEngland = 1;
const LongInt cNationFrance = 2;
const LongInt cNationSpain = 3;
const LongInt cNationHolland = 4;
//Indians
const LongInt cNationArawak = 5;
const LongInt cNationAztec = 6;
const LongInt cNationInca = 7;
const LongInt cNationTupi = 8;
const LongInt cNationCherokee = 9;
const LongInt cNationIroquois = 10;
const LongInt cNationSioux = 11;
const LongInt cNationApache = 12;

//the colours of the nations as RGB values
const std::array<std::array<Byte,3>, cMaxNations+1> cNationColours
      ={{
          /* filler for zero (invalid index to access)
             We didn't have this one in Pascal code, but Pascal allowed array
             with indices starting at 1 (or other arbitrary integer values).
          */
          {{80, 80, 80}},
          //europeans
          {{255, 0, 0}}, //England
          {{50, 50, 255}}, //France
          {{255, 255, 0}}, //Spain
          {{255, 128, 0}}, //Holland
          //indians
          {{100, 140, 190}}, //Arawak
          {{200, 160, 30}}, //Aztec
          {{240, 240, 200}}, //Inca
          {{0, 100, 0}}, //Tupi
          {{120, 160, 80}}, //Cherokee
          {{110, 60, 25}}, //Iroquois
          {{140, 0, 0}}, //Sioux
          {{190, 170, 130}} //Apache
        }};


/* ********
   **** TNation class
   ****
   **** purpose: represents a nation within the game, i.e. a European country
   ****          or an Indian nation. However, there are more specialised
   ****          classes for both, derived from TNation.
   *******
*/
class TNation
{
  protected:
    LongInt m_count;
    std::string m_NameStr;
  public:
    /* constructor

       parameters:
           num     - integer that identifies that nation
           NameStr - name of the nation
    */
    TNation(const LongInt num, const std::string& NameStr);

    /* destructor */
    virtual ~TNation();

    /* returns true, if this nation is an IndianNation

       remarks:
           Abstract function, has to be implemented in derived classes.
    */
    virtual bool IsIndian() const = 0;

    /* returns true, if this nation is an EuropeanNation

       remarks:
           Abstract function, has to be implemented in derived classes.
    */
    virtual bool IsEuropean() const = 0;

    /* returns the integer that identifies that nation */
    LongInt GetCount() const;

    /* set the integer that identifies that nation

       parameters:
           new_num - new integer value that should identify that nation

       remarks:
           This procedure should not be called directly, it's only used during
           the loading process.
    */
    void ChangeCount(const LongInt new_num);

    /* returns the nation's name */
    const std::string& GetName() const;

    /* changes the nation's name

       parameters:
           new_name - the new name of the nation - empty string is not allowed
    */
    void ChangeName(const std::string& new_name);

    /* tries to save this nation's data to the given stream and returns true
       in case of success, or false if an error occured

       parameters:
           fs - the file stream the nation shall be saved in

       remarks:
           The file stream already has to be opened and be ready for writing.
    */
    virtual bool SaveToStream(std::ofstream& fs) const;

    /* loads the nation from the stream and returns true on success

       parameters:
           fs   - the file stream the nation will be loaded from
    */
    virtual bool LoadFromStream(std::ifstream& fs);
}; //class

#endif // NATIONS_HPP
