/* **************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2010, 2011  Dirk Stolle

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

#ifndef INDIANNATION_HPP
#define INDIANNATION_HPP

#include "Nation.hpp"
#include <array>

/* enumeration type for tech level of an Indian nation */
enum TTechLevel {tlNomadic, tlAgricultural, tlDeveloped, tlCivilised};

/* enumeration type for attitude of Indians */
enum TIndianAttitude {iaPleased, iaWorried, iaAnxious, iaAngry, iaBelligerent};

/* ********
   **** TIndianNation class
   ****
   **** purpose: represents an Indian nation within the game. This class is a
   ****          more specialised version of TNation.
   *******
*/
class TIndianNation: public TNation
{
  protected:
    //tech level of that nation
    TTechLevel m_TechLevel;
    //contact with European Nations
    std::array<bool, cMaxEuropean+1> m_Contact;
    //attitude towards Europeans
    std::array<TIndianAttitude, cMaxEuropean+1> m_Attitude;
    //boolean that indicates whether this nation was present at the start of
    // the game (not necessarily all Indian Nations are spawned each game)
    bool m_Spawned;
  public:
    /* constructor

       parameters:
           num     - integer that identifies that nation
           NameStr - name of the nation
    */
    TIndianNation(const LongInt num, const std::string& NameStr);

    /* returns true, if this nation is an IndianNation

       remarks:
           Will always return true.
    */
    virtual bool IsIndian() const override;

    /* returns true, if this nation is an EuropeanNation

       remarks:
           Will always return false.
    */
    virtual bool IsEuropean() const override;

    /* returns the nation's tech level */
    TTechLevel GetTechLevel() const;

    /* returns whether this nation had already contact with an European nation

       parameters:
           num_EuroNat - the European nation's ID
    */
    bool HasContactWith(const LongInt num_EuroNat) const;

    /* sets the contact status with an European Nation

       parameters:
           num_EuroNat - the European nation's ID
           newContact  - the new contact state
    */
    void SetContactWith(const LongInt num_EuroNat, const bool newContact = true);

    /* returns this nation's attitude towards an European nation

       parameters:
           num_EuroNat - the European nation's ID
    */
    TIndianAttitude GetAttitude(const LongInt num_EuroNat) const;

    /* sets the nation's attitude towards an European Nation

       parameters:
           num_EuroNat - the European nation's ID
           newAttitude - the new attitude
    */
    void SetAttitude(const LongInt num_EuroNat, const TIndianAttitude newAttitude);

    /* returns true, if this nation spawned at the start of the current game.
       The default value right after constructor call is false. */
    bool GetSpawnStatus() const;

    /* sets the nation's spawn status

       parameters:
           new_stat - the new status value
    */
    void SetSpawnStatus(const bool new_stat);

    /* tries to save this nation's data to the given stream and returns true
       in case of success, or false if an error occurred

       parameters:
           fs - the file stream the nation shall be saved in

       remarks:
           The file stream already has to be opened and be ready for writing.
    */
    virtual bool SaveToStream(std::ofstream& fs) const override;

    /* loads the European nation from the stream and returns true on success

       parameters:
           fs   - the file stream the nation will be loaded from
    */
    virtual bool LoadFromStream(std::ifstream& fs) override;
}; //class

#endif // INDIANNATION_HPP
