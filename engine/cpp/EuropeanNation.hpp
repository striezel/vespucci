/* ***************************************************************************

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

#ifndef EUROPEANNATION_HPP
#define EUROPEANNATION_HPP

#include "Nation.hpp"
#include <array>
#include "FoundingFathers.hpp"
#include "Goods.hpp"


/* enumeration type to describe the relation between two European nations */
enum TDiplomaticStatus {dsUndefined, dsPeace, dsWar};

/* ********
   **** TEuropeanNation class
   ****
   **** purpose: represents an European nation within the game. This class is
   ****          a more specialised version of TNation.
   *******
*/

class TEuropeanNation: public TNation
{
  private:
    //Name of nation's leader
    std::string m_Leader;
    //amount of gold owned by this nation
    LongInt m_Gold;
    //tax rate (in %) for this nation
    Byte m_TaxRate; //can't be more than 100% ;) so a Byte will do
    std::array<bool, gtCross+1> m_Boycotted;
    std::array<Byte, gtCross+1> m_Prices;
    //number of destroyed indian villages
    LongInt m_VillagesBurned;
    //list of current founding fathers
    std::array<bool, ffNone+1> m_FoundingFathers; //array[TFoundingFathers] of Boolean;
    //next founding father
    TFoundingFathers m_NextFoundingFather;
    //number of collected liberty bells since last founding father joined
    Word m_LibertyBells;
    //contains relationship to other nations
    std::array<TDiplomaticStatus, cMaxEuropean+1> m_Diplomatic;
    //location of spawnpoint for units on map
    LongInt m_SpawnX, m_SpawnY;
  public:
    /* constructor

       parameters:
           num          - integer that identifies that nation
           NameStr      - name of the nation
           NameOfLeader - name of the nation's leader
    */
    TEuropeanNation(const LongInt num, const std::string& NameStr,
                    const std::string& NameOfLeader);

    /* destructor */
    ~TEuropeanNation();

    /* returns true, if this nation is an IndianNation

       remarks:
           Will always return false.
    */
    virtual bool IsIndian() const override;

    /* returns true, if this nation is an EuropeanNation

       remarks:
           Will always return true.
    */
    virtual bool IsEuropean() const override;

    /* returns the name of the nation's leader */
    const std::string& GetLeaderName() const;

    /* changes the name of the nation's leader

       parameters:
           NameOfLeader - new name of the nation's leader

       remarks:
           You should not use this procedure directly; the leader's name can
           be set during creation/ via constructor parameters. This function
           is only used during loading process.
    */
    void ChangeLeaderName(const std::string& NameOfLeader);

    /* sets the map square where new units (ships) of that nation will spawn
       after creation

       parameters:
           x, y - coordinates of the map square
    */
    void SetSpawnpoint(const LongInt x, const LongInt y);

    /* returns x-coordinate of the spawnpoint */
    LongInt GetSpawnpointX() const;

    /* returns y-coordinate of the spawnpoint */
    LongInt GetSpawnpointY() const;

    /* returns true, if this nation has valid spawnpoint coordinates */
    bool HasValidSpawnpoint() const;

    //returns the tax rate for this nation in percent
    Byte GetTaxRate() const;

    /* increases the nation's tax rate

       parameters:
           AddedPercentage - amount that should be added to the current tax rate
    */
    void IncreaseTax(const Byte AddedPercentage);

    /* sets the nation's tax rate directly

       parameters:
           NewPercentage - new tax rate in percent

       remarks:
           ChangeTaxRate is only used during loading; use IncreaseTax on other
           occassions.
    */
    void ChangeTaxRate(const Byte NewPercentage);

    /* returns true, if a certain good is boycotted by parliament

       parameters:
           AGood - the good to check for boycott
    */
    bool IsBoycotted(const TGoodType AGood) const;

    /* boycotts a certain good, i.e. it cannot be traded in European harbour
       any more, until the boycott is removed

       parameters:
           AGood - the good that will be boycotted
    */
    void DoBoycott(const TGoodType AGood);

    /* removes boycott from a certain good, i.e. it can be traded in European
       harbour again

       parameters:
           AGood - the good that will not be boycotted any more
    */
    void UndoBoycott(const TGoodType AGood);

    /* removes boycott from all goods, i.e. all goods can be traded in European
       harbour again

       remarks:
           This procedure is only used when Jakob Fugger joins the nation's
           continental congress, because his effect/power is to remove all
           boycotts.
    */
    void UndoAllBoycotts();

    /* returns the current amount of gold that this nation has */
    LongInt GetGold() const;

    /* decreases the current amount of gold that this nation has

       parameters:
           amount - amount of gold pieces that should be removed

       remarks:
           This procedure cannot reduce the nation's gold amount to
           less than zero.
    */
    void DecreaseGold(const LongInt amount);

    /* increases the current amount of gold that this nation has

       parameters:
           amount - amount of gold pieces that should be added
    */
    void IncreaseGold(const LongInt amount);

    /* returns the current price of a certain good in Europe

       parameters:
           AGood - the good to check for
           low   - boolean that indicates whether you want the good's low
                   price (true), i.e. the price you get when selling this good,
                   or the high price (false), i.e. the price you have to buy
                   when buying this good.
    */
    Byte GetPrice(const TGoodType AGood, const bool low) const;

    /* increases the price of a certain good, if the price is still below the
       upper price limit set in the game rules (see Goods.pas for limits)

       parameters:
           AGood - the good whose price has to be raised (by one gold piece)
    */
    void AdvancePrice(const TGoodType AGood);

    /* decreases the price of a certain good, if the current price is still
       above the lower limit defined in the game rules (see Goods.pas for limits)

       parameters:
           AGood - the good whose price has to be lowered (by one gold piece)
    */
    void DropPrice(const TGoodType AGood);

    /* sets the price of a certain good directly, if the new price is within
       the limits defined in the game rules (see Goods.pas for limits)

       parameters:
           AGood    - the good whose price has to be set
           NewPrice - the new price of that good

      remarks:
          If the new price exceeds the given limits, it will be set to the
          upper or lower limit, respectively.
          Do not call this function directly - it's only used during the
          loading process.
    */
    void ChangePrice(const TGoodType AGood, const Byte NewPrice);

    //functions to buy and sell goods, just does the gold-related stuff
    /* buys a certain amount of a good in Europe and returns true on success

       parameters:
           AGood - the good that will be bought
           num   - the amount of that good that will be bought
    */
    bool BuyGood(const TGoodType AGood, const Byte num);

    /* sells a certain amount of a good in Europe and returns true on success

       parameters:
           AGood - the good that will be sold
           num   - the amount of that good that will be sold
    */
    bool SellGood(const TGoodType AGood, const Byte num);

    /* returns the number of Indian villages that have been destroyed by this
       European Nation
    */
    LongInt GetVillagesBurned() const;

    /* returns true, if the given founding father is present at the nation's
       congress

       parameters:
           ff - the founding father which has to be checked for
    */
    bool HasFoundingFather(const TFoundingFathers ff) const;

    /* sets the presence state of a certain founding father

       parameters:
           ff      - the founding father whose state is set
           present - true, if the founding father shall be present, false
                     otherwise
    */
    void SetFoundingFather(const TFoundingFathers ff, const bool present);

    /* returns the number of founding fathers that are present in this nation's
       congress
    */
    Byte GetPresentFoundingFathers() const;

    /* returns the number of liberty bells that this nation has produced yet */
    Word GetLibertyBells() const;

    /* adds to the number of produced liberty bells

       parameters:
           lb - the number of new liberty bells, i.e. that amount that will be
                added to the number of current liberty bells
    */
    void AddLibertyBells(const Word lb);

    /* returns the next founding father that will join this nation's congress */
    TFoundingFathers GetNextFoundingFather() const;

    /* sets the next founding father that will join this nation's congress

       parameters:
           ff     - enumeration value that indicates the next founding father
    */
    void SetNextFoundingFather(const TFoundingFathers ff);

    /* returns an array of founding fathers the player can choose from */
    TFoundingFatherArray GetFoundingFatherSelection() const;

    /* sets the number of Indian villages that have been destroyed by this
       European Nation

       parameters:
           villages - the new amount of villages

       remarks:
          Do not call this function directly - it's only used during the
          loading process.
    */
    void SetVillagesBurned(const LongInt villages);

    /* returns the diplomatic status of the relations between this nation and
       the specified other European nation

       parameters:
           other_nation - integer constant identifying the other nation

       remarks:
           If other_nation does not identify a European nation, dsUndefined
           will be returned. However, dsUndefined is also a valid return value,
           if this nation and the other nation have not met yet.
    */
    TDiplomaticStatus GetDiplomatic(const LongInt other_nation) const;

    /* sets the diplomatic status of the relations between this nation and
       the specified other European nation

       parameters:
           other_nation - integer constant identifying the other nation
           new_status   - the status that has to be set

       remarks:
           If other_nation does not identify a European nation, then no changes
           are made to any diplomatic status.
    */
    void SetDiplomatic(const LongInt other_nation, const TDiplomaticStatus new_status);

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
}; //class TEuropeanNation

#endif // EUROPEANNATION_HPP
