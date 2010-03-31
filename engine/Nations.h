#ifndef NATIONS_H
#define NATIONS_H

#include "PascalTypes.h"
#include "Goods.h"
#include <string>
#include <fstream>

const LongInt cMin_Nations = 1;
const LongInt cMinEuropean = 1;
const LongInt cMax_Nations = 4;
const LongInt cMaxEuropean = 4;
const LongInt cMinIndian = 5;
const LongInt cMaxIndian = 12;

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

//the colours
const Byte cNationColours[cMaxIndian+1][3] = {
           /*filler for zero (invalid index to access)
             We didn't have this one in Pascal code, but Pascal allowed array
             with indices starting at 1 (or other arbitrary integer values).
           */
           { 80, 80, 80},
           //Europeans
           {255, 0, 0}, //England
           {50, 50, 255}, //France
           {255, 255, 0}, //Spain
           {255, 128, 0}, //Holland
           //indians
           {100, 140, 190}, //Arawak
           {200, 160, 30}, //Aztec
           {240, 240, 200}, //Inca
           {0, 100, 0}, //Tupi
           {120, 160, 80}, //Cherokee
           {110, 60, 25}, //Iroquois
           {140, 0, 0}, //Sioux
           {190, 170, 130} //Apache
};

class TNation
{
  protected:
    LongInt m_count;
    std::string m_NameStr;
  public:
    TNation(const LongInt num, const std::string& NameStr);
    virtual ~TNation();
    virtual bool IsIndian() const = 0;
    virtual bool IsEuropean() const = 0;
    LongInt GetCount() const;
    void ChangeCount(const LongInt new_num);
    std::string GetName() const;
    void ChangeName(const std::string& new_name);
    virtual bool SaveToStream(std::ofstream& fs) const;
};//class

class TIndianNation: public TNation
{
  public:
    TIndianNation(const LongInt num, const std::string& NameStr);
    virtual bool IsIndian() const;
    virtual bool IsEuropean() const;
};//class

class TEuropeanNation: public TNation
{
  protected:
    //Name of nation's leader
    std::string m_Leader;
    //amount of gold owned by this nation
    LongInt m_Gold;
    //tax rate (in %) for this nation
    Byte m_TaxRate; //can't be more than 100% ;) so a Byte will do
    bool m_Boycotted[gtCross+1]; //m_Boycotted: array [TGoodType] of Boolean;
    Byte m_Prices[gtCross+1];
  public:
    TEuropeanNation(const LongInt num, const std::string& NameStr,
                    const std::string& NameOfLeader);
    ~TEuropeanNation();

    virtual bool IsIndian() const;
    virtual bool IsEuropean() const;
    std::string GetLeaderName() const;
    void ChangeLeaderName(const std::string& NameOfLeader);

    //tax rate for this nation in percent
    Byte GetTaxRate() const;
    void IncreaseTax(const Byte AddedPercentage);
    //ChangeTaxRate only used during loading; use IncreaseTax on other occassions
    void ChangeTaxRate(const Byte NewPercentage);

    bool IsBoycotted(const TGoodType AGood) const;
    void DoBoycott(const TGoodType AGood);
    void UndoBoycott(const TGoodType AGood);

    LongInt GetGold() const;
    void DecreaseGold(const LongInt amount);
    void IncreaseGold(const LongInt amount);
    Byte GetPrice(const TGoodType AGood, const bool low) const;
    //increases price if good, if below upper limit
    void AdvancePrice(const TGoodType AGood);
    //decrease price, if above lower limit
    void DropPrice(const TGoodType AGood);
    //set new price, if within limits
    void ChangePrice(const TGoodType AGood, const Byte NewPrice);
    //functions to buy and sell goods, just does the gold-related stuff
    bool BuyGood(const TGoodType AGood, const Byte num);
    bool SellGood(const TGoodType AGood, const Byte num);

    virtual bool SaveToStream(std::ofstream& fs) const;
};//class

#endif

