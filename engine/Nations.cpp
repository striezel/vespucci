#include "Nations.h"
#include "Randomizer.h"

//**** functions of TNation ****

TNation::TNation(const LongInt num, const std::string& NameStr)
{
  m_count = num;
  m_NameStr = NameStr;
}//construc

TNation::~TNation()
{
  //empty
}//destructor

std::string TNation::GetName() const
{
  return m_NameStr;
}//func

void TNation::ChangeName(const std::string& new_name)
{
  if (new_name!="") m_NameStr = new_name;
}//proc

LongInt TNation::GetCount() const
{
  return m_count;
}//func

void TNation::ChangeCount(const LongInt new_num)
{
  m_count = new_num;
}//proc

bool TNation::SaveToStream(std::ofstream& fs) const
{
  fs.write((char*) &m_count, sizeof(LongInt));
  if (!fs.good()) return false;
  const LongInt i = m_NameStr.length();
  fs.write((char*) &i, sizeof(LongInt));
  if (!fs.good()) return false;
  fs.write(m_NameStr.c_str(), i);
  return fs.good();
}//func


// **** functions of TIndianNation ****

TIndianNation::TIndianNation(const LongInt num, const std::string& NameStr)
  : TNation(num, NameStr)
{
  //check number and pick a default in case of invalidity
  if ((m_count<cMinIndian) or (m_count>cMaxIndian))
    m_count = cNationArawak;
}//construc

bool TIndianNation::IsIndian() const
{
  return true;
}//func

bool TIndianNation::IsEuropean() const
{
  return false;
}//func


//**** functions of TEuropeanNation ****

TEuropeanNation::TEuropeanNation(const LongInt num, const std::string& NameStr,
                                 const std::string& NameOfLeader)
  : TNation(num, NameStr)
{
  TGoodType gt;
  //inherited Create(num, NameStr);
  //check number and pick some default value to make sure it's European
  if ((num<cMin_Nations) or (num>cMaxEuropean)) m_count = cNationEngland;
  m_Leader = NameOfLeader;
  m_Gold = 1000;
  m_TaxRate = 0;
  gt = Low(gtFood);
  while (gt<High(gt))
  {
    m_Boycotted[gt] = false;//no boycotts at beginning
    m_Prices[gt] = Randomizer::GetSingleton().random(cGoodPrices[gt].start_min, cGoodPrices[gt].start_max);
    gt = Succ(gt);
  }//while
  m_Boycotted[High(gtFood)] = false;
}//construc

TEuropeanNation::~TEuropeanNation()
{
  //inherited Destroy;
}//destruc

bool TEuropeanNation::IsIndian() const
{
  return false;
}//func

bool TEuropeanNation::IsEuropean() const
{
  return true;
}//func

std::string TEuropeanNation::GetLeaderName() const
{
  return m_Leader;
}//func

void TEuropeanNation::ChangeLeaderName(const std::string& NameOfLeader)
{
  if (NameOfLeader!="") m_Leader = NameOfLeader;
}//proc

Byte TEuropeanNation::GetTaxRate() const
{
  return m_TaxRate;
}//func

void TEuropeanNation::IncreaseTax(const Byte AddedPercentage)
{
  if ((AddedPercentage<100) and (m_TaxRate+AddedPercentage<100))
    m_TaxRate = m_TaxRate + AddedPercentage;
}//proc

void TEuropeanNation::ChangeTaxRate(const Byte NewPercentage)
{
  if (NewPercentage<100) m_TaxRate = NewPercentage;
}//proc

bool TEuropeanNation::IsBoycotted(const TGoodType AGood) const
{
  return m_Boycotted[AGood];
}//func

void TEuropeanNation::DoBoycott(const TGoodType AGood)
{
  if ((AGood!=gtLibertyBell) and (AGood!=gtHammer))
    m_Boycotted[AGood] = true;
}//proc

void TEuropeanNation::UndoBoycott(const TGoodType AGood)
{
  m_Boycotted[AGood] = false;
}//proc

LongInt TEuropeanNation::GetGold() const
{
  return m_Gold;
}//func

void TEuropeanNation::DecreaseGold(const LongInt amount)
{
  if (m_Gold>amount)
  {
    m_Gold = m_Gold - amount;
  }
  else m_Gold = 0;
}//proc

void TEuropeanNation::IncreaseGold(const LongInt amount)
{
  if (amount>=0)
  {
    m_Gold = m_Gold + amount;
  }
  else DecreaseGold(-amount);
}//proc

Byte TEuropeanNation::GetPrice(const TGoodType AGood, const bool low) const
{
  if (low)
  {
    return m_Prices[AGood];
  }
  else return m_Prices[AGood]+cGoodPrices[AGood].diff;
}//func

void TEuropeanNation::AdvancePrice(const TGoodType AGood)
{
  if (m_Prices[AGood]+cGoodPrices[AGood].diff<cGoodPrices[AGood].max)
  {
    m_Prices[AGood] = m_Prices[AGood]+1;
    //should display message to player
  }//if
}//proc

void TEuropeanNation::DropPrice(const TGoodType AGood)
{
  if (m_Prices[AGood]>cGoodPrices[AGood].min)
  {
    m_Prices[AGood] = m_Prices[AGood]-1;
    //should display a message to the player
  }//if
}//proc

void TEuropeanNation::ChangePrice(const TGoodType AGood, const Byte NewPrice)
{
  if (NewPrice<cGoodPrices[AGood].min)
  {
    m_Prices[AGood]= cGoodPrices[AGood].min;
  }
  else if (NewPrice+cGoodPrices[AGood].diff>cGoodPrices[AGood].max)
  {
    m_Prices[AGood] = cGoodPrices[AGood].max;
  }
  else m_Prices[AGood] = NewPrice;
}//proc

//tries to "buy" goods, but only does the money related stuff; returns true on success
bool TEuropeanNation::BuyGood(const TGoodType AGood, const Byte num)
{
  if (IsBoycotted(AGood) or (m_Gold<num*GetPrice(AGood, false))) return false;
  m_Gold = m_Gold - GetPrice(AGood, false)*num;
  //should display message about cost to the player -> GUI
  return true;
}//func

//tries to "sell" goods, but only does the money related stuff; returns true on success
bool TEuropeanNation::SellGood(const TGoodType AGood, const Byte num)
{
  if (IsBoycotted(AGood)) return false;
  const LongInt tax_amount = (GetPrice(AGood, true)*num*GetTaxRate()) / 100;
  m_Gold = m_Gold + GetPrice(AGood, true)*num - tax_amount;
  //should display message about gain & tax to the player -> GUI
  return true;
}//func

bool TEuropeanNation::SaveToStream(std::ofstream& fs) const
{
  if (!fs.good()) return false;
  fs.write((char*) &m_count, sizeof(LongInt));
  if (!fs.good()) return false;
  LongInt i = m_NameStr.length();
  fs.write((char*) &i, sizeof(LongInt));
  if (!fs.good()) return false;
  fs.write(m_NameStr.c_str(), i);
  if (!fs.good()) return false;
  i = m_Leader.length();
  fs.write((char*) &i, sizeof(LongInt));
  if (!fs.good()) return false;
  fs.write(m_Leader.c_str(), i);
  if (!fs.good()) return false;
  fs.write((char*) &m_Gold, sizeof(LongInt));
  fs.write((char*) &m_TaxRate, sizeof(Byte));
  if (!fs.good()) return false;
  for (i = Ord(Low(gtFood)); i<=Ord(High(gtFood)); ++i)
  {
    fs.write((char*) &(m_Boycotted[TGoodType(i)]), sizeof(bool));
    fs.write((char*) &(m_Prices[TGoodType(i)]), sizeof(Byte));
    if (!fs.good()) return false;
  }//for
  return fs.good();
}//func
