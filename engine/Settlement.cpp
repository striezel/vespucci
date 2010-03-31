#include "Settlement.h"

// **** TSettlement functions ****

TSettlement::TSettlement(const LongInt X, const LongInt Y, const LongInt ANation)
{
  //no settlements outside of range or at border row/column (index: 0) allowed
  if (X>0)
  {
    PosX = X;
  }
  else
  {
    PosX = 1;
  }
  if (Y>0)
  {
    PosY = Y;
  }
  else
  {
    PosY = 1;
  }
  m_Nation = ANation;
}

TSettlement::~TSettlement()
{
  //emtpty
}//destruc

LongInt TSettlement::GetNation() const
{
  return m_Nation;
}//func

void TSettlement::ChangeNation(const LongInt new_nation)
{
  if (new_nation>=0) m_Nation = new_nation;
}//proc

LongInt TSettlement::GetPosX() const
{
  return PosX;
}//func

LongInt TSettlement::GetPosY() const
{
  return PosY;
}//func

void TSettlement::SetPosition(const LongInt x, const LongInt y)
{
  if (x>0) PosX = x; else PosX = 1;
  if (y>0) PosY = y; else PosY = 1;
}//proc

