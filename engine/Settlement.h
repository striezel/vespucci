#ifndef SETTLEMENT_H
#define SETTLEMENT_H

#include "PascalTypes.h"

class TSettlement
{
  public:
    TSettlement(const LongInt X, const LongInt Y, const LongInt ANation);
    ~TSettlement();
    LongInt GetNation() const;
    void ChangeNation(const LongInt new_nation);
    LongInt GetPosX() const;
    LongInt GetPosY() const;
    void SetPosition(const LongInt x, const LongInt y);
  protected:
    LongInt m_Nation;
    LongInt PosX, PosY;
};//class

#endif

