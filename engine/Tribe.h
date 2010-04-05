#ifndef TRIBE_H
#define TRIBE_H

#include "Settlement.h"
#include "Nations.h"
#include "Units.h"
#include "PascalTypes.h"

struct TribeLocRec
{
  LongInt Nation;
  Byte x, y;
};

const int cTribeLocationsCount = 13;

const TribeLocRec cTribeLocations[cTribeLocationsCount] = {
      {Nation: cNationAztec, x: 11, y: 23},
      {Nation: cNationAztec, x: 16, y: 26},
      {Nation: cNationAztec, x: 23, y: 27},
      {Nation: cNationAztec, x: 25, y: 33},

      {Nation: cNationInca, x: 26, y: 42},
      {Nation: cNationInca, x: 34, y: 59},
      {Nation: cNationInca, x: 34, y: 65},
      {Nation: cNationInca, x: 35, y: 51},
      {Nation: cNationInca, x: 36, y: 55},

      {Nation: cNationCherokee, x: 20, y: 20},
      {Nation: cNationCherokee, x: 21, y: 17},
      {Nation: cNationCherokee, x: 24, y: 19},
      {Nation: cNationCherokee, x: 27, y: 22}
};

class TTribe: public TSettlement
{
  public:
    TTribe(const LongInt X, const LongInt Y, const LongInt ANation, const TUnitType KnownFor);
    void Teach(TUnit* AUnit);
  private:
      TUnitType m_KnownFor;
      bool m_HasTought[cMaxEuropean+1];
};//class

#endif //TRIBE_H
