#include "Goods.h"

TGoodType High(const TGoodType gt)
{
  return gtCross;
}

TGoodType Low(const TGoodType gt)
{
  return gtFood;
}

TGoodType Succ(const TGoodType gt)
{
  if (gt<gtCross) return static_cast<TGoodType> (gt+1);
  throw 42; //maybe we should change that value
}

int Ord(const TGoodType gt)
{
  return static_cast<int> (gt);
}
