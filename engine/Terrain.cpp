#include "Terrain.h"


TTerrain::TTerrain(const TTerrainType ATerrain, const bool River,
            const bool Road, const bool Special,
            const bool Ploughed)
{
  m_Type = ATerrain;
  m_River = River;
  m_Road = Road;
  m_Special = Special;
  m_Ploughed = Ploughed;
}//constructor

TTerrain::~TTerrain()
{
  //empty
}//destruc

TTerrainType TTerrain::GetType()
{
  return m_Type;
}//func

TTerrainType TTerrain::ClearedBecomes()
{
  switch(m_Type)
  {
    /*ttArctic: Result:= ttArctic;
    ttSea: Result:= ttSea;
    ttOpenSea : Result:= ttOpenSea;
    ttPlains: Result:= ttPlains;
    ttGrassland: Result:= ttGrassland;
    ttPrairie: Result:= ttPrairie;
    ttSavannah: Result:= ttSavannah;
    ttMarsh: Result:= ttMarsh;
    ttSwamp: Result:= ttSwamp;
    ttDesert: Result:= ttDesert;
    ttTundra: Result:= ttTundra;*/
    case ttBoreal:
         return ttTundra;
    case ttWetland:
         return ttMarsh;
    case ttScrubForest:
         return ttDesert;
    case ttBroadleaf:
         return ttPrairie;
    case ttMixedForest:
         return ttPlains;
    case ttConiferForest:
         return ttGrassland;
    case ttRainForest:
         return ttSwamp;
    case ttTropicalForest:
         return ttSavannah;
    /*ttHills: Result:= ttHills;
    ttMountains: Result:= ttMountains;*/
    default:
         return m_Type;
  }//swi
}//func

bool TTerrain::HasForest()
{
  /*Result:= m_Type in [ttBoreal, ttWetland, ttScrubForest, ttBroadleaf, ttMixedForest, ttConiferForest,
                  ttRainForest, ttTropicalForest];*/
  return (m_Type>=ttBoreal) && (m_Type<=ttTropicalForest);
}

bool TTerrain::HasRiver()
{
  return m_River;
}

bool TTerrain::HasRoad()
{
  return m_Road;
}

bool TTerrain::HasSpecial()
{
  return m_Special;
}

bool TTerrain::IsPloughed()
{
  return m_Ploughed;
}

bool TTerrain::IsWater()
{
  return (m_Type==ttSea) or (m_Type==ttOpenSea);
}

unsigned char TTerrain::GetGoodProduction(const TGoodType AGood, const bool expert)
{
  unsigned char Result = 0;
  switch (m_Type)
  {
    //ttArctic: Result:= 0;
    case ttSea:
    case ttOpenSea: if (AGood==gtFood)
                    {
                      Result = 2;
                      if (m_Special) /*fish*/ Result = 5;
                      if (expert) Result = Result*2;
                    }//if
                    break;
    case ttPlains: if (AGood == gtFood)
                   {
                     Result = 5;
                     if (m_Special) /*wheat*/
                     {
                       if (expert) { Result = Result+4; }
                       else { Result = Result+2; }
                     }
                     if (m_Ploughed) Result = Result+1;
                     if (m_River) Result = Result+1;
                     if (expert) Result = Result+3;
                   }
                   else if (AGood == gtCotton)
                   {
                     Result = 2;
                     if (m_Ploughed) Result = Result+1;
                     if (m_River) Result = Result+1;
                     if (expert) Result = Result*2;
                   }
                   else if (AGood == gtOre)
                   {
                     Result = Result +1;
                     if (m_Road) Result = Result+1;
                     if (m_River) Result = Result+1;
                     if (expert) Result = Result*2;
                   }
                   break;
    case ttGrassland: if (AGood == gtFood)
                      {
                        Result = 3;
                        if (m_Ploughed) Result = Result+1;
                        if (m_River) Result = Result+1;
                        if (expert) Result = Result+3;
                      }
                      else if (AGood == gtTobacco)
                      {
                        Result = 3;
                        if (m_Ploughed) Result = Result+1;
                        if (m_River) Result = Result+1;
                        if (m_Special) /*best tobacco*/ Result = Result*2;
                        if (expert) Result = Result*2;
                      }
                      break;
    case ttPrairie: if (AGood == gtFood)
                    {
                      Result = 3;
                      if (m_Ploughed) Result = Result+1;
                      if (m_River) Result = Result+1;
                      if (expert) Result = Result+3;
                    }
                    else if (AGood == gtCotton)
                    {
                      Result = 3;
                      if (m_Ploughed) Result = Result+1;
                      if (m_River) Result = Result+1;
                      if (m_Special) /*best cotton*/ Result = Result*2;
                      if (expert) Result = Result*2;
                    }
                    break;
    case ttSavannah: if (AGood == gtFood)
                     {
                       Result = 4;
                       if (m_Ploughed) Result = Result+1;
                       if (m_River) Result = Result+1;
                       if (expert) Result = Result+3;
                     }
                     else if (AGood == gtSugar)
                     {
                       Result = 3;
                       if (m_Ploughed) Result = Result+1;
                       if (m_River) Result = Result+1;
                       if (m_Special) /*best sugar*/ Result = Result*2;
                       if (expert) Result = Result*2;
                     }
                     break;
    case ttMarsh: if (AGood == gtFood)
                  {
                    Result = 3;
                    if (m_Ploughed) Result = Result+1;
                    if (m_River) Result = Result+1;
                    if (expert) Result = Result+3;
                  }
                  else if (AGood == gtTobacco)
                  {
                    Result = 2;
                    if (m_Ploughed) Result = Result+1;
                    if (m_River) Result = Result+1;
                    if (expert) Result = Result*2;
                  }
                  else if (AGood == gtOre)
                  {
                    Result = 2;
                    if (m_Road) Result = Result+1;
                    if (m_River) Result = Result+1;
                    if (m_Special) /*Minerals*/ Result = Result+3;
                    if (expert) Result = Result*2;
                  }
                  break;
    case ttSwamp: if (AGood == gtFood)
                  {
                    Result = 3;
                    if (m_Ploughed) Result = Result+1;
                    if (m_River) Result = Result+1;
                    if (expert) Result = Result+3;
                  }
                  else if (AGood == gtSugar)
                  {
                    Result = 2;
                    if (m_Ploughed) Result = Result+1;
                    if (m_River) Result = Result+1;
                    if (expert) Result = Result*2;
                  }
                  else if (AGood == gtOre)
                  {
                    Result = 2;
                    if (m_Road) Result = Result+1;
                    if (m_River) Result = Result+1;
                    if (m_Special) /*Minerals*/ Result = Result+3;
                    if (expert) Result = Result*2;
                  }
                  break;
    case ttDesert: if (AGood == gtFood)
                   {
                     Result = 2;
                     if (m_Ploughed) Result = Result+1;
                     if (m_River) Result = Result+1;
                     if (m_Special) /*Oasis*/ Result = Result+2;
                     if (expert) Result = Result+3;
                   }
                   else if (AGood == gtCotton)
                   {
                     Result = 1;
                     if (m_Ploughed) Result = Result+1;
                     if (m_River) Result = Result+1;
                     if (expert) Result = Result*2;
                   }
                   else if (AGood == gtOre)
                   {
                     Result = 2;
                     if (m_Road) Result = Result+1;
                     if (m_River) Result = Result+1;
                     if (expert) Result = Result*2;
                   }
                   break;
    case ttTundra: if (AGood == gtFood)
                   {
                     Result = 3;
                     if (m_Ploughed) Result = Result+1;
                     if (m_River) Result = Result+1;
                     if (expert) Result = Result+3;
                   }
                   else if (AGood == gtOre)
                   {
                     Result = 2;
                     if (m_Road) Result = Result+1;
                     if (m_River) Result = Result+1;
                     if (m_Special) /*Minerals*/ Result = Result+3;
                     if (expert) Result = Result*2;
                   }
                   break;
    case ttBoreal: if (AGood == gtFood)
                   {
                     Result = 2;
                     if (m_River) Result = Result+1;
                     //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                     if (m_Special) /*deer*/ Result = Result+2;
                     if (expert) Result = Result+3;
                   }
                   else if (AGood == gtFur)
                   {
                     Result = 3;
                     if (m_River) Result = Result+1;
                     if (m_Road) Result = Result+1;
                     if (m_Special) /*deer*/ Result = Result+2;
                     if (expert) Result = Result*2;
                   }
                   else if (AGood == gtWood)
                   {
                     Result = 4;
                     if (m_River) Result = Result+1;
                     if (m_Road) Result = Result+1;
                     if (expert) Result = Result*2;
                   }
                   else if (AGood == gtOre)
                   {
                     Result = 1;
                     if (m_River) Result = Result+1;
                     if (m_Road) Result = Result+1;
                     if (expert) Result = Result*2;
                   }
                   break;
    //"Feuchtwald"
    case ttWetland: if (AGood == gtFood)
                    {
                      Result = 2;
                      if (m_River) Result = Result+1;
                      //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                      if (expert) Result = Result+3;
                    }
                    else if (AGood == gtTobacco)
                    {
                      Result = 1;
                      if (m_River) Result = Result+1;
                      //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                      if (expert) Result = Result*2;
                    }
                    else if (AGood == gtFur)
                    {
                      Result = 2;
                      if (m_River) Result = Result+2;//yes, it's two instead of one
                      if (m_Road) Result = Result+2;
                      if (expert) Result = Result*2;
                    }
                    else if (AGood == gtWood)
                    {
                      Result = 4;
                      if (m_River) Result = Result+2;//yes, it's two instead of one
                      if (m_Road) Result = Result+2;
                      if (expert) Result = Result*2;
                    }
                    else if (AGood == gtOre)
                    {
                      Result = 1;
                      if (m_River) Result = Result+1;
                      if (m_Road) Result = Result+1;
                      if (m_Special) /*Minerals*/ Result = Result+3;
                      if (expert) Result = Result*2;
                    }
                    break;
    case ttScrubForest: if (AGood == gtFood)
                        {
                          Result = 2;
                          if (m_River) Result = Result+1;
                          //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                          if (m_Special) /*Oasis*/ Result = Result+2;
                          if (expert) Result = Result+3;
                        }
                        else if (AGood == gtCotton)
                        {
                          Result = 1;
                          if (m_River) Result = Result+1;
                          //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                          if (expert) Result = Result*2;
                        }
                        else if (AGood == gtFur)
                        {
                          Result = 2;
                          if (m_River) Result = Result+2;//yes, that's two
                          if (m_Road) Result = Result+2;
                          if (expert) Result = Result*2;
                        }
                        else if (AGood == gtWood)
                        {
                          Result = 2;
                          if (m_River) Result = Result+2;//yes, that's two
                          if (m_Road) Result = Result+2;
                          if (expert) Result = Result*2;
                        }
                        else if (AGood == gtOre)
                        {
                          Result = 1;
                          if (m_River) Result = Result+1;
                          if (m_Road) Result = Result+1;
                          if (expert) Result = Result*2;
                        }
                        break;
    case ttBroadleaf: if (AGood == gtFood)
                      {
                        Result = 2;
                        if (m_River) Result = Result+1;
                        //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                        if (m_Special) /*deer*/
                        {
                          if (expert) { Result = Result+4; }
                          else { Result = Result+2; }
                        }
                        if (expert) Result = Result+3;
                      }
                      else if (AGood == gtCotton)
                      {
                        Result = 1;
                        if (m_River) Result = Result+1;
                        if (expert) Result = Result*2;
                      }
                      else if (AGood == gtFur)
                      {
                        Result = 2;
                        if (m_River) Result = Result+2;//yes, it's two, not one
                        if (m_Road) Result = Result+2;
                        if (m_Special) /*deer*/ Result = Result+2;
                        if (expert) Result = Result*2;
                      }
                      else if (AGood == gtWood)
                      {
                        Result = 4;
                        if (m_River) Result = Result+2;//yes, it's two, not one
                        if (m_Road) Result = Result+2;
                        if (expert) Result = Result*2;
                      }
                      break;
    case ttMixedForest: if (AGood == gtFood)
                        {
                          Result = 3;
                          if (m_River) Result = Result+1;
                          //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                          if (expert) Result = Result+3;
                        }
                        else if (AGood == gtCotton)
                        {
                          Result = 1;
                          if (m_River) Result = Result+1;
                          if (expert) Result = Result*2;
                        }
                        else if (AGood == gtFur)
                        {
                          Result = 3;
                          if (m_River) Result = Result+2;//yes, two :)
                          if (m_Road) Result = Result+2;
                          if (m_Special) /*beaver*/ Result = Result +3;
                          if (expert) Result = Result*2;
                        }
                        else if (AGood == gtWood)
                        {
                          Result = 6;
                          if (m_River) Result = Result+2;//yes, two :)
                          if (m_Road) Result = Result+2;
                          if (expert) Result = Result*2;
                        }//if
                        break;
    case ttConiferForest: if (AGood == gtFood)
                          {
                            Result = 2;
                            if (m_River) Result = Result+1;
                            //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                            if (expert) Result = Result+3;
                          }
                          else if (AGood == gtTobacco)
                          {
                            Result = 1;
                            if (m_River) Result = Result+1;
                            if (expert) Result = Result*2;
                          }
                          else if (AGood == gtFur)
                          {
                            Result = 2;
                            if (m_River) Result = Result+2; //yes, it's a +2
                            if (m_Road) Result = Result+2;
                            if (expert) Result = Result*2;
                          }
                          else if (AGood == gtWood)
                          {
                            Result = 6;
                            if (m_River) Result = Result+2; //yes, it's a +2
                            if (m_Road) Result = Result+2;
                            if (m_Special) /*best wood*/ Result = Result+4;
                            if (expert) Result = Result*2;
                          }
                          break;
    case ttRainForest: if (AGood == gtFood)
                       {
                         Result = 2;
                         if (m_River) Result = Result+1;
                         //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                         if (expert) Result = Result+3;
                       }
                       else if (AGood == gtSugar)
                       {
                         Result = 1;
                         if (m_River) Result = Result+1;
                         if (expert) Result = Result*2;
                       }
                       else if (AGood == gtFur)
                       {
                         Result = 1;
                         if (m_River) Result = Result+2;//a two here
                         if (m_Road) Result = Result+2;
                         if (expert) Result = Result*2;
                       }
                       else if (AGood == gtWood)
                       {
                         Result = 4;
                         if (m_River) Result = Result+2;//a two here
                         if (m_Road) Result = Result+2;
                         if (expert) Result = Result*2;
                       }
                       else if (AGood == gtOre)
                       {
                         Result = 1;
                         if (m_River) Result = Result+1;
                         if (m_Road) Result = Result+1;
                         if (m_Special) /*Minerals*/ Result = Result+3;
                         if (expert) Result = Result*2;
                       }
                       break;
    case ttTropicalForest: if (AGood == gtFood)
                           {
                             Result = 3;
                             if (m_River) Result = Result+1;
                             //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                             if (expert) Result = Result+3;
                           }
                           else if (AGood == gtSugar)
                           {
                             Result = 1;
                             if (m_River) Result = Result+1;
                             if (expert) Result = Result*2;
                           }
                           else if (AGood == gtFur)
                           {
                             Result = 2;
                             if (m_River) Result = Result+2;//yes, it's two
                             if (m_Road) Result = Result+2;
                             if (expert) Result = Result*2;
                           }
                           else if (AGood == gtWood)
                           {
                             Result = 4;
                             if (m_River) Result = Result+2;//yes, it's two
                             if (m_Road) Result = Result+2;
                             if (m_Special) /*best wood*/ Result = Result+4;
                             if (expert) Result = Result*2;
                           }
                           break;
    case ttHills: if (AGood == gtFood)
                  {
                    Result = 2;
                    if (m_River) Result = Result+1;
                    //if m_Ploughed then Result:= Result+1; //hills cannot be ploughed
                    if (expert) Result = Result+3;
                  }
                  else if (AGood == gtOre)
                  {
                    Result = 4;
                    if (m_River) Result = Result+1;
                    if (m_Road) Result = Result+1;
                    if (m_Special) /*Ore*/ Result = Result+2;
                    if (expert) Result = Result*2;
                  }
                  break;
    case ttMountains: if (AGood == gtOre)
                      {
                        Result = 4;
                        if (m_River) Result = Result+1;
                        if (m_Road) Result = Result+1;
                        if (expert) Result = Result*2;
                      }
                      else if (AGood == gtSilver)
                      {
                        Result = 1;
                        if (m_River) Result = Result+1;
                        if (m_Road) Result = Result+1;
                        if (m_Special) /*Silver*/ Result = Result+4;
                        if (expert) Result = Result*2;
                      }
                      break;
  }//swi
  return Result;
}//func

//defense bonus for terrain in %. Maximum is 150%.
unsigned char TTerrain::GetDefenceBonus()
{
  switch (m_Type)
  {
    case ttMarsh:
    case ttSwamp:
         return 25;
         break;
    case ttBoreal:
    case ttWetland:
    case ttScrubForest:
    case ttBroadleaf:
    case ttMixedForest:
    case ttConiferForest:
         return 50;
         break;
    case ttRainForest:
         return 75;
         break;
    case ttTropicalForest:
         return 50;
         break;
    case ttHills:
         return 100;
         break;
    case ttMountains:
         return 150;
         break;
    default:
         return 0;
  }//case
}//func

//**** Terrain alteration functions ****

void TTerrain::ClearForest()
{
  m_Type = this->ClearedBecomes();
}

void TTerrain::CreateRoad()
{
  m_Road = true;
}

void TTerrain::Plough()
{
  //can't plough in forest, has to deforest first
  if (HasForest())
  {
    ClearForest();
  }
  //can't plough in hills or mountains
  else if ((m_Type != ttHills) && (m_Type != ttMountains)) m_Ploughed = true;
}

void TTerrain::CreateSpecial()
{
  //can't have special in high sea
  if (m_Type!=ttOpenSea)
    m_Special = true;
}

//functions for colony base field
unsigned char TTerrain::GetColonyFood()
{
  switch (m_Type)
  {
    case ttArctic:
         return 2;
         break;
    case ttPlains:
    case ttGrassland:
    case ttPrairie:
    case ttSavannah:
    case ttMarsh:
    case ttSwamp:
    case ttTundra:
         return 5;
         break;
    case ttDesert:
    case ttScrubForest:
         return 3;
         break;
    case ttBoreal:
    case ttWetland:
         return 4;
         break;
    case ttBroadleaf:
    case ttMixedForest:
    case ttConiferForest:
    case ttRainForest:
    case ttTropicalForest:
    case ttHills:
         return 4;
         break;
    default:
         return 0;//ttSea, ttOpenSea, ttMountains
  }//swi
}//func

TGoodType TTerrain::GetColonyGoodType()
{
  switch (m_Type)
  {
    case ttPlains:
    case ttPrairie:
         return gtCotton; break;
    case ttGrassland:
    case ttMarsh:
         return gtTobacco; break;
    case ttSavannah:
    case ttSwamp:
    case ttRainForest:
         return gtSugar; break;
    case ttDesert:
    case ttTundra:
    case ttHills:
         return gtOre; break;
    case ttBoreal:
    case ttWetland:
    case ttScrubForest:
    case ttBroadleaf:
    case ttMixedForest:
    case ttConiferForest:
    case ttTropicalForest:
         return gtFur; break;
    default:
         return gtFood; //ttArctic, ttSea, ttOpenSea, ttMountains
         break;
  }//swi
}//func

unsigned char TTerrain::GetColonyGoodAmount()
{
  switch (m_Type)
  {
    case ttPlains:
    case ttMarsh:
    case ttSwamp:
    case ttDesert:
    case ttTundra:
    case ttWetland:
    case ttScrubForest:
    case ttBroadleaf:
         return 3;
         break;
    case ttGrassland:
    case ttPrairie:
    case ttSavannah:
    case ttBoreal:
    case ttMixedForest:
         return 4;
         break;
    case ttConiferForest:
    case ttTropicalForest:
         return 3;
         break;
    case ttRainForest:
         return 2;
         break;
    case ttHills:
         return 5;
         break;
    default:
         return 0;//ttArctic, ttSea, ttOpenSea, ttMountains
  }//swi
}//func
