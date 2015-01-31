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

#include "Terrain.hpp"

TTerrain::TTerrain(const TTerrainType ATerrain, const bool River,
                   const bool Road, const bool Special,
                   const bool Ploughed)
: m_Type(ATerrain),
  m_River(River),
  m_Road(Road),
  m_Special(Special),
  m_Ploughed(Ploughed)
{
}

TTerrain::~TTerrain()
{
  //empty
}

TTerrainType TTerrain::GetType() const
{
  return m_Type;
}

TTerrainType TTerrain::ClearedBecomes() const
{
  switch (m_Type)
  {
    /*ttArctic: Result = ttArctic;
    ttSea: Result = ttSea;
    ttOpenSea : Result = ttOpenSea;
    ttPlains: Result = ttPlains;
    ttGrassland: Result = ttGrassland;
    ttPrairie: Result = ttPrairie;
    ttSavannah: Result = ttSavannah;
    ttMarsh: Result = ttMarsh;
    ttSwamp: Result = ttSwamp;
    ttDesert: Result = ttDesert;
    ttTundra: Result = ttTundra;*/
    case ttBoreal: return ttTundra;
    case ttWetland: return ttMarsh;
    case ttScrubForest: return ttDesert;
    case ttBroadleaf: return ttPrairie;
    case ttMixedForest: return ttPlains;
    case ttConiferForest: return ttGrassland;
    case ttRainForest: return ttSwamp;
    case ttTropicalForest: return ttSavannah;
    /*ttHills: Result = ttHills;
    ttMountains: Result = ttMountains;*/
    default:
      return m_Type;
  } //swi
}//func

bool TTerrain::HasForest() const
{
  switch (m_Type)
  {
    case ttBoreal:
    case ttWetland:
    case ttScrubForest:
    case ttBroadleaf:
    case ttMixedForest:
    case ttConiferForest:
    case ttRainForest:
    case ttTropicalForest:
         return true;
    default:
         return false;
  } //swi
}

bool TTerrain::HasRiver() const
{
  return m_River;
}

bool TTerrain::HasRoad() const
{
  return m_Road;
}

bool TTerrain::HasSpecial() const
{
  return m_Special;
}

bool TTerrain::IsPloughed() const
{
  return m_Ploughed;
}

bool TTerrain::IsWater() const
{
  return (m_Type == ttSea || m_Type == ttOpenSea);
}

uint8_t TTerrain::GetGoodProduction(const TGoodType AGood, const bool expert) const
{
  uint8_t Result = 0;
  switch (m_Type)
  {
    //case ttArctic: Result = 0;
    case ttSea:
    case ttOpenSea:
         if (AGood == gtFood)
         {
           Result = 2;
           if (m_Special) /*fish*/ Result = 5;
           if (expert) Result *= 2;
         } //if
         break;
    case ttPlains:
         if (AGood == gtFood)
         {
           Result = 5;
           if (m_Special) /*wheat*/
           {
             if (expert)
               Result = Result + 4;
             else Result = Result + 2;
           }
           if (m_Ploughed) Result = Result + 1;
           if (m_River) Result += 1;
           if (expert) Result += 3;
         } //if
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
    case ttGrassland:
         if (AGood == gtFood)
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
    case ttPrairie:
         if (AGood == gtFood)
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
    case ttSavannah:
         if (AGood == gtFood)
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
    case ttMarsh:
         if (AGood == gtFood)
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
    case ttSwamp:
         if (AGood == gtFood)
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
    case ttDesert:
         if (AGood == gtFood)
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
    case ttTundra:
         if (AGood == gtFood)
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
    case ttBoreal:
         if (AGood == gtFood)
         {
           Result = 2;
           if (m_River) Result = Result+1;
           //if (m_Ploughed) Result = Result+1; //forest cannot be ploughed
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
    case ttWetland:
         if (AGood == gtFood)
         {
           Result = 2;
           if (m_River) Result = Result+1;
           //if (m_Ploughed) Result = Result+1; //forest cannot be ploughed
           if (expert) Result = Result+3;
         }
         else if (AGood == gtTobacco)
         {
           Result = 1;
           if (m_River) Result = Result+1;
           //if (m_Ploughed) Result = Result+1; //forest cannot be ploughed
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
    case ttScrubForest:
         if (AGood == gtFood)
         {
           Result = 2;
           if (m_River) Result = Result+1;
           //if (m_Ploughed) Result = Result+1; //forest cannot be ploughed
           if (m_Special) /*Oasis*/ Result = Result+2;
           if (expert) Result = Result+3;
         }
         else if (AGood == gtCotton)
         {
           Result = 1;
           if (m_River) Result = Result+1;
           //if (m_Ploughed) Result = Result+1; //forest cannot be ploughed
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
    case ttBroadleaf:
         if (AGood == gtFood)
         {
           Result = 2;
           if (m_River) Result = Result+1;
           //if (m_Ploughed) Result = Result+1; //forest cannot be ploughed
           if (m_Special) /*deer*/
           {
             if (expert) Result = Result+4;
             else Result = Result+2;
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
    case ttMixedForest:
         if (AGood == gtFood)
         {
           Result = 3;
           if (m_River) Result = Result+1;
           //if (m_Ploughed) Result = Result+1; //forest cannot be ploughed
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
         }
         break;
    case ttConiferForest:
         if (AGood == gtFood)
         {
           Result = 2;
           if (m_River) Result = Result+1;
           //if (m_Ploughed) Result = Result+1; //forest cannot be ploughed
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
    case ttRainForest:
         if (AGood == gtFood)
         {
           Result = 2;
           if (m_River) Result = Result+1;
           //if (m_Ploughed) Result = Result+1; //forest cannot be ploughed
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
    case ttTropicalForest:
         if (AGood == gtFood)
         {
           Result = 3;
           if (m_River) Result = Result+1;
           //if (m_Ploughed) Result = Result+1; //forest cannot be ploughed
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
           //if (m_Ploughed) Result = Result+1; //hills cannot be ploughed
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
    case ttMountains:
         if (AGood == gtOre)
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
  } //swi
  return Result;
} //func

//defense bonus for terrain in %. Maximum is 150%.
uint8_t TTerrain::GetDefenceBonus() const
{
  switch (m_Type)
  {
    case ttMarsh:
    case ttSwamp:
         return 25;
    case ttBoreal:
    case ttWetland:
    case ttScrubForest:
    case ttBroadleaf:
    case ttMixedForest:
    case ttConiferForest:
         return 50;
    case ttRainForest:
         return 75;
    case ttTropicalForest:
         return 50;
    case ttHills:
         return 100;
    case ttMountains:
         return 150;
    default:
         return 0;
  } //swi
}//func

//**** Terrain alteration functions ****

void TTerrain::ClearForest()
{
  m_Type = ClearedBecomes();
}

void TTerrain::CreateRoad()
{
  m_Road = true;
}

void TTerrain::Plough()
{
  //can't plough in forest, has to deforest first
  if (HasForest())
    ClearForest();
  //can't plough in hills or mountains
  else if ((m_Type != ttHills) && (m_Type != ttMountains))
    m_Ploughed = true;
}

void TTerrain::CreateSpecial()
{
  //can't have special in high sea
  if (m_Type != ttOpenSea)
    m_Special = true;
}

//functions for colony base field
uint8_t TTerrain::GetColonyFood() const
{
  switch (m_Type)
  {
    case ttArctic: return 2;
    case ttPlains:
    case ttGrassland:
    case ttPrairie:
    case ttSavannah:
    case ttMarsh:
    case ttSwamp:
    case ttTundra:
         return 5;
    case ttDesert:
    case ttScrubForest:
         return 3;
    case ttBoreal:
    case ttWetland:
         return 4;
    case ttBroadleaf:
    case ttMixedForest:
    case ttConiferForest:
    case ttRainForest:
    case ttTropicalForest:
    case ttHills:
         return 4;
    default:
         return 0;//ttSea, ttOpenSea, ttMountains
  } //swi
}//func

TGoodType TTerrain::GetColonyGoodType() const
{
  switch (m_Type)
  {
    case ttPlains:
    case ttPrairie:
         return gtCotton;
    case ttGrassland:
    case ttMarsh:
         return gtTobacco;
    case ttSavannah:
    case ttSwamp:
    case ttRainForest:
         return gtSugar;
    case ttDesert:
    case ttTundra:
    case ttHills:
         return gtOre;
    case ttBoreal:
    case ttWetland:
    case ttScrubForest:
    case ttBroadleaf:
    case ttMixedForest:
    case ttConiferForest:
    case ttTropicalForest:
         return gtFur;
    default:
         return gtFood; //ttArctic, ttSea, ttOpenSea, ttMountains
  } //swi
} //func

uint8_t TTerrain::GetColonyGoodAmount() const
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
    case ttGrassland:
    case ttPrairie:
    case ttSavannah:
    case ttBoreal:
    case ttMixedForest:
         return 4;
    case ttConiferForest:
    case ttTropicalForest:
         return 3;
    case ttRainForest:
         return 2;
    case ttHills:
         return 5;
    default:
         return 0;//ttArctic, ttSea, ttOpenSea, ttMountains
  } //swi
}//func
