/* ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2010  Dirk Stolle

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

#ifndef GOODS_HPP
#define GOODS_HPP

/* enumeration type for all possible goods within the game */
enum TGoodType { gtFood = 0, gtSugar, gtTobacco, gtCotton, gtFur, gtWood,
                 gtOre, gtSilver, gtHorses, gtRum, gtCigar, gtCloth,
                 gtCoat, gtTradegoods, gtTool, gtMusket, gtHammer,
                 gtLibertyBell, gtCross};

struct GoodData
{
  uint8_t diff;
  uint8_t start_min, start_max;
  uint8_t min, max;
}; //struct

const GoodData cGoodPrices[] = {
{diff: 8, start_min: 1, start_max: 3, min: 1, max: 6}, //gtFood
{diff: 2, start_min: 4, start_max: 7, min: 3, max: 7}, //gtSugar
{diff: 2, start_min: 3, start_max: 5, min: 2, max: 5}, //gtTobacco
{diff: 2, start_min: 2, start_max: 5, min: 2, max: 5}, //gtCotton
{diff: 2, start_min: 4, start_max: 6, min: 2, max: 6}, //gtFur
{diff: 5, start_min: 2, start_max: 2, min: 2, max: 2}, //gtWood
{diff: 3, start_min: 3, start_max: 6, min: 2, max: 6}, //gtOre
{diff: 1, start_min: 20, start_max: 20, min: 2, max: 20}, //gtSilver
{diff: 1, start_min: 2, start_max: 3, min: 2, max: 11}, //gtHorses
{diff: 1, start_min: 11, start_max: 13, min: 1, max: 20}, //gtRum
{diff: 1, start_min: 11, start_max: 13, min: 1, max: 20}, //gtCigar
{diff: 1, start_min: 11, start_max: 13, min: 1, max: 20}, //gtCloth
{diff: 1, start_min: 11, start_max: 13, min: 1, max: 20}, //gtCoat
{diff: 1, start_min: 2, start_max: 3, min: 2, max: 12}, //gtTradegoods
{diff: 1, start_min: 2, start_max: 2, min: 2, max: 9}, //gtTool
{diff: 1, start_min: 3, start_max: 3, min: 2, max: 20}, //gtMusket
{diff: 0, start_min: 0, start_max: 0, min: 0, max: 0}, //gtHammer
{diff: 0, start_min: 0, start_max: 0, min: 0, max: 0}, //gtLibertyBell
{diff: 0, start_min: 0, start_max: 0, min: 0, max: 0} //gtCross
};
#endif // GOODS_HPP
