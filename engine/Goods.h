#ifndef GOODS_H
#define GOODS_H

enum TGoodType {gtFood=0, gtSugar, gtTobacco, gtCotton, gtFur, gtWood, gtOre,
                gtSilver, gtHorses, gtRum, gtCigar, gtCloth, gtCoat, gtTradegoods,
                gtTool, gtMusket, gtHammer, gtLibertyBell, gtCross};

//workaround to simulate built-in Object Pascal functions
TGoodType High(const TGoodType gt);

TGoodType Low(const TGoodType gt);

TGoodType Succ(const TGoodType gt);

int Ord(const TGoodType gt);

struct GoodData
{
  unsigned char diff;
  unsigned char start_min, start_max;
  unsigned char min, max;
};

const GoodData cGoodPrices[] = {
             {diff: 8, start_min: 1, start_max: 3, min: 1, max: 6},//gtFood
             {diff: 2, start_min: 4, start_max: 7, min: 3, max: 7},//gtSugar
             {diff: 2, start_min: 3, start_max: 5, min: 2, max: 5},//gtTobacco
             {diff: 2, start_min: 2, start_max: 5, min: 2, max: 5},//gtCotton
             {diff: 2, start_min: 4, start_max: 6, min: 2, max: 6},//gtFur
             {diff: 5, start_min: 2, start_max: 2, min: 2, max: 2},//gtWood
             {diff: 3, start_min: 3, start_max: 6, min: 2, max: 6},//gtOre
             {diff: 1, start_min: 20, start_max: 20, min: 2, max: 20},//gtSilver
             {diff: 1, start_min: 2, start_max: 3, min: 2, max: 11},//gtHorses
             {diff: 1, start_min: 11, start_max: 13, min: 1, max: 20},//gtRum
             {diff: 1, start_min: 11, start_max: 13, min: 1, max: 20},//gtCigar
             {diff: 1, start_min: 11, start_max: 13, min: 1, max: 20},//gtCloth
             {diff: 1, start_min: 11, start_max: 13, min: 1, max: 20},//gtCoat
             {diff: 1, start_min: 2, start_max: 3, min: 2, max: 12},//gtTradegoods
             {diff: 1, start_min: 2, start_max: 2, min: 2, max: 9},//gtTool
             {diff: 1, start_min: 3, start_max: 3, min: 2, max: 20},//gtMusket
             {diff: 0, start_min: 0, start_max: 0, min: 0, max: 0},//gtHammer
             {diff: 0, start_min: 0, start_max: 0, min: 0, max: 0},//gtLibertyBell
             {diff: 0, start_min: 0, start_max: 0, min: 0, max: 0}//gtCross
           };

/*
 cGoodPrices: array [TGoodType] of record
                 diff: Byte;
                 start_min, start_max: Byte;
                 min, max: Byte;
               end =(
             (diff: 8; start_min: 1; start_max: 3; min: 1; max: 6),//gtFood
             (diff: 2; start_min: 4; start_max: 7; min: 3; max: 7),//gtSugar
             (diff: 2; start_min: 3; start_max: 5; min: 2; max: 5),//gtTobacco
             (diff: 2; start_min: 2; start_max: 5; min: 2; max: 5),//gtCotton
             (diff: 2; start_min: 4; start_max: 6; min: 2; max: 6),//gtFur
             (diff: 5; start_min: 2; start_max: 2; min: 2; max: 2),//gtWood
             (diff: 3; start_min: 3; start_max: 6; min: 2; max: 6),//gtOre
             (diff: 1; start_min: 20; start_max: 20; min: 2; max: 20),//gtSilver
             (diff: 1; start_min: 2; start_max: 3; min: 2; max: 11),//gtHorses
             (diff: 1; start_min: 11; start_max: 13; min: 1; max: 20),//gtRum
             (diff: 1; start_min: 11; start_max: 13; min: 1; max: 20),//gtCigar
             (diff: 1; start_min: 11; start_max: 13; min: 1; max: 20),//gtCloth
             (diff: 1; start_min: 11; start_max: 13; min: 1; max: 20),//gtCoat
             (diff: 1; start_min: 2; start_max: 3; min: 2; max: 12),//gtTradegoods
             (diff: 1; start_min: 2; start_max: 2; min: 2; max: 9),//gtTool
             (diff: 1; start_min: 3; start_max: 3; min: 2; max: 20),//gtMusket
             (diff: 0; start_min: 0; start_max: 0; min: 0; max: 0),//gtHammer
             (diff: 0; start_min: 0; start_max: 0; min: 0; max: 0),//gtLibertyBell
             (diff: 0; start_min: 0; start_max: 0; min: 0; max: 0)//gtCross
           ); */

#endif // GOODS_H

