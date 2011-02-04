{ ***************************************************************************

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
}

unit Terrain;

interface

uses
  Goods;

const
  TERRAIN_RIVER_BIT = 1;
  TERRAIN_ROAD_BIT = 2;
  TERRAIN_SPECIAL_BIT = 4;
  TERRAIN_PLOUGHED_BIT = 8;

type
  { enumeration type to represent the terrain type }
  TTerrainType = (//open terrain types
                  ttArctic, ttSea, ttOpenSea, ttPlains, ttGrassland, ttPrairie,
                  ttSavannah, ttMarsh, ttSwamp, ttDesert, ttTundra,
                  //forested terrain types
                  ttBoreal, ttWetland, ttScrubForest, ttBroadleaf, ttMixedForest,
                  ttConiferForest, ttRainForest, ttTropicalForest,
                  //others
                  ttHills, ttMountains);

{ ********
    **** TTerrain class
    ****
    **** purpose: holds the information about the terrain, i.e. one square in
    ****          the map, such as type and whether it has a road or a river.
    *******
  }
  TTerrain = class
    private
      m_River: Boolean;
      m_Road: Boolean;

      {**indicates, whether terrain has a special ressource.
        arctic: none
        Sea, OpenSea: Fish
        Plains: Wheat
        Grassland: Tobacco
        Prairie: Cotton
        Savannah: Sugar
        Marsh: Minerals
        Swamp: Minerals
        Desert: Oasis
        Tundra: None allowed
        Boreal: Wild/ Deer
        Wetland: Minerals
        ScrubForest: Oasis
        Broadleaf: Wild/Deer
        MixedForest: Beaver
        ConiferForest: Best Wood
        RainForest: Minerals
        TropicalForest: BestWood
        Hills: Ore
        Mountains: Silver
       **}
      m_Special: Boolean;
      m_Ploughed: Boolean;

    public
      { type of the terrain

         remarks:
             In theory, this member should be private and not public, but I
             decided to have this one public for faster access.
      }
      m_Type: TTerrainType;

      { constructor

        parameters:
            ATerrain - type of that terrain
            River    - boolean to indicate whether or not there shall be a river
            Road     - boolean to indicate whether or not there shall be a road
            Special  - boolean to indicate whether or not there shall be a
                       special ressource on that field
            Ploughed - boolean to indicate whether or not this terrain shall be
                       ploughed
      }
      constructor Create(const ATerrain: TTerrainType; const River: Boolean=False;
                         const Road: Boolean=False; const Special: Boolean=False;
                         const Ploughed: Boolean=False);

      { destructor }
      destructor Destroy; override;

      { returns the type of the terrain }
      function GetType: TTerrainType;

      { returns the type this terrain would become, if all forest would be
        removed }
      function ClearedBecomes: TTerrainType;

      { returns true, if there's forest on that terrain }
      function HasForest: Boolean;

      { returns true, if there's a river flowing through that square }
      function HasRiver: Boolean;

      { returns true, if this terrain has a road }
      function HasRoad: Boolean;

      { returns true, if this square has a special ressource }
      function HasSpecial: Boolean;

      { returns true, if this terrain was ploughed }
      function IsPloughed: Boolean;

      { returns true, if this terrain isn't land but water }
      function IsWater: Boolean;

      { returns the amount of a certain good that would be produced in that
        field, if a certain unit was working here

        parameters:
            AGood  - the good that shall be "produced" (i.e. food, cotton,
                     tobacco, wood, ...)
            expert - boolean value that indicates, if the unit is an expert for
                     the production of this good (true) or not (false)
      }
      function GetGoodProduction(const AGood: TGoodType; const expert: Boolean): Byte;
      //for colony base fields
      { returns the amount of food that would be produced in that field, if a
        colony was build here
      }
      function GetColonyFood: Byte;

      { returns the type of goods that would be produced in that field, if a
        colony was build here (use GetColonyGoodAmount to get the amount)
      }
      function GetColonyGoodType: TGoodType;

      { returns the amount of goods that would be produced in that field, if a
        colony was build here (use GetColonyGoodType to get the good type)
      }
      function GetColonyGoodAmount: Byte;
      //defense bonus for terrain, in percent. Maximum is 150, so Byte will do.
      function GetDefenceBonus: Byte;

      //change terrain state
      { if this terrain has a forest, this procedure will remove it }
      procedure ClearForest;

      { creates a road on that field }
      procedure CreateRoad;

      { ploughs that field }
      procedure Plough;

      //changes that should only take place at initialization
      { creates a special ressource here }
      procedure CreateSpecial;
  end;//class

const
  ttFlachland = ttPlains;
  ttGreenland = ttGrassland;
  //ttFeuchtgebiete = ttMarsh;
  //ttGestrueppwald = ttScrub;
  //ttNadelwald = ttConifer;
  //ttLaubwald = ttBroadleafForest;
  //ttFeuchtwald = ttWetland;

implementation

constructor TTerrain.Create(const ATerrain: TTerrainType; const River: Boolean=False;
                            const Road: Boolean=False; const Special: Boolean=False;
                            const Ploughed: Boolean=False);
begin
  inherited Create;
  m_Type:= ATerrain;
  m_River:= River;
  m_Road:= Road;
  m_Special:= Special;
  m_Ploughed:= Ploughed;
end;//constructor

destructor TTerrain.Destroy;
begin
  inherited Destroy;
end;//destruc

function TTerrain.GetType: TTerrainType;
begin
  Result:= m_Type;
end;//func

function TTerrain.ClearedBecomes: TTerrainType;
begin
  case m_Type of
    {ttArctic: Result:= ttArctic;
    ttSea: Result:= ttSea;
    ttOpenSea : Result:= ttOpenSea;
    ttPlains: Result:= ttPlains;
    ttGrassland: Result:= ttGrassland;
    ttPrairie: Result:= ttPrairie;
    ttSavannah: Result:= ttSavannah;
    ttMarsh: Result:= ttMarsh;
    ttSwamp: Result:= ttSwamp;
    ttDesert: Result:= ttDesert;
    ttTundra: Result:= ttTundra;}
    ttBoreal: Result:= ttTundra;
    ttWetland: Result:= ttMarsh;
    ttScrubForest: Result:= ttDesert;
    ttBroadleaf: Result:= ttPrairie;
    ttMixedForest: Result:= ttPlains;
    ttConiferForest: Result:= ttGrassland;
    ttRainForest: Result:= ttSwamp;
    ttTropicalForest: Result:= ttSavannah;
    {ttHills: Result:= ttHills;
    ttMountains: Result:= ttMountains;}
  else
    Result:= m_Type;
  end;//case
end;//func

function TTerrain.HasForest: Boolean;
begin
  Result:= m_Type in [ttBoreal, ttWetland, ttScrubForest, ttBroadleaf, ttMixedForest, ttConiferForest,
                  ttRainForest, ttTropicalForest];
end;

function TTerrain.HasRiver: Boolean;
begin
  Result:= m_River;
end;

function TTerrain.HasRoad: Boolean;
begin
  Result:= m_Road;
end;

function TTerrain.HasSpecial: Boolean;
begin
  Result:= m_Special;
end;

function TTerrain.IsPloughed: Boolean;
begin
  Result:= m_Ploughed;
end;

function TTerrain.IsWater: Boolean;
begin
  Result:= m_Type in [ttSea, ttOpenSea];
end;

function TTerrain.GetGoodProduction(const AGood: TGoodType; const expert: Boolean): Byte;
begin
  Result:= 0;
  case m_Type of
    //ttArctic: Result:= 0;
    ttSea, ttOpenSea: if AGood= gtFood then
                      begin
                        Result:= 2;
                        if m_Special {fish} then Result:= 5;
                        if expert then Result:= Result*2;
                      end;//if
    ttPlains: if AGood = gtFood then
              begin
                Result:= 5;
                if m_Special {wheat} then
                  if expert then Result:= Result+4
                  else Result:= Result+2;
                if m_Ploughed then Result:= Result+1;
                if m_River then Result:= Result+1;
                if expert then Result:= Result+3;
              end
              else if AGood = gtCotton then
              begin
                Result:= 2;
                if m_Ploughed then Result:= Result+1;
                if m_River then Result:= Result+1;
                if expert then Result:= Result*2;
              end
              else if AGood = gtOre then
              begin
                Result:= Result +1;
                if m_Road then Result:= Result+1;
                if m_River then Result:= Result+1;
                if expert then Result:= Result*2;
              end;
    ttGrassland: if AGood = gtFood then
                 begin
                   Result:= 3;
                   if m_Ploughed then Result:= Result+1;
                   if m_River then Result:= Result+1;
                   if expert then Result:= Result+3;
                 end
                 else if AGood = gtTobacco then
                 begin
                   Result:= 3;
                   if m_Ploughed then Result:= Result+1;
                   if m_River then Result:= Result+1;
                   if m_Special {best tobacco} then Result:= Result*2;
                   if expert then Result:= Result*2;
                 end;
    ttPrairie: if AGood = gtFood then
               begin
                 Result:= 3;
                 if m_Ploughed then Result:= Result+1;
                 if m_River then Result:= Result+1;
                 if expert then Result:= Result+3;
               end
               else if AGood = gtCotton then
               begin
                 Result:= 3;
                 if m_Ploughed then Result:= Result+1;
                 if m_River then Result:= Result+1;
                 if m_Special {best cotton} then Result:= Result*2;
                 if expert then Result:= Result*2;
               end;
    ttSavannah: if AGood = gtFood then
                begin
                  Result:= 4;
                  if m_Ploughed then Result:= Result+1;
                  if m_River then Result:= Result+1;
                  if expert then Result:= Result+3;
                end
                else if AGood = gtSugar then
                begin
                  Result:= 3;
                  if m_Ploughed then Result:= Result+1;
                  if m_River then Result:= Result+1;
                  if m_Special {best sugar} then Result:= Result*2;
                  if expert then Result:= Result*2;
                end;
    ttMarsh: if AGood = gtFood then
             begin
               Result:= 3;
               if m_Ploughed then Result:= Result+1;
               if m_River then Result:= Result+1;
               if expert then Result:= Result+3;
             end
             else if AGood = gtTobacco then
             begin
               Result:= 2;
               if m_Ploughed then Result:= Result+1;
               if m_River then Result:= Result+1;
               if expert then Result:= Result*2;
             end
             else if AGood = gtOre then
             begin
               Result:= 2;
               if m_Road then Result:= Result+1;
               if m_River then Result:= Result+1;
               if m_Special {Minerals} then Result:= Result+3;
               if expert then Result:= Result*2;
             end;
    ttSwamp: if AGood = gtFood then
             begin
               Result:= 3;
               if m_Ploughed then Result:= Result+1;
               if m_River then Result:= Result+1;
               if expert then Result:= Result+3;
             end
             else if AGood = gtSugar then
             begin
               Result:= 2;
               if m_Ploughed then Result:= Result+1;
               if m_River then Result:= Result+1;
               if expert then Result:= Result*2;
             end
             else if AGood = gtOre then
             begin
               Result:= 2;
               if m_Road then Result:= Result+1;
               if m_River then Result:= Result+1;
               if m_Special {Minerals} then Result:= Result+3;
               if expert then Result:= Result*2;
             end;
    ttDesert: if AGood = gtFood then
              begin
                Result:= 2;
                if m_Ploughed then Result:= Result+1;
                if m_River then Result:= Result+1;
                if m_Special {Oasis} then Result:= Result+2;
                if expert then Result:= Result+3;
              end
              else if AGood = gtCotton then
              begin
                Result:= 1;
                if m_Ploughed then Result:= Result+1;
                if m_River then Result:= Result+1;
                if expert then Result:= Result*2;
              end
              else if AGood = gtOre then
              begin
                Result:= 2;
                if m_Road then Result:= Result+1;
                if m_River then Result:= Result+1;
                if expert then Result:= Result*2;
              end;
    ttTundra: if AGood = gtFood then
              begin
                Result:= 3;
                if m_Ploughed then Result:= Result+1;
                if m_River then Result:= Result+1;
                if expert then Result:= Result+3;
              end
              else if AGood = gtOre then
              begin
                Result:= 2;
                if m_Road then Result:= Result+1;
                if m_River then Result:= Result+1;
                if m_Special {Minerals} then Result:= Result+3;
                if expert then Result:= Result*2;
              end;
    ttBoreal: if AGood = gtFood then
              begin
                Result:= 2;
                if m_River then Result:= Result+1;
                //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                if m_Special {deer} then Result:= Result+2;
                if expert then Result:= Result+3;
              end
              else if AGood = gtFur then
              begin
                Result:= 3;
                if m_River then Result:= Result+1;
                if m_Road then Result:= Result+1;
                if m_Special {deer} then Result:= Result+2;
                if expert then Result:= Result*2;
              end
              else if AGood = gtWood then
              begin
                Result:= 4;
                if m_River then Result:= Result+1;
                if m_Road then Result:= Result+1;
                if expert then Result:= Result*2;
              end
              else if AGood = gtOre then
              begin
                Result:= 1;
                if m_River then Result:= Result+1;
                if m_Road then Result:= Result+1;
                if expert then Result:= Result*2;
              end;
    //"Feuchtwald"
    ttWetland: if AGood = gtFood then
               begin
                 Result:= 2;
                 if m_River then Result:= Result+1;
                 //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                 if expert then Result:= Result+3;
               end
               else if AGood = gtTobacco then
               begin
                 Result:= 1;
                 if m_River then Result:= Result+1;
                 //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                 if expert then Result:= Result*2;
               end
               else if AGood = gtFur then
               begin
                 Result:= 2;
                 if m_River then Result:= Result+2;//yes, it's two instead of one
                 if m_Road then Result:= Result+2;
                 if expert then Result:= Result*2;
               end
               else if AGood = gtWood then
               begin
                 Result:= 4;
                 if m_River then Result:= Result+2;//yes, it's two instead of one
                 if m_Road then Result:= Result+2;
                 if expert then Result:= Result*2;
               end
               else if AGood = gtOre then
               begin
                 Result:= 1;
                 if m_River then Result:= Result+1;
                 if m_Road then Result:= Result+1;
                 if m_Special {Minerals} then Result:= Result+3;
                 if expert then Result:= Result*2;
               end;
    ttScrubForest: if AGood = gtFood then
                   begin
                     Result:= 2;
                     if m_River then Result:= Result+1;
                     //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                     if m_Special {Oasis} then Result:= Result+2;
                     if expert then Result:= Result+3;
                   end
                   else if AGood = gtCotton then
                   begin
                     Result:= 1;
                     if m_River then Result:= Result+1;
                     //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                     if expert then Result:= Result*2;
                   end
                   else if AGood = gtFur then
                   begin
                     Result:= 2;
                     if m_River then Result:= Result+2;//yes, that's two
                     if m_Road then Result:= Result+2;
                     if expert then Result:= Result*2;
                   end
                   else if AGood = gtWood then
                   begin
                     Result:= 2;
                     if m_River then Result:= Result+2;//yes, that's two
                     if m_Road then Result:= Result+2;
                     if expert then Result:= Result*2;
                   end
                   else if AGood = gtOre then
                   begin
                     Result:= 1;
                     if m_River then Result:= Result+1;
                     if m_Road then Result:= Result+1;
                     if expert then Result:= Result*2;
                   end;
    ttBroadleaf: if AGood = gtFood then
                 begin
                   Result:= 2;
                   if m_River then Result:= Result+1;
                   //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                   if m_Special {deer} then
                     if expert then Result:= Result+4
                     else Result:= Result+2;
                   if expert then Result:= Result+3;
                 end
                 else if AGood = gtCotton then
                 begin
                   Result:= 1;
                   if m_River then Result:= Result+1;
                   if expert then Result:= Result*2;
                 end
                 else if AGood = gtFur then
                 begin
                   Result:= 2;
                   if m_River then Result:= Result+2;//yes, it's two, not one
                   if m_Road then Result:= Result+2;
                   if m_Special {deer} then Result:= Result+2;
                   if expert then Result:= Result*2;
                 end
                 else if AGood = gtWood then
                 begin
                   Result:= 4;
                   if m_River then Result:= Result+2;//yes, it's two, not one
                   if m_Road then Result:= Result+2;
                   if expert then Result:= Result*2;
                 end;
    ttMixedForest: if AGood = gtFood then
                   begin
                     Result:= 3;
                     if m_River then Result:= Result+1;
                     //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                     if expert then Result:= Result+3;
                   end
                   else if AGood = gtCotton then
                   begin
                     Result:= 1;
                     if m_River then Result:= Result+1;
                     if expert then Result:= Result*2;
                   end
                   else if AGood = gtFur then
                   begin
                     Result:= 3;
                     if m_River then Result:= Result+2;//yes, two :)
                     if m_Road then Result:= Result+2;
                     if m_Special {beaver} then Result:= Result +3;
                     if expert then Result:= Result*2;
                   end
                   else if AGood = gtWood then
                   begin
                     Result:= 6;
                     if m_River then Result:= Result+2;//yes, two :)
                     if m_Road then Result:= Result+2;
                     if expert then Result:= Result*2;
                   end;//if
    ttConiferForest: if AGood = gtFood then
                     begin
                       Result:= 2;
                       if m_River then Result:= Result+1;
                       //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                       if expert then Result:= Result+3;
                     end
                     else if AGood = gtTobacco then
                     begin
                       Result:= 1;
                       if m_River then Result:= Result+1;
                       if expert then Result:= Result*2;
                     end
                     else if AGood = gtFur then
                     begin
                       Result:= 2;
                       if m_River then Result:= Result+2; //yes, it's a +2
                       if m_Road then Result:= Result+2;
                       if expert then Result:= Result*2;
                     end
                     else if AGood = gtWood then
                     begin
                       Result:= 6;
                       if m_River then Result:= Result+2; //yes, it's a +2
                       if m_Road then Result:= Result+2;
                       if m_Special {best wood} then Result:= Result+4;
                       if expert then Result:= Result*2;
                     end;
    ttRainForest: if AGood = gtFood then
                  begin
                    Result:= 2;
                    if m_River then Result:= Result+1;
                    //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                    if expert then Result:= Result+3;
                  end
                  else if AGood = gtSugar then
                  begin
                    Result:= 1;
                    if m_River then Result:= Result+1;
                    if expert then Result:= Result*2;
                  end
                  else if AGood = gtFur then
                  begin
                    Result:= 1;
                    if m_River then Result:= Result+2;//a two here
                    if m_Road then Result:= Result+2;
                    if expert then Result:= Result*2;
                  end
                  else if AGood = gtWood then
                  begin
                    Result:= 4;
                    if m_River then Result:= Result+2;//a two here
                    if m_Road then Result:= Result+2;
                    if expert then Result:= Result*2;
                  end
                  else if AGood = gtOre then
                  begin
                    Result:= 1;
                    if m_River then Result:= Result+1;
                    if m_Road then Result:= Result+1;
                    if m_Special {Minerals} then Result:= Result+3;
                    if expert then Result:= Result*2;
                  end;
    ttTropicalForest: if AGood = gtFood then
                      begin
                        Result:= 3;
                        if m_River then Result:= Result+1;
                        //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                        if expert then Result:= Result+3;
                      end
                      else if AGood = gtSugar then
                      begin
                        Result:= 1;
                        if m_River then Result:= Result+1;
                        if expert then Result:= Result*2;
                      end
                      else if AGood = gtFur then
                      begin
                        Result:= 2;
                        if m_River then Result:= Result+2;//yes, it's two
                        if m_Road then Result:= Result+2;
                        if expert then Result:= Result*2;
                      end
                      else if AGood = gtWood then
                      begin
                        Result:= 4;
                        if m_River then Result:= Result+2;//yes, it's two
                        if m_Road then Result:= Result+2;
                        if m_Special {best wood} then Result:= Result+4;
                        if expert then Result:= Result*2;
                      end;
    ttHills: if AGood = gtFood then
             begin
               Result:= 2;
               if m_River then Result:= Result+1;
               //if m_Ploughed then Result:= Result+1; //hills cannot be ploughed
               if expert then Result:= Result+3;
             end
             else if AGood = gtOre then
             begin
               Result:= 4;
               if m_River then Result:= Result+1;
               if m_Road then Result:= Result+1;
               if m_Special {Ore} then Result:= Result+2;
               if expert then Result:= Result*2;
             end;
    ttMountains: if AGood = gtOre then
                 begin
                   Result:= 4;
                   if m_River then Result:= Result+1;
                   if m_Road then Result:= Result+1;
                   if expert then Result:= Result*2;
                 end
                 else if AGood = gtSilver then
                 begin
                   Result:= 1;
                   if m_River then Result:= Result+1;
                   if m_Road then Result:= Result+1;
                   if m_Special {Silver} then Result:= Result+4;
                   if expert then Result:= Result*2;
                 end;
  end;//case
end;//func

//defense bonus for terrain in %. Maximum is 150%.
function TTerrain.GetDefenceBonus: Byte;
begin
  case m_Type of
    ttMarsh, ttSwamp: Result:= 25;
    ttBoreal, ttWetland, ttScrubForest, ttBroadleaf, ttMixedForest, ttConiferForest: Result:= 50;
    ttRainForest: Result:= 75;
    ttTropicalForest: Result:= 50;
    ttHills: Result:= 100;
    ttMountains: Result:= 150;
  else Result:= 0;
  end;//case
end;//func

//**** Terrain alteration functions ****

procedure TTerrain.ClearForest;
begin
  m_Type:= self.ClearedBecomes;
end;

procedure TTerrain.CreateRoad;
begin
  m_Road:= True;
end;

procedure TTerrain.Plough;
begin
  //can't plough in forest, has to deforest first
  if HasForest then ClearForest
  //can't plough in hills or mountains
  else if (not (m_Type in [ttHills, ttMountains])) then m_Ploughed:= True;
end;

procedure TTerrain.CreateSpecial;
begin
  //can't have special in high sea
  if m_Type<>ttOpenSea then
    m_Special:= true;
end;

//functions for colony base field
function TTerrain.GetColonyFood: Byte;
begin
  case m_Type of
    ttArctic: Result:= 2;
    ttPlains, ttGrassland, ttPrairie, ttSavannah, ttMarsh, ttSwamp, ttTundra: Result:= 5;
    ttDesert, ttScrubForest: Result:= 3;
    ttBoreal, ttWetland: Result:= 4;
    ttBroadleaf, ttMixedForest, ttConiferForest, ttRainForest, ttTropicalForest, ttHills: Result:= 4;
  else
    Result:= 0;//ttSea, ttOpenSea, ttMountains
  end;//case
end;//func

function TTerrain.GetColonyGoodType: TGoodType;
begin
  case m_Type of
    ttPlains, ttPrairie: Result:= gtCotton;
    ttGrassland, ttMarsh: Result:= gtTobacco;
    ttSavannah, ttSwamp, ttRainForest: Result:= gtSugar;
    ttDesert, ttTundra, ttHills: Result:= gtOre;
    ttBoreal, ttWetland, ttScrubForest, ttBroadleaf, ttMixedForest, ttConiferForest, ttTropicalForest: Result:= gtFur;
  else
    Result:= gtFood; //ttArctic, ttSea, ttOpenSea, ttMountains
  end;//case
end;//func

function TTerrain.GetColonyGoodAmount: Byte;
begin
  case m_Type of
    ttPlains, ttMarsh, ttSwamp, ttDesert, ttTundra, ttWetland, ttScrubForest, ttBroadleaf: Result:= 3;
    ttGrassland, ttPrairie, ttSavannah, ttBoreal, ttMixedForest: Result:= 4;
    ttConiferForest, ttTropicalForest: Result:= 3;
    ttRainForest: Result:= 2;
    ttHills: Result:= 5;
  else
    Result:= 0;//ttArctic, ttSea, ttOpenSea, ttMountains
  end;//case
end;//func

end.