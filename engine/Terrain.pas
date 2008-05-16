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
  TTerrainType = (//open terrain types
                  ttArctic, ttSea, ttOpenSea, ttPlains, ttGrassland, ttPrairie,
                  ttSavannah, ttMarsh, ttSwamp, ttDesert, ttTundra,
                  //forested terrain types
                  ttBoreal, ttWetland, ttScrubForest, ttBroadleaf, ttMixedForest,
                  ttConiferForest, ttRainForest, ttTropicalForest,
                  //others
                  ttHills, ttMountains);
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
      m_Type: TTerrainType;

      constructor Create(const ATerrain: TTerrainType; const River: Boolean=False;
                         const Road: Boolean=False; const Special: Boolean=False;
                         const Ploughed: Boolean=False);
      destructor Destroy;

      function ClearedBecomes: TTerrainType;
      function HasForest: Boolean;
      function HasRiver: Boolean;
      function HasRoad: Boolean;
      function HasSpecial: Boolean;
      function IsPloughed: Boolean;
      function IsWater: Boolean;

      function GetGoodProduction(const AGood: TGoodType): Byte;
      //defense bonus for terrain, in percent. Maximum is 150, so Byte will do.
      function GetDefenceBonus: Byte;

      //change terrain state
      procedure ClearForest;
      procedure CreateRoad;
      procedure Plough;
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

function TTerrain.GetGoodProduction(const AGood: TGoodType): Byte;
begin
  Result:= 0;
  case m_Type of
    //ttArctic: Result:= 0;
    ttSea, ttOpenSea: if AGood= gtFood then
                      begin
                        Result:= 4;
                        if m_Special {fish} then Result:= 7;
                      end;//if
    ttPlains: if AGood = gtFood then
              begin
                Result:= 5;
                if m_Special {wheat} then Result:= Result+2;
                if m_Ploughed then Result:= Result+1;
                if m_River then Result:= Result+1;
              end
              else if AGood = gtCotton then
              begin
                Result:= 2;
                if m_Ploughed then Result:= Result+1;
                if m_River then Result:= Result+1;
              end
              else if AGood = gtOre then
              begin
                Result:= Result +1;
                if m_Road then Result:= Result+1;
                if m_River then Result:= Result+1;
              end;
    ttGrassland: if AGood = gtFood then
                 begin
                   Result:= 3;
                   if m_Ploughed then Result:= Result+1;
                   if m_River then Result:= Result+1;
                 end
                 else if AGood = gtTobacco then
                 begin
                   Result:= 3;
                   if m_Ploughed then Result:= Result+1;
                   if m_River then Result:= Result+1;
                   if m_Special {best tobacco} then Result:= Result*2;
                 end;
    ttPrairie: if AGood = gtFood then
               begin
                 Result:= 3;
                 if m_Ploughed then Result:= Result+1;
                 if m_River then Result:= Result+1;
               end
               else if AGood = gtCotton then
               begin
                 Result:= 3;
                 if m_Ploughed then Result:= Result+1;
                 if m_River then Result:= Result+1;
                 if m_Special {best cotton} then Result:= Result*2;
               end;
    ttSavannah: if AGood = gtFood then
                begin
                  Result:= 4;
                  if m_Ploughed then Result:= Result+1;
                  if m_River then Result:= Result+1;
                end
                else if AGood = gtSugar then
                begin
                  Result:= 3;
                  if m_Ploughed then Result:= Result+1;
                  if m_River then Result:= Result+1;
                  if m_Special {best sugar} then Result:= Result*2;
                end;
    ttMarsh: if AGood = gtFood then
             begin
               Result:= 3;
               if m_Ploughed then Result:= Result+1;
               if m_River then Result:= Result+1;
             end
             else if AGood = gtTobacco then
             begin
               Result:= 2;
               if m_Ploughed then Result:= Result+1;
               if m_River then Result:= Result+1;
             end
             else if AGood = gtOre then
             begin
               Result:= 2;
               if m_Road then Result:= Result+1;
               if m_River then Result:= Result+1;
               if m_Special {Minerals} then Result:= Result+3;
             end;
    ttSwamp: if AGood = gtFood then
             begin
               Result:= 3;
               if m_Ploughed then Result:= Result+1;
               if m_River then Result:= Result+1;
             end
             else if AGood = gtSugar then
             begin
               Result:= 2;
               if m_Ploughed then Result:= Result+1;
               if m_River then Result:= Result+1;
             end
             else if AGood = gtOre then
             begin
               Result:= 2;
               if m_Road then Result:= Result+1;
               if m_River then Result:= Result+1;
               if m_Special {Minerals} then Result:= Result+3;
             end;
    ttDesert: if AGood = gtFood then
              begin
                Result:= 2;
                if m_Ploughed then Result:= Result+1;
                if m_River then Result:= Result+1;
                if m_Special {Oasis} then Result:= Result+2;
              end
              else if AGood = gtCotton then
              begin
                Result:= 1;
                if m_Ploughed then Result:= Result+1;
                if m_River then Result:= Result+1;
              end
              else if AGood = gtOre then
              begin
                Result:= 2;
                if m_Road then Result:= Result+1;
                if m_River then Result:= Result+1;
              end;
    ttTundra: if AGood = gtFood then
              begin
                Result:= 3;
                if m_Ploughed then Result:= Result+1;
                if m_River then Result:= Result+1;
              end
              else if AGood = gtOre then
              begin
                Result:= 2;
                if m_Road then Result:= Result+1;
                if m_River then Result:= Result+1;
                if m_Special {Minerals} then Result:= Result+3;
              end;
    ttBoreal: if AGood = gtFood then
              begin
                Result:= 2;
                if m_River then Result:= Result+1;
                //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                if m_Special {deer} then Result:= Result+2;
              end
              else if AGood = gtFur then
              begin
                Result:= 3;
                if m_River then Result:= Result+1;
                if m_Road then Result:= Result+1;
                if m_Special {deer} then Result:= Result+2;
              end
              else if AGood = gtWood then
              begin
                Result:= 4;
                if m_River then Result:= Result+1;
                if m_Road then Result:= Result+1;
              end
              else if AGood = gtOre then
              begin
                Result:= 1;
                if m_River then Result:= Result+1;
                if m_Road then Result:= Result+1;
              end;
    //"Feuchtwald"
    ttWetland: if AGood = gtFood then
               begin
                 Result:= 2;
                 if m_River then Result:= Result+1;
                 //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
               end
               else if AGood = gtTobacco then
               begin
                 Result:= 1;
                 if m_River then Result:= Result+1;
                 //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
               end
               else if AGood = gtFur then
               begin
                 Result:= 2;
                 if m_River then Result:= Result+2;//yes, it's two instead of one
                 if m_Road then Result:= Result+2;
               end
               else if AGood = gtWood then
               begin
                 Result:= 4;
                 if m_River then Result:= Result+2;//yes, it's two instead of one
                 if m_Road then Result:= Result+2;
               end
               else if AGood = gtOre then
               begin
                 Result:= 1;
                 if m_River then Result:= Result+1;
                 if m_Road then Result:= Result+1;
                 if m_Special {Minerals} then Result:= Result+3;
               end;
    ttScrubForest: if AGood = gtFood then
                   begin
                     Result:= 2;
                     if m_River then Result:= Result+1;
                     //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                     if m_Special {Oasis} then Result:= Result+2;
                   end
                   else if AGood = gtCotton then
                   begin
                     Result:= 1;
                     if m_River then Result:= Result+1;
                     //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                   end
                   else if AGood = gtFur then
                   begin
                     Result:= 2;
                     if m_River then Result:= Result+2;//yes, that's two
                     if m_Road then Result:= Result+2;
                   end
                   else if AGood = gtWood then
                   begin
                     Result:= 2;
                     if m_River then Result:= Result+2;//yes, that's two
                     if m_Road then Result:= Result+2;
                   end
                   else if AGood = gtOre then
                   begin
                     Result:= 1;
                     if m_River then Result:= Result+1;
                     if m_Road then Result:= Result+1;
                   end;
    ttBroadleaf: if AGood = gtFood then
                 begin
                   Result:= 2;
                   if m_River then Result:= Result+1;
                   //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                   if m_Special {deer} then Result:= Result+2;
                 end
                 else if AGood = gtCotton then
                 begin
                   Result:= 1;
                   if m_River then Result:= Result+1;
                   //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                 end
                 else if AGood = gtFur then
                 begin
                   Result:= 2;
                   if m_River then Result:= Result+2;//yes, it's two, not one
                   if m_Road then Result:= Result+2;
                   if m_Special {deer} then Result:= Result+2;
                 end
                 else if AGood = gtWood then
                 begin
                   Result:= 4;
                   if m_River then Result:= Result+2;//yes, it's two, not one
                   if m_Road then Result:= Result+2;
                 end;
    ttMixedForest: if AGood = gtFood then
                   begin
                     Result:= 3;
                     if m_River then Result:= Result+1;
                     //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                   end
                   else if AGood = gtCotton then
                   begin
                     Result:= 1;
                     if m_River then Result:= Result+1;
                     //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                   end
                   else if AGood = gtFur then
                   begin
                     Result:= 3;
                     if m_River then Result:= Result+2;//yes, two :)
                     if m_Road then Result:= Result+2;
                     if m_Special {beaver} then Result:= Result +3;
                   end
                   else if AGood = gtWood then
                   begin
                     Result:= 6;
                     if m_River then Result:= Result+2;//yes, two :)
                     if m_Road then Result:= Result+2;
                   end;//if
    ttConiferForest: if AGood = gtFood then
                     begin
                       Result:= 2;
                       if m_River then Result:= Result+1;
                       //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                     end
                     else if AGood = gtTobacco then
                     begin
                       Result:= 1;
                       if m_River then Result:= Result+1;
                       //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                     end
                     else if AGood = gtFur then
                     begin
                       Result:= 2;
                       if m_River then Result:= Result+2; //yes, it's a +2
                       if m_Road then Result:= Result+2;
                     end
                     else if AGood = gtWood then
                     begin
                       Result:= 6;
                       if m_River then Result:= Result+2; //yes, it's a +2
                       if m_Road then Result:= Result+2;
                       if m_Special {best wood} then Result:= Result+4;
                     end;
    ttRainForest: if AGood = gtFood then
                  begin
                    Result:= 2;
                    if m_River then Result:= Result+1;
                    //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                  end
                  else if AGood = gtSugar then
                  begin
                    Result:= 1;
                    if m_River then Result:= Result+1;
                    //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                  end
                  else if AGood = gtFur then
                  begin
                    Result:= 1;
                    if m_River then Result:= Result+2;//a two here
                    if m_Road then Result:= Result+2;
                  end
                  else if AGood = gtWood then
                  begin
                    Result:= 4;
                    if m_River then Result:= Result+2;//a two here
                    if m_Road then Result:= Result+2;
                  end
                  else if AGood = gtOre then
                  begin
                    Result:= 1;
                    if m_River then Result:= Result+1;
                    if m_Road then Result:= Result+1;
                    if m_Special {Minerals} then Result:= Result+3;
                  end;
    ttTropicalForest: if AGood = gtFood then
                      begin
                        Result:= 3;
                        if m_River then Result:= Result+1;
                        //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                      end
                      else if AGood = gtSugar then
                      begin
                        Result:= 1;
                        if m_River then Result:= Result+1;
                        //if m_Ploughed then Result:= Result+1; //forest cannot be ploughed
                      end
                      else if AGood = gtFur then
                      begin
                        Result:= 2;
                        if m_River then Result:= Result+2;//yes, it's two
                        if m_Road then Result:= Result+2;
                      end
                      else if AGood = gtWood then
                      begin
                        Result:= 4;
                        if m_River then Result:= Result+2;//yes, it's two
                        if m_Road then Result:= Result+2;
                        if m_Special {best wood} then Result:= Result+4;
                      end;
    ttHills: if AGood = gtFood then
             begin
               Result:= 2;
               if m_River then Result:= Result+1;
               //if m_Ploughed then Result:= Result+1; //hills cannot be ploughed
             end
             else if AGood = gtOre then
             begin
               Result:= 4;
               if m_River then Result:= Result+1;
               if m_Road then Result:= Result+1;
               if m_Special {Ore} then Result:= Result+2;
             end;
    ttMountains: if AGood = gtOre then
                 begin
                   Result:= 4;
                   if m_River then Result:= Result+1;
                   if m_Road then Result:= Result+1;
                 end
                 else if AGood = gtSilver then
                 begin
                   Result:= 1;
                   if m_River then Result:= Result+1;
                   if m_Road then Result:= Result+1;
                   if m_Special {Silver} then Result:= Result+4;
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

end.