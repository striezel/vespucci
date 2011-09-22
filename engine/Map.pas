{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2009, 2010, 2011  Thoronador

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

unit Map;

interface

uses
  Terrain, Nation, Classes{for TFileStream}, SysUtils;

const
  //general map size
  cMap_X = 56;
  cMap_Y = 70;

  //const for loading/ saving map
  cMapFileHeader = 'VMD';

  //const for rivers
  { These constants are used during visualisation of the rivers on the map
    to interact properly with rivers in adjacent fields.
  }
  cMapRiverNone = 0;
  cMapRiverNorth = 1;
  cMapRiverEast = 2;
  cMapRiverSouth = 4;
  cMapRiverWest = 8;

  //combined river consts
  cMapRiverNE = cMapRiverNorth or cMapRiverEast;
  cMapRiverSE = cMapRiverSouth or cMapRiverEast;
  cMapRiverSW = cMapRiverSouth or cMapRiverWest;
  cMapRiverNW = cMapRiverNorth or cMapRiverWest;

  cMapRiverNS = cMapRiverNorth or cMapRiverSouth;
  cMapRiverEW = cMapRiverEast or cMapRiverWest;

  cMapRiverNotN = cMapRiverWest or cMapRiverEast or cMapRiverSouth;
  cMapRiverNotE = cMapRiverWest or cMapRiverNorth or cMapRiverSouth;
  cMapRiverNotS = cMapRiverWest or cMapRiverEast or cMapRiverNorth;
  cMapRiverNotW = cMapRiverNorth or cMapRiverEast or cMapRiverSouth;

  cMapRiverAll = cMapRiverNorth or cMapRiverEast or cMapRiverSouth
                  or cMapRiverWest;

type
  { function type for landscape height generation

    parameters:
        mx, my - location of peak in map coordinates
        r      - radius of hill, distance to zero
        h      - height of peak
        x, y   - current field coordinates

  }
  THillFunction = function(const mx, my, r: Byte; const h: Single; const x, y: Byte): Single;

  { enumeration type for climate setting in map generation function }
  TClimateType = (ctDry, ctNormal, ctWet);

  { enumeration type for temperature setting in map generation function }
  TTemperatureType = (ttCool, ttModerate, ttWarm);

  { ********
    **** TMap class
    ****
    **** purpose: holds the map data for the current game.
    *******

    To Do:
    ======
       - save discovery status info (array discovered) during SaveToFile() and
         load it during LoadFromFile() function
       - write a better function for generation of maps
       - two-parameter version of TMap.Generate():
           - generate rivers
           - generate broader variety of terrain types
  }
  TMap = class
    private
      filled: Boolean; //internal value to determine whether map is not only nil
      { holds information about which fields of the map have already been
        discovered by the four European nations. }
      discovered: array [0..cMap_X-1, 0..cMap_Y-1] of array [cMinEuropean..cMaxEuropean] of Boolean;
      { "River Cache": holds information about rivers in adjacent fields of all
         fields on the map }
      river: array [0..cMap_X-1, 0..cMap_Y-1] of Byte;
      { generates the river cache, i.e. fills the above array with proper values }
      procedure GenerateRiverCache;
      { clears the "river cache", i.e. sets all fields to cMapRiverNone }
      procedure ClearRiverCache;
      { determines the river cache entry for a single square on the map

        parameters:
            x, y - coordinates of that square
      }
      procedure SetRiverType(const x,y: Byte);
    public
      { holds information about all squares/fields on the map

        remarks:
            This array should be private and only accessible via public
            functions. However, I decided to make it public for faster access,
            because this array will usually be used during every frame (or most
            frames) while a game is running.
      }
      tiles: array [0..cMap_X-1, 0..cMap_Y-1] of TTerrain;

      { constructor }
      constructor Create;

      { destructor }
      destructor Destroy; override;

      { generates a map with the given parameters

        parameters:
            Landmass - the amount of land on the map - 1.0 means all is land,
                       while 0.0 means no land and just water. Usually this
                       value should be between 0.5 and 0.8, I guess.

        remarks:
            Although this function already generates a map, it does not do it
            too well. It just generates a row of arctic terrain at the top and
            bottom of the map, a column of open sea at the left and right side,
            and all other land is either grassland or sea.
            It's quite clear that this function has to be improved in the
            future, if we really want to use it. Things that could be done are
            considering terrain height to generate mountains and hills, too;
            generation of rivers, consideration of climate (given by a new
            function parameter) to generate different terrain types, and a way
            to make the generated map look more like a continent (or a group of
            islands) and not just like randomly thrown squares of land.
      }
      procedure Generate(const Landmass: Single); overload;

      { generates a map with the given parameters

        parameters:
            Landmass    - the amount of land on the map - 1.0 means all is land,
                          while 0.0 means no land and just water. Usually this
                          value should be between 0.3 and 0.8, I guess.
            f           - the function that is used to generate hills/ mountains
            temperature - the temperate setting used to generate terrain tiles
                          (This setting is ignored in the current implementation.)
            climate     - the climate setting used to generate terrain tiles

        remarks:
            Although this function already generates a map, it does not do it
            too well. It just generates a row of arctic terrain at the top and
            bottom of the map, a column of open sea at the left and right side,
            and all other land is a more or less randomly chosen type of
            terrain. Function f is used to generate the height map. The climate
            setting influences the number of rivers generated, the temperature
            setting is currently ignored.
            This is a try to implement a slightly better version of the map
            generation function.
      }
      procedure Generate(const LandMass: Single; const f: THillFunction; const temperature: TTemperatureType; const climate: TClimateType); overload;

      { generates the special ressources for the map

        parameters:
            LandOnly - boolean that indicates whether special ressources should
                       only be generated for land squares (true) or also for
                       watery squares (false)

        remarks:
            Only call this once for a genarates map. For maps loaded from a
            file, never ever call that function, because loaded maps already
            have their special ressources set.
      }
      procedure GenerateSpecials(const LandOnly: Boolean=True);

      { tries to save the map to the given file and returns true on success

        parameters:
            FileName - name of the destionation file
      }
      function SaveToFile(const FileName: string): Boolean;

      { tries to load a map from the given file and returns true on success

        parameters:
            FileName - name of the source file
      }
      function LoadFromFile(const FileName: string): Boolean;

      { returns the river type of a certain square

        parameters:
            x, y - coordinates of the square
      }
      function GetRiverType(const x,y: Byte): Byte;

      { determines, whether a water tile has at least one non-water neighbour
        and returns true in that case

        parameters:
            x, y - coordinates of the square
      }
      function IsTouchingLand(const x, y: Byte): Boolean;

      { determines, whether a land tile has at least one water neighbour and
        returns true in that case

        parameters:
            x, y - coordinates of the square
      }
      function IsAdjacentToWater(const x,y: Byte): Boolean;

      { determines, whether a land tile has at least one hill/mountain neighbour
        square and returns true in that case

        parameters:
            x, y - coordinates of the square
      }
      function IsAdjacentToMountains(const x,y: Byte): Boolean;

      { determines, whether a land tile has at least one neighbour square with
        forest and returns true in that case

        parameters:
            x, y - coordinates of the square
      }
      function IsAdjacentToForest(const x,y: Byte): Boolean;

      { this procedure reaveals surrounding tiles around a unit (or that is what
        it's made for)

        parameters:
            x, y       - coordinates of unit's position on the map
            cNation    - integer constant identifying the unit's nation
            twoSquares - usually, this function only reveals the squares
                         directly adjacent to the unit's position, i.e. at
                         maximum nine squares (including the unit's position).
                         However, if twoSquares is true, this function will also
                         reveal the squares adjacent to that, revealing each
                         field that is within two (and not one) square range of
                         the unit. Usually only scout units do that.
      }
      procedure DiscoverSurroundingTiles(const x,y: Byte; const cNation: Byte; const two_squares: Boolean);

      { This procedure reaveals the complete map for the given nation. Yes,
        it's a cheat.

        parameters:
            num_Nation - integer constant identifying the nation in question
      }
      procedure RevealAll(const num_Nation: Integer);

      { returns true, if a certain European nation has discovered a certain
        field of the map

        parameters:
            x, y      - coordinates of the field's position
            numNation - integer constant identifying the nation in question
      }
      function IsDiscovered(const x,y: Byte; const num_Nation: Integer): Boolean;

      { returns true, if the passed position is a valid position on the map

        parameters:
            x, y - coordinates of the field's position
      }
      function IsValidMapPosition(const x,y: Integer): Boolean;
  end;//class

  //functions for height map generation
  { h2 generates a hill at (mx,my) with radius r and not so heavy slope }
  function h2(const mx, my, r: Byte; const h: Single; const x, y: Byte): Single;

  { h4 generates a hill at (mx,my) with radius r and faster slope (i.e. less
    hills and mountains, more grassland than h2) }
  function h4(const mx, my, r: Byte; const h: Single; const x, y: Byte): Single;

implementation

function h2(const mx, my, r: Byte; const h: Single; const x, y: Byte): Single;
var dist: Single;
begin
  dist:= sqrt(sqr(mx-x)+sqr(my-y))/r;
  Result:= h*(1- sqrt(dist));
end;//func

function h4(const mx, my, r: Byte; const h: Single; const x, y: Byte): Single;
var dist: Single;
begin
  dist:= sqrt(sqr(mx-x)+sqr(my-y))/r;
  Result:= h*(1- sqrt(sqrt(dist)));
end;//func

constructor TMap.Create;
var i,j, k: Integer;
begin
  inherited Create;
  filled:= False;
  //initialize map tiles
  for i:= 0 to cMap_X-1 do
    for j:= 0 to cMap_Y-1 do
    begin
      for k:= cMinEuropean to cMaxEuropean do
        discovered[i,j,k] := False;
      tiles[i,j]:= nil;
      river[i,j]:= cMapRiverNone;
    end;//for
end;//construc

destructor TMap.Destroy;
var i, j: Byte;
begin
  for i:=0 to cMap_X-1 do
    for j:=0 to cMap_Y-1 do
      if tiles[i,j]<>nil then tiles[i,j].Free;
  inherited Destroy;
end;//destruc

procedure TMap.Generate(const Landmass: Single);
var i,j: Integer;
begin
  //highest and lowest row are arctic
  for i:=1 to cMap_X-2 do
  begin
    tiles[i,0]:= TTerrain.Create(ttArctic);
    tiles[i,cMap_Y-1]:= TTerrain.Create(ttArctic);
  end;//for
  //first and last column are open sea (as in: goes to Europe)
  for i:=0 to cMap_Y-1 do
  begin
    tiles[0,i]:= TTerrain.Create(ttOpenSea);
    tiles[cMap_X-1, i]:= TTerrain.Create(ttOpenSea);
  end;//for

  //fill inner map
  Randomize;
  for i:= 1 to cMap_X-2 do
    for j:= 1 to cMap_Y-2 do
      if (Random <=Landmass) then
        tiles[i,j]:= TTerrain.Create(ttGrassland)
      else
        tiles[i,j]:= TTerrain.Create(ttSea);

  //smoothen landmass
  for i:=2 to cMap_X-3 do
    for j:=2 to cMap_Y-3 do
    begin
      if ((tiles[i-1,j].m_Type=ttGrassland) and (tiles[i,j-1].m_Type=ttGrassland) and
          (tiles[i+1,j].m_Type=ttGrassland) and (tiles[i,j+1].m_Type=ttGrassland)) then
          tiles[i,j].m_Type:= ttGrassland;
    end;//for

  //no rivers yet
  ClearRiverCache;

  //set the flag to indicate that we have data for all tiles
  filled:= True;
end;//proc

function TerrainProbability(const h1, h2, y: Integer): Single;
begin
  if y<h1 then Result:= 1.0
  else if y>h2 then Result:= 0.0
  else Result:= -0.9/(h2-h1) * y +0.95+0.9*h1/(h2-h1);
end;//function

function TerrainByH(const h: Integer): TTerrainType;
const cTundraStart = 1;
      cBorealStart = 7;
      cLeafStart = 14;
      cPrairieStart = 21;
      cSavannahStart = 28;
      cTropicalCenter = (cMap_Y div 2);
      cSavannahEnd = cMap_Y -28;
      cPrairieEnd = cMap_Y-21;
      cLeafEnd = cMap_Y-14;
      cBorealEnd = cMap_Y-7;
      cTundraEnd = cMap_Y-1;
begin
  case h of
    cTundraStart..cBorealStart-1: if Random < TerrainProbability(cTundraStart, cBorealStart, h) then Result:= ttTundra
                                  else Result:= ttBoreal;
    cBorealStart..cLeafStart-1: if Random < TerrainProbability(cBorealStart, cLeafStart, h) then Result:= ttBoreal
                                else Result:= ttBroadleaf;
    cLeafStart..cPrairieStart-1: if Random < TerrainProbability(cLeafStart, cPrairieStart, h) then Result:= ttBroadleaf
                                 else Result:= ttPrairie;
    cPrairieStart..cSavannahStart-1: if Random < TerrainProbability(cPrairieStart, cSavannahStart, h) then Result:= ttPrairie
                                  else Result:= ttSavannah;
    cSavannahStart..cTropicalCenter-1: if Random < TerrainProbability(cSavannahStart, cTropicalCenter, h) then Result:= ttSavannah
                                  else Result:= ttRainForest;
    cTropicalCenter: Result:= ttTropicalForest;
    cTropicalCenter+1..cSavannahEnd: if Random < TerrainProbability(cTropicalCenter, cSavannahEnd, h) then Result:= ttRainForest
                                     else Result:= ttSavannah;
    cSavannahEnd+1..cPrairieEnd: if Random < TerrainProbability(cSavannahEnd, cPrairieEnd, h) then Result:= ttSavannah
                                 else Result:= ttPrairie;
    cPrairieEnd+1..cLeafEnd: if Random < TerrainProbability(cPrairieEnd, cLeafEnd, h) then Result:= ttPrairie
                             else Result:= ttBroadleaf;
    cLeafEnd+1..cBorealEnd: if Random < TerrainProbability(cLeafEnd, cBorealEnd, h) then Result:= ttBroadleaf
                            else Result:= ttConiferForest;
    cBorealEnd+1..cTundraEnd: if Random < TerrainProbability(cBorealEnd, cTundraEnd, h) then Result:= ttConiferForest
                            else Result:= ttTundra;
  else
    //should not normally happen
    Result:= ttArctic;
  end;//case
end;//func

procedure TMap.Generate(const LandMass: Single; const f: THillFunction; const temperature: TTemperatureType; const climate: TClimateType);
var i,j, river_count: Integer;
    heightmap: array [0..cMap_X-1, 0..cMap_Y-1] of Single;
    rivermap: array [0..cMap_X-1, 0..cMap_Y-1] of Boolean;
    h_x, h_y, radius, new_x, new_y: Byte;
    currentLandmass: Cardinal;
    count: Cardinal;
begin
  if (LandMass>=0.86) then
  begin
    WriteLn('Invalid parameter value for LandMass in TMap.Generate!');
    WriteLn('Using generation function with default parameter instead.');
    Generate(0.85, f, temperature, climate);
    Exit;
  end;

  //highest and lowest row are arctic
  for i:=1 to cMap_X-2 do
  begin
    tiles[i,0]:= TTerrain.Create(ttArctic);
    tiles[i,cMap_Y-1]:= TTerrain.Create(ttArctic);
  end;//for
  //first and last column are open sea (as in: goes to Europe)
  for i:=0 to cMap_Y-1 do
  begin
    tiles[0,i]:= TTerrain.Create(ttOpenSea);
    tiles[cMap_X-1, i]:= TTerrain.Create(ttOpenSea);
  end;//for

  // ---- generation of height map ----
  //first fill it with default value
  for i:=0 to cMap_X-1 do
    for j:=0 to cMap_Y-1 do
      heightmap[i,j]:= -2.0; //randomly chosen

  Randomize;
  currentLandMass:= 0;
  count:= 0;

  while (currentLandMass/((cMap_X-1)*(cMap_Y-1))<LandMass) do
  begin
    count:= count +1;
    WriteLn('Info: Generation cycle count: ', count, 'Land: ', currentLandMass);
    //now set the hill's radius
    radius:= 3+Random(7);
    //...and its location
    { Check for x-position is neccessary, because we don't want half an island
      popping in at the eastern or western end of the map.}
    repeat
      h_x:= Random(cMap_X);
    until (h_x>radius) and (h_x<cMap_X-radius);

    { For the y-position: Too much checks will lead to isolated artic lines
      at the bottom and top of the map. Too few checks will lead to no
      (northern or southern) east-west passage to the "Pacific Ocean", because
      it will be blocked by land.

      To get out of this dilemma (sort of, at least we try to get out), we only
      check randomly.
    }
    h_y:= Random(cMap_Y);
    if ((h_y<=radius) and (h_y>=cMap_Y-radius) and (Random<0.33)) then
    begin
      repeat
        h_y:= Random(cMap_Y);
      until (h_y>radius) and (h_y<cMap_Y-radius);
    end;//if

    WriteLn('    x: ', h_x, ', y: ', h_y, ', r: ',radius);
    for i:= h_x-radius to h_x+radius do
      for j:= h_y-radius to h_y+radius do
      begin
        if (IsValidMapPosition(i,j)) then
          if (heightmap[i,j]<0.0) then
          begin
            heightmap[i,j]:= f(h_x, h_y, radius, 2.5, i, j);
            if (heightmap[i,j]>0.0) then currentLandMass:= currentLandMass +1;
          end;//if
      end;//for
  end;//while

  // ---- generation of rivers ----
  //fill with false (i.e. no river)
  for i:=0 to cMap_X-1 do
    for j:=0 to cMap_Y-1 do
      rivermap[i,j]:= false;

  //generate one river for every 3% of landmasson average, but still seems quite
  // dry there
  case climate of
    ctDry:    river_count:= Trunc(25*LandMass);
    ctNormal: river_count:= Trunc(33*LandMass);
    ctWet:    river_count:= Trunc(50*LandMass);
  else
    river_count:= 0; //should never happen
  end;//case
  for i:= 1 to river_count do
  begin
    //get a location that is land and has no river yet
    repeat
      h_x:= 3+Random(cMap_X-6);
      h_y:= 3+Random(cMap_Y-6);
    until (heightmap[h_x, h_y]>0.0) and  not rivermap[h_x, h_y];

    repeat
      rivermap[h_x, h_y]:= True;
      new_x:= h_x;
      new_y:= h_y;
      //search for lowest adjacent tile (only horizontal or vertical)
      if (IsValidMapPosition(h_x-1, h_y)) then
        if heightmap[h_x-1, h_y]<heightmap[h_x, h_y] then
        begin
          new_x:= h_x-1;
          new_y:= h_y;
        end;
      if (IsValidMapPosition(h_x+1, h_y)) then
        if heightmap[h_x+1, h_y]<heightmap[new_x, new_y] then
        begin
          new_x:= h_x+1;
          new_y:= h_y;
        end;
      if (IsValidMapPosition(h_x, h_y-1)) then
        if heightmap[h_x, h_y-1]<heightmap[new_x, new_y] then
        begin
          new_x:= h_x;
          new_y:= h_y-1;
        end;
      if (IsValidMapPosition(h_x, h_y+1)) then
        if heightmap[h_x, h_y+1]<heightmap[new_x, new_y] then
        begin
          new_x:= h_x;
          new_y:= h_y+1;
        end;
      //Now (new_x,new_y) should be coordinates of lowest adjecent tile. If
      // (new_x,new_y) equals (h_x,h_y), then we are already at the lowest.
      //In that case, just create a lake there by making that tile below zero.
      if (new_x=h_x) and (new_y=h_y) then
      begin
        heightmap[h_x,h_y]:= -0.5;
        rivermap[h_x,h_y]:= True;
      end
      else begin
        rivermap[new_x,new_y]:= True;
        h_x:= new_x;
        h_y:= new_y;
      end;//else
    until (heightmap[h_x, h_y]<0.0);
  end;//for
  // ---- end of generation of rivers ----

  //now set the real tiles
  for i:= 1 to cMap_X-2 do
    for j:= 1 to cMap_Y-2 do
    begin
      if tiles[i,j]<>nil then
      begin
        tiles[i,j].Destroy;
        tiles[i,j]:= nil;
      end;
      if heightmap[i,j]<=0.0 then
        tiles[i,j]:= TTerrain.Create(ttSea, rivermap[i,j])
      else if heightmap[i,j]<=1.0 then
        tiles[i,j]:= TTerrain.Create(TerrainByH(j), rivermap[i,j])
      else if heightmap[i,j]<=2.0 then
        tiles[i,j]:= TTerrain.Create(ttHills, rivermap[i,j])
      else tiles[i,j]:= TTerrain.Create(ttMountains, rivermap[i,j]);
    end;//for

  {//smoothen landmass
  for i:=2 to cMap_X-3 do
    for j:=2 to cMap_Y-3 do
    begin
      if ((tiles[i-1,j].m_Type=ttGrassland) and (tiles[i,j-1].m_Type=ttGrassland) and
          (tiles[i+1,j].m_Type=ttGrassland) and (tiles[i,j+1].m_Type=ttGrassland)) then
          tiles[i,j].m_Type:= ttGrassland;
    end;//for}
  //update river cache
  ClearRiverCache;
  //set the flag to indicate that we have data for all tiles
  filled:= True;
  GenerateRiverCache;
end;//proc Generate (with f)

procedure TMap.GenerateSpecials(const LandOnly: Boolean=True);
var i, j: Integer;
begin
  if not filled then Generate(0.7, @h2, ttModerate, ctNormal)
  else Randomize;
  for i:= 0 to cMap_X-1 do
    for j:= 0 to cMap_Y-1 do
      if ((not LandOnly) or (not tiles[i,j].IsWater)) then
        if (random<=0.15) then tiles[i,j].CreateSpecial;
end;//proc

procedure TMap.ClearRiverCache;
var i,j: Byte;
begin
  for i:= 0 to cMap_X-1 do
    for j:= 0 to cMap_Y -1 do
      river[i,j]:= cMapRiverNone;
end;//proc

procedure TMap.GenerateRiverCache;
var i,j: Byte;
begin
  if (not filled) then Exit;
  for i:= 0 to cMap_X-1 do
    for j:= 0 to cMap_Y -1 do
    begin
      if (tiles[i,j].HasRiver) then SetRiverType(i,j)
      else river[i,j]:= cMapRiverNone;
    end;//for
end;//proc

function TMap.GetRiverType(const x,y: Byte): Byte;
begin
  if ((x<cMap_X) and (y<cMap_Y)) then
    Result:= river[x,y]
  else Result:= cMapRiverNone;
end;//func

function TMap.SaveToFile(const FileName: string): Boolean;
var fs: TFileStream;
    i,j: Integer;
    Buffer: Byte;
begin
  fs:= nil;
  try
    fs:= TFileStream.Create(FileName, fmCreate or fmShareDenyNone);
  except
    Result:= False;
    if fs<>nil then
      fs.Free;
    exit;
  end;//tryxcept
  if fs.Position>0 then fs.Seek(0, soBeginning);
  //Header
  fs.Write(cMapFileHeader, length(cMapFileHeader));
  //The real data
  for i:=0 to cMap_X-1 do
    for j:=0 to cMap_Y-1 do
    begin
      Buffer:= Ord(tiles[i,j].m_Type);
      fs.Write(Buffer, 1);
      if tiles[i,j].HasRiver then Buffer:= TERRAIN_RIVER_BIT else Buffer:= 0;
      if tiles[i,j].HasRoad then Buffer:= Buffer or TERRAIN_ROAD_BIT;
      if tiles[i,j].HasSpecial then Buffer:= Buffer or TERRAIN_SPECIAL_BIT;
      if tiles[i,j].IsPloughed then Buffer:= Buffer or TERRAIN_PLOUGHED_BIT;
      fs.Write(Buffer,1);
    end;//for
  fs.Free;
  Result:= True;
end;//func

function TMap.LoadFromFile(const FileName: string): Boolean;
var fs: TFileStream;
    bytes_read, i,j: Integer;
    buffer: array[0..255] of Byte;
begin
  Result:= False;
  if (not FileExists(FileName)) then Exit;
  fs:= nil;
  try
    fs:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  except
    Result:= False;
    if fs<>nil then
      fs.Free;
    Exit;
  end;//trycept

  //read header
  bytes_read:= fs.Read(buffer, length(cMapFileHeader));
  if (bytes_read<> length(cMapFileHeader)) then Exit;//file is to short to read header
  //check header
  for i:=1 to length(cMapFileHeader) do
    if (Chr(buffer[i-1]) <> cMapFileHeader[i]) then
    begin
      Result:= False;
      Exit;
    end;//if

  //now read the real data
  for i:= 0 to cMap_X-1 do
    for j:= 0 to cMap_Y-1 do
    begin
      bytes_read:= fs.Read(buffer[0],2);
      if bytes_read<>2 then
      begin
        //Read error
        fs.Free;
        Result:= False;
        Exit;
      end;//if
      if (buffer[0]>20) then //byte value does not represent a TTerrainType
      begin
        fs.Free;
        Result:= False;
        Exit;
      end;//id
      if tiles[i,j]<>nil then tiles[i,j].Free;
      tiles[i,j]:= TTerrain.Create(TTerrainType(buffer[0]),
                     (buffer[1] and TERRAIN_RIVER_BIT<>0),
                     (buffer[1] and TERRAIN_ROAD_BIT<>0),
                     (buffer[1] and TERRAIN_SPECIAL_BIT<>0),
                     (buffer[1] and TERRAIN_PLOUGHED_BIT<>0));
    end;//for

  fs.Free;
  filled:= True;
  GenerateRiverCache;
  Result:= True;
end;//func

//used later, for GUI
procedure TMap.SetRiverType(const x,y: Byte);
begin
  if ((x<cMap_X) and (y<cMap_Y) and filled) then

  begin
    river[x,y]:= cMapRiverNone;
    if (tiles[x,y].HasRiver) then
    begin
      if (x>0) then
        if (tiles[x-1,y].HasRiver) then river[x,y]:= river[x,y] or cMapRiverWest;
      if (y>0) then
        if (tiles[x,y-1].HasRiver) then river[x,y]:= river[x,y] or cMapRiverNorth;
      if (x+1<cMap_x) then
        if (tiles[x+1,y].HasRiver) then river[x,y]:= river[x,y] or cMapRiverEast;
      if (y+1<cMap_y) then
        if (tiles[x,y+1].HasRiver) then river[x,y]:= river[x,y] or cMapRiverSouth;
    end;//if
  end;//if
end;//proc

function TMap.IsTouchingLand(const x,y: Byte): Boolean;
var i,j: Integer;
begin
  Result:= False;
  if not filled then Exit;
  for i:= x-1 to x+1 do
    for j:= y-1 to y+1 do
    begin
      if (IsValidMapPosition(i,j)) then
        if (not tiles[i,j].IsWater) then
        begin
          Result:= True;
          Break;
        end;//if
    end;//for
  //maybe we should check for tile[x,y] being land and exempt it from examination
  // or at least react in some way...
end;//func

function TMap.IsAdjacentToWater(const x,y: Byte): Boolean;
var i, j: Integer;
begin
  Result:= False;
  //don't go for non-filled maps
  if not filled then Exit;
  //return false for (x,y) being water
  if IsValidMapPosition(x,y) then
    if tiles[x,y].IsWater then Exit;
  //loop through surrounding tiles
  for i:= x-1 to x+1 do
    for j:= y-1 to y+1 do
      if (i in [0..cMap_X-1]) and (j in [0..cMap_Y-1]) then
        if tiles[i,j].IsWater and ((i<>0) or (j<>0)) then
        begin
          Result:= True;
          Exit;
        end;//if
end;//func

function TMap.IsAdjacentToMountains(const x,y: Byte): Boolean;
var i, j: Integer;
begin
  Result:= False;
  //don't go for non-filled maps or invalid squares
  if not filled or not IsValidMapPosition(x,y) then Exit;
  //loop through surrounding tiles
  for i:= x-1 to x+1 do
    for j:= y-1 to y+1 do
      if (i in [0..cMap_X-1]) and (j in [0..cMap_Y-1]) then
        if (tiles[i,j].GetType in [ttHills, ttMountains]) and ((i<>0) or (j<>0)) then
        begin
          Result:= True;
          Exit;
        end;//if
end;//func

function TMap.IsAdjacentToForest(const x,y: Byte): Boolean;
var i, j: Integer;
begin
  Result:= False;
  //don't go for non-filled maps or invalid squares
  if not filled or not IsValidMapPosition(x,y) then Exit;
  //loop through surrounding tiles
  for i:= x-1 to x+1 do
    for j:= y-1 to y+1 do
      if (i in [0..cMap_X-1]) and (j in [0..cMap_Y-1]) then
        if tiles[i,j].HasForest and ((i<>0) or (j<>0)) then
        begin
          Result:= True;
          Exit;
        end;//if
end;//func

procedure TMap.DiscoverSurroundingTiles(const x,y: Byte; const cNation: Byte; const two_squares: Boolean);
var i, j: Integer;
begin
  if ((cNation>=cMinEuropean) and (cNation<=cMaxEuropean)) then
  begin
    for i:= x-1-Ord(two_squares) to x+1+Ord(two_squares) do
      for j:= y-1-Ord(two_squares) to y+1+Ord(two_squares) do
        if (i>=0) and (j>=0) and (i<=cMap_X-1) and (j<=cMap_Y-1) then
          discovered[i,j,cNation]:= True;
  end;//if
end;//proc

procedure TMap.RevealAll(const num_Nation: Integer);
var i, j: Integer;
begin
  if ((num_Nation<=cMaxEuropean) and (num_Nation>=cMinEuropean)) then
  begin
    for i:= 0 to cMap_X-1 do
      for j:= 0 to cMap_Y-1 do
        discovered[i,j, num_Nation]:= True;
  end;//if
end;//proc

function TMap.IsDiscovered(const x,y: Byte; const num_Nation: Integer): Boolean;
begin
  if ((x>=cMap_X) or (y>=cMap_Y) or (num_Nation>cMaxEuropean)
      or (num_Nation<cMinEuropean)) then
    Result:= True
  else Result:= discovered[x,y, num_Nation];
end;//func

function TMap.IsValidMapPosition(const x,y: Integer): Boolean;
begin
  Result:=  (x>=0) and (y>=0) and (x<cMap_X) and (y<cMap_Y);
end;//func

end.