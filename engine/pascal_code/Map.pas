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
  TMap = class
    private
      filled: Boolean; //internal value to determine whether map is not only nil
      discovered: array [0..cMap_X-1, 0..cMap_Y-1] of array [cMin_Nations..cMax_Nations] of Boolean;
      river: array [0..cMap_X-1, 0..cMap_Y-1] of Byte;
      procedure GenerateRiverCache;
      procedure ClearRiverCache;
      procedure SetRiverType(const x,y: Byte);
    public
      tiles: array [0..cMap_X-1, 0..cMap_Y-1] of TTerrain;
      constructor Create;
      destructor Destroy; override;
      procedure Generate(const Landmass: Single);
      procedure GenerateSpecials(const LandOnly: Boolean=True);
      function SaveToFile(const FileName: string): Boolean;
      function LoadFromFile(const FileName: string): Boolean;

      function GetRiverType(const x,y: Byte): Byte;
      //determines, whether a water tile has at least one non-water neighbour
      function IsTouchingLand(const x, y: Byte): Boolean;
      //proc for reavealing surrounding tiles around a unit
      procedure DiscoverSurroundingTiles(const x,y: Byte; const cNation: Byte; const two_squares: Boolean);
      //proc to reaveal complete map ("cheat")
      procedure RevealAll(const num_Nation: Integer);
      function IsDiscovered(const x,y: Byte; const num_Nation: Integer): Boolean;
  end;//class

implementation

constructor TMap.Create;
var i,j, k: Integer;
begin
  inherited Create;
  filled:= False;
  //initialize map tiles
  for i:= 0 to cMap_X-1 do
    for j:= 0 to cMap_Y-1 do
    begin
      for k:= cMin_Nations to cMax_Nations do
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

procedure TMap.GenerateSpecials(const LandOnly: Boolean=True);
var i, j: Integer;
begin
  if not filled then Generate(0.7)
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
      if ((i>=0) and (j>=0) and (i<cMap_X) and (j<cMap_Y)) then
        if (not tiles[i,j].IsWater) then
        begin
          Result:= True;
          Break;
        end;//if
    end;//for
  //maybe we should check for tile[x,y] being land and exempt it from examination
  // or at least react in some way...
end;//func

procedure TMap.DiscoverSurroundingTiles(const x,y: Byte; const cNation: Byte; const two_squares: Boolean);
var i, j: Integer;
begin
  if ((cNation>=cMin_Nations) and (cNation<=cMax_Nations)) then
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
  if ((num_Nation<=cMax_Nations) and (num_Nation>=cMin_Nations)) then
  begin
    for i:= 0 to cMap_X-1 do
      for j:= 0 to cMap_Y-1 do
        discovered[i,j, num_Nation]:= True;
  end;//if
end;//proc

function TMap.IsDiscovered(const x,y: Byte; const num_Nation: Integer): Boolean;
begin
  if ((x>=cMap_X) or (y>=cMap_Y) or (num_Nation>cMax_Nations)
      or (num_Nation<cMin_Nations)) then
    Result:= True
  else Result:= discovered[x,y, num_Nation];
end;//func

end.
