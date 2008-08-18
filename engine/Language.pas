unit Language;

interface

uses
  Goods, Units, Terrain, Nation, SysUtils;

type
  TMenuCategory = (mcNone, mcGame, mcView, mcOrders, mcReports, mcTrade);
  TLanguage = class
    private
      Menu: array[TMenuCategory] of string;
      MenuOptions: array [TMenuCategory] of array [1..10] of string;
      GoodNames: array[TGoodType] of string;
      NationNames: array[cMin_Nations..cMaxIndian] of string;
      TerrainNames: array[TTerrainType] of string;
      UnitNames: array[TUnitType] of string;
      Seasons: array[0..1] of string;
      //others (Maybe we should introduce a structure for them, too.)
      Location: string;
      Moves: string;
      procedure InitialValues;
    public
      constructor Create;
      function GetOptionCount(const categ: TMenuCategory): Integer;
      function GetMenuLabel(const categ: TMenuCategory): string;
      function GetGoodName(const AGood: TGoodType): string;
      function GetNationName(const NationNum: Integer): string;
      function GetTerrainName(const ATerrain: TTerrainType): string;
      function GetUnitName(const AUnit: TUnitType): string;
      function GetSeason(const autumn: Boolean): string;
      //others
      function GetLocation: string;
      function GetMoves: string;
      function SaveToFile(const FileName: string): Boolean;
      function LoadFromFile(const FileName: string): Boolean;
  end;//class

implementation

constructor TLanguage.Create;
begin
  InitialValues;
end;//file

procedure TLanguage.InitialValues;
var i,j: Integer;
begin
  Menu[mcNone]:= '';
  Menu[mcGame]:= 'Spiel';
  Menu[mcView]:= 'Ansicht';
  Menu[mcOrders]:= 'Befehle';
  Menu[mcReports]:= 'Berichte';
  Menu[mcTrade]:= 'Handel';
  //options (empty strings)
  for i:= Ord(Low(TMenuCategory)) to Ord(High(TMenuCategory)) do
    for j:= 1 to 10 do
      MenuOptions[TMenuCategory(i), j]:= '';
  //goods
  GoodNames[gtFood]:= 'Nahrungsmittel';
  GoodNames[gtSugar]:= 'Zuckerrohr';
  GoodNames[gtTobacco]:= 'Tabak';
  GoodNames[gtCotton]:= 'Baumwolle';
  GoodNames[gtFur]:= 'Felle';
  GoodNames[gtWood]:= 'Nutzholz';
  GoodNames[gtOre]:= 'Erz';
  GoodNames[gtSilver]:= 'Silber';
  GoodNames[gtHorses]:= 'Pferde';
  GoodNames[gtRum]:= 'Rum';
  GoodNames[gtCigar]:= 'Zigarren';
  GoodNames[gtCloth]:= 'Stoff';
  GoodNames[gtCoat]:= 'Mäntel';
  GoodNames[gtTradegoods]:= 'Handelswaren';
  GoodNames[gtTool]:= 'Werkzeuge';
  GoodNames[gtMusket]:= 'Musketen';
  GoodNames[gtHammer]:= 'Hämmer';
  GoodNames[gtLibertyBell]:= 'Freiheitsglocken';
  GoodNames[gtCross]:= 'Kreuze';
  //Nations
  // - Europeans
  NationNames[cNationEngland]:= 'England';
  NationNames[cNationFrance]:= 'Frankreich';
  NationNames[cNationSpain]:= 'Spanien';
  NationNames[cNationHolland]:= 'Holland';
  // - Indians
  NationNames[cNationArawak]:= 'Arukaner';
  NationNames[cNationAztec]:= 'Azteken';
  NationNames[cNationInca]:= 'Inka';
  NationNames[cNationTupi]:= 'Tupi';
  NationNames[cNationCherokee]:= 'Cherokee';
  NationNames[cNationIroquois]:= 'Irokesen';
  NationNames[cNationSioux]:= 'Sioux';
  NationNames[cNationApache]:= 'Apache';
  //units
  UnitNames[utCriminal]:= 'Kleinkriminelle';
  UnitNames[utServant]:= 'Verdingte Knechte';
  UnitNames[utColonist]:= 'Freie Siedler';
  UnitNames[utFarmer]:= 'Farmer';
  UnitNames[utFisher]:= 'Fischer';
  UnitNames[utFurHunter]:= 'Pelzjäger';
  UnitNames[utSilverMiner]:= 'Silberbergarbeiter';
  UnitNames[utWoodcutter]:= 'Holzfäller';
  UnitNames[utOreMiner]:= 'Erzbergarbeiter';
  UnitNames[utSugarplanter]:= 'Zuckerpflanzer';
  UnitNames[utCottonplanter]:= 'Baumwollpflanzer';
  UnitNames[utTobaccoplanter]:= 'Tabakpflanzer';
  UnitNames[utPreacher]:= 'Prediger';
  UnitNames[utStatesman]:= 'Staatsmann';
  UnitNames[utCarpenter]:= 'Schreiner';
  UnitNames[utDistiller]:= 'Rumbrenner';
  UnitNames[utWeaver]:= 'Weber';
  UnitNames[utTobacconist]:= 'Zigarrenhändler';
  UnitNames[utFurTrader]:= 'Pelzhändler';
  UnitNames[utSmith]:= 'Schmied';
  UnitNames[utWeaponSmith]:= 'Waffenschmied';
  UnitNames[utScout]:= 'Späher';
  UnitNames[utPioneer]:= 'Pioniere';
  UnitNames[utMissionary]:= 'Missionar';
  UnitNames[utRegular]:= 'Reguläre';
  UnitNames[utDragoon]:= 'Dragoner';
  UnitNames[utArtillery]:= 'Artillerie';
  UnitNames[utConvoy]:= 'Wagenzug';
  UnitNames[utCaravel]:= 'Karavelle';
  UnitNames[utTradingShip]:= 'Handelsschiff';
  UnitNames[utGalleon]:= 'Galleone';
  UnitNames[utPrivateer]:= 'Kaperschiff';
  UnitNames[utFrigate]:= 'Fregatte';
  UnitNames[utMan_o_War]:= 'Kriegsschiff';
  UnitNames[utBrave]:= 'Krieger';
  UnitNames[utBraveOnHorse]:= 'Berittener Krieger';
  //terrain types
  TerrainNames[ttArctic]:= 'Arktisch';
  TerrainNames[ttSea]:= 'Ozean';
  TerrainNames[ttOpenSea]:= 'Seeweg';
  TerrainNames[ttPlains]:= 'Flachland';
  TerrainNames[ttGrassland]:= 'Grünland';
  TerrainNames[ttPrairie]:= 'Prärie';
  TerrainNames[ttSavannah]:= 'Savanne';
  TerrainNames[ttMarsh]:= 'Feuchtgebiete';
  TerrainNames[ttSwamp]:= 'Sumpfland';
  TerrainNames[ttDesert]:= 'Wüste';
  TerrainNames[ttTundra]:= 'Tundra';
  TerrainNames[ttBoreal]:= 'Borealwald';
  TerrainNames[ttWetland]:= 'Feuchtwald';
  TerrainNames[ttScrubForest]:= 'Gestrüppwald';
  TerrainNames[ttBroadleaf]:= 'Laubwald';
  TerrainNames[ttMixedForest]:= 'Mischwald';
  TerrainNames[ttConiferForest]:= 'Nadelwald';
  TerrainNames[ttRainForest]:= 'Regenwald';
  TerrainNames[ttTropicalForest]:= 'Tropenwald';
  TerrainNames[ttHills]:= 'Hügellandschaft';
  TerrainNames[ttMountains]:= 'Berge';
  //seasons
  Seasons[0]:= 'Frühling';
  Seasons[1]:= 'Herbst';
  //others
  Location:= 'Ort';
  Moves:= 'Züge';
end;//proc

function TLanguage.GetOptionCount(const categ: TMenuCategory): Integer;
var i: Integer;
begin
  Result:= 0;
  i:=1;
  for i:=1 to 10 do
  begin
   if MenuOptions[categ][i]<>'' then Result:= i else break;
  end;//for
end;//func

function TLanguage.GetMenuLabel(const categ: TMenuCategory): string;
begin
  Result:= Menu[categ];
end;//func

function TLanguage.GetGoodName(const AGood: TGoodType): string;
begin
  Result:= GoodNames[AGood];
end;//func

function TLanguage.GetNationName(const NationNum: Integer): string;
begin
  if (NationNum<cMin_Nations) or (NationNum>cMaxIndian) then Result:= '(no nation)'
  else Result:= NationNames[NationNum];
end;//func

function TLanguage.GetTerrainName(const ATerrain: TTerrainType): string;
begin
  Result:= TerrainNames[ATerrain];
end;//func

function TLanguage.GetUnitName(const AUnit: TUnitType): string;
begin
  Result:= UnitNames[AUnit];
end;//func

function TLanguage.GetSeason(const autumn: Boolean): string;
begin
  if autumn then Result:= Seasons[1]
  else Result:= Seasons[0];
end;//func

function TLanguage.GetLocation: string;
begin
  Result:= Location;
end;//func

function TLanguage.GetMoves: string;
begin
  Result:= Moves;
end;//func

function TLanguage.SaveToFile(const FileName: string): Boolean;
var dat: TextFile;
    i: Integer;
begin
  Result:= False;
  try
    AssignFile(dat, FileName);
    Reset(dat);
    WriteLn(dat, '[Goods]');
  except
    Result:= False;
    CloseFile(dat);
  end;//try-xcept
  for i:= Ord(Low(TGoodType)) to Ord(High(TGoodType)) do
    WriteLn(dat, GoodNames[TGoodType(i)]);
  WriteLn;
  WriteLn(dat, '[Nations]');
  for i:= cMin_Nations to cMaxIndian do
    WriteLn(dat, NationNames[i]);
  WriteLn;
  WriteLn(dat, '[Terrain]');
  for i:= Ord(Low(TTerrainType)) to Ord(High(TTerrainType)) do
    WriteLn(dat, TerrainNames[TTerrainType(i)]);
  WriteLn;
  WriteLn(dat, '[Units]');
  for i:= Ord(Low(TUnitType)) to Ord(High(TUnitType)) do
    WriteLn(dat, UnitNames[TUnitType(i)]);
  WriteLn;
  WriteLn(dat, '[Seasons]');
  WriteLn(dat, Seasons[0]);
  WriteLn(dat, Seasons[1]);
  WriteLn;
  WriteLn(dat, '[Others]');
  WriteLn(dat, Location);
  WriteLn(dat, Moves);
  WriteLn;
  CloseFile(dat);
  Result:= True;
end;//func

function TLanguage.LoadFromFile(const FileName: string): Boolean;
var dat: TextFile;
    str1: string;
    i: Integer;
begin
  if not FileExists(FileName) then Result:= False
  else begin
    try
      AssignFile(dat, FileName);
    except
      Result:= False;
      CloseFile(dat);
      Exit;
    end;

    while not Eof(dat) do
    begin
      ReadLn(dat, str1);
      if str1='[Goods]' then
      begin
        i:= Ord(Low(TGoodType));
        while (i<=Ord(High(TGoodType))) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then GoodNames[TGoodType(i)]:= str1;
          i:= i+1;
        end;//while
      end//if
      else if str1='[Nations]' then
      begin
        i:= cMin_Nations;
        while (i<=cMaxIndian) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then NationNames[i]:= str1;
          i:= i+1;
        end;//while
      end//if
      else if str1='[Terrain]' then
      begin
        i:= Ord(Low(TTerrainType));
        while (i<=Ord(High(TTerrainType))) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then TerrainNames[TTerrainType(i)]:= str1;
          i:= i+1;
        end;//while
      end//if
      else if str1='[Units]' then
      begin
        i:= Ord(Low(TUnitType));
        while (i<=Ord(High(TUnitType))) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then UnitNames[TUnitType(i)]:= str1;
          i:= i+1;
        end;//while
      end//if
      else if str1='[Seasons]' then
      begin
        i:= 0;
        while (i<=1) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then Seasons[i]:= str1;
          i:= i+1;
        end;//while
      end//if
      else if str1='[Others]' then
      begin
        i:= 0;
        while (i<=1) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then
          begin
            if i=0 then Location:= str1
            else Moves:= str1;
          end;//if
          i:= i+1;
        end;//while
      end;//if
    end;//while
    CloseFile(dat);
    Result:= True;
  end;//else
end;//func

end.
