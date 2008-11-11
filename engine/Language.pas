unit Language;

interface

uses
  Goods, Units, Terrain, Nation, SysUtils;

type
  TMenuCategory = (mcNone, mcGame, mcView, mcOrders, mcReports, mcTrade);
  TSaveLoadString = (slsLoadChoose, slsLoadError, slsLoadSuccess, slsSaveChoose, slsSaveError, slsSaveSuccess, slsNoGameLoaded);
  TTransferString = (tsBoycotted, tsOutOfGold, tsOutOfSpace);
  TOtherString = (osLocation, osDestination, osFreight, osShip, osHighSea,
                  osNewWorld, osMoves, osEmpty, osNothing, osNoChanges, osTax, osGold, osCost, osSaving, osEarnings);
  TEuroPortString = (epsHeading, epsNotOnShip, epsGoOnShip, epsArm, epsDisarm, epsGiveHorses, epsNoHorses, epsGiveTools, epsNoTools);
  TReportType = (rtNone, rtEconomy, rtColony, rtFleet);
  TColonyString = (csRenameQuestion, csRenameLabel, csAbandonYes, csAbandonNo, csAbandonQuestion);
  TColonyUnitString = (cusOptions, cusCancelOrders, cusOnBoard, cusFortify);
  TPioneerString = (psNoTools, psHasRoad, psIsPloughed, psIsCleared, psNeedsClearing, psWrongUnit);
  TLanguage = class
    private
      Menu: array[TMenuCategory] of string;
      MenuOptions: array [TMenuCategory] of array [1..10] of string;
      menu_helpers: array [TMenuCategory] of record
                                               max_len: Integer;
                                               count:  Integer;
                                             end;//rec
      GoodNames: array[TGoodType] of string;
      NationNames: array[cMin_Nations..cMaxIndian] of string;
      PortNames: array[cMinEuropean..cMaxEuropean] of string;
      TerrainNames: array[TTerrainType] of string;
      UnitNames: array[TUnitType] of string;
      Seasons: array[0..1] of string;
      Transfer: array[TTransferString] of string;
      //others
      Others: array[TOtherString] of string;
      //for messages after saving/loading the game
      SaveLoad: array[TSaveLoadString] of string;
      //for landfall message box
      Landfall: array[0..2] of string;
      //for building new colonies
      BuildColony: array[0..4] of string;
      //Renaming colonies & Abandon colony
      ColonyStrings: array[TColonyString] of string;
      //proposal for colony names
      ColonyNames: array [cMinEuropean..cMaxEuropean] of array of ShortString;
      //managing units outside of colonies (but within colony square)
      ColonyUnit: array[TColonyUnitString] of string;
      //for managing units in european port
      EuroPortManage: array[TEuroPortString] of string;
      //for pioneer actions
      Pioneers: array[TPioneerString] of string;
      procedure InitialValues;
      procedure InitialColonyNames;
      procedure SetMenuHelpers;
      function privGetOptionCount(const categ: TMenuCategory): Integer;
    public
      constructor Create;
      //menu related
      function GetOptionCount(const categ: TMenuCategory): Integer;
      function GetMenuLabel(const categ: TMenuCategory): string;
      function GetMenuOption(const categ: TMenuCategory; const option: Byte): string;
      //menu helper
      function GetMaxLen(const categ: TMenuCategory): Integer;
      //general stuff
      function GetGoodName(const AGood: TGoodType): string;
      function GetNationName(const NationNum: Integer): string;
      function GetPortName(const NationNum: Integer): string;
      function GetTerrainName(const ATerrain: TTerrainType): string;
      function GetUnitName(const AUnit: TUnitType): string;
      function GetSeason(const autumn: Boolean): string;
      function GetTransfer(const which_string: TTransferString): string;
      //others
      function GetOthers(const which_one: TOtherString): string;
      function GetSaveLoad(const which: TSaveLoadString): string;
      function GetLandfall(const which: Byte): string;
      function GetBuildColony(const which: Byte): string;
      function GetColonyString(const which: TColonyString): string;
      function GetColonyNames(const num_nation: LongInt; col_count: Byte): string;
      function GetColonyUnit(const which: TColonyUnitString): string;
      function GetEuroPort(const which: TEuroPortString): string;
      function GetPioneer(const which: TPioneerString): string;
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
  //present options
  // -- Spiel
  MenuOptions[mcGame, 1]:= 'Speichern';
  MenuOptions[mcGame, 2]:= 'Laden';
  MenuOptions[mcGame, 3]:= 'Spiel beenden';
  // -- Ansicht
  MenuOptions[mcView, 1]:= 'Europa-Status';
  MenuOptions[mcView, 2]:= 'Ansicht zentrieren';
  // -- Befehle
  MenuOptions[mcOrders, 1]:= 'Befestigen';
  MenuOptions[mcOrders, 2]:= 'Gehe zu';
  MenuOptions[mcOrders, 3]:= 'Wald roden';
  MenuOptions[mcOrders, 4]:= 'Felder pflügen';
  MenuOptions[mcOrders, 5]:= 'Straße bauen';
  MenuOptions[mcOrders, 6]:= 'Keine Befehle';
  MenuOptions[mcOrders, 7]:= 'Einheit auflösen';
  // -- Berichte
  MenuOptions[mcReports, 1]:= 'Wirtschaftsbericht';
  MenuOptions[mcReports, 2]:= 'Koloniebericht';
  MenuOptions[mcReports, 3]:= 'Flottenbericht';
  // -- Handel
  MenuOptions[mcTrade, 1]:= 'Handelsroute festlegen';
  MenuOptions[mcTrade, 2]:= 'Handelsroute ändern';
  MenuOptions[mcTrade, 3]:= 'Handelsroute löschen';

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
  //ports in europe
  PortNames[cNationEngland]:= 'London';
  PortNames[cNationFrance]:= 'La Rochelle';
  PortNames[cNationSpain]:= 'Sevilla';
  PortNames[cNationHolland]:= 'Amsterdam';
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
  //good transfers
  Transfer[tsBoycotted]:= 'Diese Ware wird momentan vom Parlament boykottiert, Eure    '
                          +'Exzellenz. Wir können damit nicht handeln, bis das Parlament'
                          +'den Boykott aufhebt.';
  Transfer[tsOutOfGold]:= 'Wir haben nicht genug Gold, um uns 100 Einheiten der gewün- '
                          +'schten Ware zu leisten, Eure Exzellenz.';
  Transfer[tsOutOfSpace]:= 'Unser Schiff hat nicht genug freien Laderaum, um noch diese '
                           +'Ware noch laden zu können.';
  //others
  Others[osLocation]:= 'Ort';
  Others[osDestination]:= 'Ziel';
  Others[osFreight]:= 'Fracht';
  Others[osShip]:= 'Schiff';
  Others[osHighSea]:= 'Hohe See';
  Others[osNewWorld]:= 'Neue Welt';
  Others[osMoves]:= 'Züge';
  Others[osEmpty]:= 'leer';
  Others[osNothing]:= 'nichts';
  Others[osNoChanges]:= 'Keine Veränderungen';
  Others[osTax]:= 'Steuer';
  Others[osGold]:= 'Gold';
  Others[osCost]:= 'Kosten';
  Others[osSaving]:= 'Einsparung';
  Others[osEarnings]:= 'Gewinn';
  //save/ load messages
  SaveLoad[slsLoadChoose]:= 'Wählen Sie den zu ladenden Spielstand.';
  SaveLoad[slsLoadError]:= 'Fehler beim Laden des Spielstandes! Das geladene Spiel kann '
                          +'unter Umständen unvorhersehbare Fehler verursachen, daher   '
                          +'wird das aktuelle Spiel beendet. Versuchen Sie, ein anderes '
                          +'Spiel neu zu laden oder das Programm erneut zu starten.';
  SaveLoad[slsLoadSuccess]:= 'Spiel wurde erfolgreich geladen!';
  SaveLoad[slsSaveChoose]:= 'Wählen Sie den Slot, in welchem das Spiel gespeichert werden'
                           +'soll.';
  SaveLoad[slsSaveError]:= 'Fehler beim Speichern des Spieles! Die gespeicherten Daten  '
                          +'sind möglicherweise unbrauchbar und können beim Versuch,    '
                          +'diese zu laden, zum Spielabbruch führen.';
  SaveLoad[slsSaveSuccess]:= 'Das Spiel wurde gespeichert.';
  SaveLoad[slsNoGameLoaded]:= 'Es ist kein Spiel geladen, welches gespeichert werden       '
                             +'könnte.';
  //landfall
  Landfall[0]:= 'Sollen wir an Land gehen, Eure Exzellenz, und die Schiffe zurücklassen?';
  Landfall[1]:= 'Bei den Schiffen bleiben';
  Landfall[2]:= 'An Land gehen';
  //build colony
  BuildColony[0]:= 'Wie sollen wir diese Kolonie nennen?';
  BuildColony[1]:= 'Name:';
  // -- build colony error messages
  BuildColony[2]:= 'Sie können keine Kolonie im Wasser oder vom Schiff aus er-  '
                  +'richten, sondern müssen erst Sielder an Land schicken.';
  BuildColony[3]:= 'Dieses Land liegt für eine neue Kolonie zu nah an einer     '
                  +'schon bestehenden Kolonie, Eure Exzellenz.';
  BuildColony[4]:= 'Kolonien können nicht in den Bergen gebaut werden, Eure Exzellenz.';
  //renaming colonies & abandon colony
  ColonyStrings[csRenameQuestion]:= 'Wie soll diese Kolonie jetzt heißen?';
  ColonyStrings[csRenameLabel]:= 'Neuer Name';
  ColonyStrings[csAbandonYes]:= 'Ja, es ist mein Wille.';
  ColonyStrings[csAbandonNo]:= 'Nein, das wäre töricht!';
  ColonyStrings[csAbandonQuestion]:= 'Sollen wir unsere Kolonie wirklich aufgeben, Eure Exzellenz, so dass all unsere harte Arbeit hier umsonst war?';
  //units outside of colony but still in colony square
  ColonyUnit[cusOptions]:= 'Optionen für';
  ColonyUnit[cusCancelOrders]:= 'Befehle aufheben';
  ColonyUnit[cusOnBoard]:= 'Wache/ An Bord gehen';
  ColonyUnit[cusFortify]:= 'Befestigen';
  //colony names
  for i:= cMinEuropean to cMaxEuropean do
    SetLength(ColonyNames[i], 0);
  InitialColonyNames;

  //for European ports
  EuroPortManage[epsHeading]:= 'Optionen für Siedler im europäischen Hafen:';
  EuroPortManage[epsNotOnShip]:= 'Nicht aufs nächste Schiff gehen';
  EuroPortManage[epsGoOnShip]:= 'An Bord des nächsten Schiffes gehen';
  EuroPortManage[epsArm]:= 'Mit Musketen bewaffnen';
  EuroPortManage[epsDisarm]:= 'Musketen verkaufen';
  EuroPortManage[epsGiveHorses]:= 'Mit Pferden ausrüsten';
  EuroPortManage[epsNoHorses]:= 'Pferde verkaufen';
  EuroPortManage[epsGiveTools]:= 'Mit Werkzeugen ausrüsten';
  EuroPortManage[epsNoTools]:= 'Werkzeuge verkaufen';

  //for pionieers
  Pioneers[psNoTools]:= 'Die Einheit hat nicht genug Werkzeuge, um diese Aktion aus- '
                       +'zuführen. Es sind mindestens 20 Werkzeuge nötig.';
  Pioneers[psHasRoad]:= 'Hier gibt es schon eine Straße, Eure Exzellenz.';
  Pioneers[psIsPloughed]:= 'Dieses Gebiet ist schon gepflügt, Eure Exzellenz.';
  Pioneers[psIsCleared]:= 'Dieses Gelände ist schon gerodet, Eure Exzellenz.';
  Pioneers[psNeedsClearing]:= 'Das Gebiet muss erst gerodet werden, bevor wir es pflügen   '
                             +'können, Eure Exzellenz.';
  Pioneers[psWrongUnit]:= 'Nur Siedler oder Pioniere, welche mit Werkzeugen ausgerüstet'
                         +' sind, können diese Aktion durchführen, Eure Exzellenz.';

  SetMenuHelpers;
end;//proc

procedure TLanguage.InitialColonyNames;
begin
  SetLength(ColonyNames[cNationEngland], 16);
  ColonyNames[cNationEngland, 0]:= 'Jamestown';
  ColonyNames[cNationEngland, 1]:= 'Plymouth';
  ColonyNames[cNationEngland, 2]:= 'Roanoke';
  ColonyNames[cNationEngland, 3]:= 'Barbados';
  ColonyNames[cNationEngland, 4]:= 'Penobscot';
  ColonyNames[cNationEngland, 5]:= 'Boston';
  ColonyNames[cNationEngland, 6]:= 'Baltimore';
  ColonyNames[cNationEngland, 7]:= 'Providence';
  ColonyNames[cNationEngland, 8]:= 'Hartford';
  ColonyNames[cNationEngland, 9]:= 'New Haven';
  ColonyNames[cNationEngland, 10]:= 'New York';
  ColonyNames[cNationEngland, 11]:= 'Albany';
  ColonyNames[cNationEngland, 12]:= 'New Jersey';
  ColonyNames[cNationEngland, 13]:= 'Charleston';
  ColonyNames[cNationEngland, 14]:= 'Philadelphia';
  ColonyNames[cNationEngland, 15]:= 'Newport';

  SetLength(ColonyNames[cNationFrance], 16);
  ColonyNames[cNationFrance, 0]:= 'Quebec';
  ColonyNames[cNationFrance, 1]:= 'Montreal';
  ColonyNames[cNationFrance, 2]:= 'Guadelupe';
  ColonyNames[cNationFrance, 3]:= 'Cayenne';
  ColonyNames[cNationFrance, 4]:= 'St. Louis';
  ColonyNames[cNationFrance, 5]:= 'Martinique';
  ColonyNames[cNationFrance, 6]:= 'Port Royal';
  ColonyNames[cNationFrance, 7]:= 'Port au Prince';
  ColonyNames[cNationFrance, 8]:= 'Trois Rivieres';
  ColonyNames[cNationFrance, 9]:= 'New Orleans';
  ColonyNames[cNationFrance, 10]:= 'Fort Caroline';
  ColonyNames[cNationFrance, 11]:= 'Fort Detroit';
  ColonyNames[cNationFrance, 12]:= 'Fort Frontenac';
  ColonyNames[cNationFrance, 13]:= 'Fort Pontchartain';
  ColonyNames[cNationFrance, 14]:= 'Fort Tadoussac';
  ColonyNames[cNationFrance, 15]:= 'Fort Canada';

  SetLength(ColonyNames[cNationSpain], 16);
  ColonyNames[cNationSpain, 0]:= 'Isabella';
  ColonyNames[cNationSpain, 1]:= 'Santo Domingo';
  ColonyNames[cNationSpain, 2]:= 'San Salvador';
  ColonyNames[cNationSpain, 3]:= 'Veracruz';
  ColonyNames[cNationSpain, 4]:= 'Habana';
  ColonyNames[cNationSpain, 5]:= 'Trinidad';
  ColonyNames[cNationSpain, 6]:= 'San Juan';
  ColonyNames[cNationSpain, 7]:= 'Panama';
  ColonyNames[cNationSpain, 8]:= 'Cartagena';
  ColonyNames[cNationSpain, 9]:= 'St. Augustine';
  ColonyNames[cNationSpain, 10]:= 'Lima';
  ColonyNames[cNationSpain, 11]:= 'Buenos Aires';
  ColonyNames[cNationSpain, 12]:= 'Guatemala';
  ColonyNames[cNationSpain, 13]:= 'Honduras';
  ColonyNames[cNationSpain, 14]:= 'Santiago';
  ColonyNames[cNationSpain, 15]:= 'Asuncion';

  SetLength(ColonyNames[cNationHolland], 16);
  ColonyNames[cNationHolland, 0]:= 'New Amsterdam';
  ColonyNames[cNationHolland, 1]:= 'Fort Orange';
  ColonyNames[cNationHolland, 2]:= 'Fort Nassau';
  ColonyNames[cNationHolland, 3]:= 'Neuholland';
  ColonyNames[cNationHolland, 4]:= 'Vlissingen';
  ColonyNames[cNationHolland, 5]:= 'Curaco';
  ColonyNames[cNationHolland, 6]:= 'Recife';
  ColonyNames[cNationHolland, 7]:= 'Bahia';
  ColonyNames[cNationHolland, 8]:= 'Paramaribo';
  ColonyNames[cNationHolland, 9]:= 'Pernambuco';
  ColonyNames[cNationHolland, 10]:= 'St. Martin';
  ColonyNames[cNationHolland, 11]:= 'Surinam';
  ColonyNames[cNationHolland, 12]:= 'Willemstad';
  ColonyNames[cNationHolland, 13]:= 'Aruba';
  ColonyNames[cNationHolland, 14]:= 'Utrecht';
  ColonyNames[cNationHolland, 15]:= 'Haarlem';
end;//proc

procedure TLanguage.SetMenuHelpers;
var i, j, temp: Integer;
begin
  for i:= Ord(Low(TMenuCategory)) to Ord(High(TMenuCategory)) do
  begin
    menu_helpers[TMenuCategory(i)].count:= privGetOptionCount(TMenuCategory(i));
    //determine max len
    menu_helpers[TMenuCategory(i)].max_len:= length(GetMenuLabel(TMenuCategory(i)))-2;
    for j:= 1 to menu_helpers[TMenuCategory(i)].count do
    begin
      temp:= length(GetMenuOption(TMenuCategory(i), j));
      if temp>menu_helpers[TMenuCategory(i)].max_len then
        menu_helpers[TMenuCategory(i)].max_len:= temp;
    end;//for
  end;//for
end;//func

function TLanguage.privGetOptionCount(const categ: TMenuCategory): Integer;
var i: Integer;
begin
  Result:= 0;
  for i:=1 to 10 do
  begin
   if MenuOptions[categ][i]<>'' then Result:= i else break;
  end;//for
end;//func

function TLanguage.GetOptionCount(const categ: TMenuCategory): Integer;
begin
  Result:= menu_helpers[categ].count;
end;//func

function TLanguage.GetMenuLabel(const categ: TMenuCategory): string;
begin
  Result:= Menu[categ];
end;//func

function TLanguage.GetMenuOption(const categ: TMenuCategory; const option: Byte): string;
begin
  if (option in [1..10]) then Result:= MenuOptions[categ, option]
  else Result:= '';
end;//func

function TLanguage.GetMaxLen(const categ: TMenuCategory): Integer;
begin
  Result:= menu_helpers[categ].max_len;
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

function TLanguage.GetPortName(const NationNum: Integer): string;
begin
  if (NationNum<cMinEuropean) or (NationNum>cMaxEuropean) then Result:= '(no port name)'
  else Result:= PortNames[NationNum];
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

function TLanguage.GetTransfer(const which_string: TTransferString): string;
begin
  Result:= Transfer[which_string];
end;//func

function TLanguage.GetOthers(const which_one: TOtherString): string;
begin
  Result:= Others[which_one];
end;//func

function TLanguage.GetSaveLoad(const which: TSaveLoadString): string;
begin
  Result:= SaveLoad[which];
end;//func

function TLanguage.GetLandfall(const which: Byte): string;
begin
  if which>2 then Result:= ''
  else Result:= Landfall[which];
end;//func

function TLanguage.GetBuildColony(const which: Byte): string;
begin
  if which>4 then Result:= ''
  else Result:= BuildColony[which];
end;//func

function TLanguage.GetColonyString(const which: TColonyString): string;
begin
  Result:= ColonyStrings[which];
end;//func

function TLanguage.GetColonyNames(const num_nation: LongInt; col_count: Byte): string;
begin
  if (num_nation in [cMinEuropean..cMaxEuropean]) then
  begin
    if col_count<=High(ColonyNames[num_nation]) then Result:= ColonyNames[num_nation, col_count]
    else Result:= GetNationName(num_nation)+' #'+IntToStr(col_count);
  end
  else Result:= '(colony name)';
end;//func

function TLanguage.GetColonyUnit(const which: TColonyUnitString): string;
begin
  Result:= ColonyUnit[which];
end;//func

function TLanguage.GetEuroPort(const which: TEuroPortString): string;
begin
  Result:= EuroPortManage[which];
end;//func

function TLanguage.GetPioneer(const which: TPioneerString): string;
begin
  Result:= Pioneers[which];
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
  WriteLn(dat);
  WriteLn(dat, '[Nations]');
  for i:= cMin_Nations to cMaxIndian do
    WriteLn(dat, NationNames[i]);
  WriteLn(dat);
  WriteLn(dat, '[Ports]');
  for i:= cMinEuropean to cMaxEuropean do
    WriteLn(dat, PortNames[i]);
  WriteLn(dat);
  WriteLn(dat, '[Terrain]');
  for i:= Ord(Low(TTerrainType)) to Ord(High(TTerrainType)) do
    WriteLn(dat, TerrainNames[TTerrainType(i)]);
  WriteLn(dat);
  WriteLn(dat, '[Units]');
  for i:= Ord(Low(TUnitType)) to Ord(High(TUnitType)) do
    WriteLn(dat, UnitNames[TUnitType(i)]);
  WriteLn(dat);
  WriteLn(dat, '[Seasons]');
  WriteLn(dat, Seasons[0]);
  WriteLn(dat, Seasons[1]);
  WriteLn(dat);
  WriteLn(dat, '[Transfer]');
  for i:= Ord(Low(TTransferString)) to Ord(High(TTransferString)) do
    WriteLn(dat, Transfer[TTransferString(i)]);
  WriteLn(dat);
  WriteLn(dat, '[Others]');
  for i:= Ord(Low(TOtherString)) to Ord(High(TOtherString)) do
    WriteLn(dat, Others[TOtherString(i)]);
  WriteLn(dat);
  WriteLn(dat, '[SaveLoad]');
  for i:= Ord(Low(TSaveLoadString)) to Ord(High(TSaveLoadString)) do
    WriteLn(dat, SaveLoad[TSaveLoadString(i)]);
  WriteLn(dat);
  WriteLn(dat, '[Landfall]');
  for i:=0 to 2 do
    WriteLn(dat, Landfall[i]);
  WriteLn(dat);
  WriteLn(dat, '[BuildColony]');
  for i:=0 to 4 do
    WriteLn(dat, BuildColony[i]);
  WriteLn(dat);
  WriteLn(dat, '[ColonyStrings]');
  for i:=Ord(Low(TColonyString)) to Ord(High(TColonyString)) do
    WriteLn(dat, ColonyStrings[TColonyString(i)]);
  WriteLn(dat);
  WriteLn(dat, '[ColonyUnit]');
  for i:=Ord(Low(TColonyUnitString)) to Ord(High(TColonyUnitString)) do
    WriteLn(dat, ColonyUnit[TColonyUnitString(i)]);
  WriteLn(dat);
  WriteLn(dat, '[EuropeanPort]');
  for i:= Ord(Low(TEuroPortString)) to Ord(High(TEuroPortString)) do
    WriteLn(dat, EuroPortManage[TEuroPortString(i)]);
  WriteLn(dat);
  WriteLn(dat, '[Pioneers]');
  for i:= Ord(Low(TPioneerString)) to Ord(High(TPioneerString)) do
    WriteLn(dat, Pioneers[TPioneerString(i)]);
  CloseFile(dat);
  Result:= True;
end;//func

function TLanguage.LoadFromFile(const FileName: string): Boolean;
var dat: TextFile;
    str1: AnsiString;
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
      else if str1='[Ports]' then
      begin
        i:= cMinEuropean;
        while (i<=cMaxEuropean) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then PortNames[i]:= str1;
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
      else if str1='[Transfer]' then
      begin
        i:= Ord(Low(TTransferString));
        while (i<=Ord(High(TTransferString))) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then Transfer[TTransferString(i)]:= str1;
          i:= i+1;
        end;//while
      end//if
      else if str1='[Others]' then
      begin
        i:= Ord(Low(TOtherString));
        while (i<=Ord(High(TOtherString))) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then Others[TOtherString(i)]:= str1;
          i:= i+1;
        end;//while
      end//if
      else if str1='[SaveLoad]' then
      begin
        i:= Ord(Low(TSaveLoadString));
        while (i<=Ord(High(TSaveLoadString))) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then SaveLoad[TSaveLoadString(i)]:= str1;
          i:= i+1;
        end;//while
      end//if
      else if str1='[Landfall]' then
      begin
        i:= 0;
        while (i<=2) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then Landfall[i]:= str1;
          i:= i+1;
        end;//while
      end//if
      else if str1='[BuildColony]' then
      begin
        i:= 0;
        while (i<=4) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then BuildColony[i]:= str1;
          i:= i+1;
        end;//while
      end//if
      else if str1='[ColonyStrings]' then
      begin
        i:= Ord(Low(TColonyString));
        while (i<=Ord(High(TColonyString))) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then ColonyStrings[TColonyString(i)]:= str1;
          i:= i+1;
        end;//while
      end//if
      else if str1='[ColonyUnit]' then
      begin
        i:= Ord(Low(TColonyUnitString));
        while (i<=Ord(High(TColonyUnitString))) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then ColonyUnit[TColonyUnitString(i)]:= str1;
          i:= i+1;
        end;//while
      end//if
      else if str1='[EuropeanPort]' then
      begin
        i:= Ord(Low(TEuroPortString));
        while (i<=Ord(High(TEuroPortString))) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then EuroPortManage[TEuroPortString(i)]:= str1;
          i:= i+1;
        end;//while
      end//if
      else if str1='[Pioneers]' then
      begin
        i:= Ord(Low(TPioneerString));
        while (i<=Ord(High(TPioneerString))) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then Pioneers[TPioneerString(i)]:= str1;
          i:= i+1;
        end;//while
      end;//if
    end;//while
    CloseFile(dat);
    Result:= True;
    SetMenuHelpers;
  end;//else
end;//func

end.
