unit language;

interface

uses
  Goods, Units;

type
  TMenuCategory = (mcNone, mcGame, mcView, mcOrders, mcReports, mcTrade);
  TLanguage = class
    private
      Menu: array[TMenuCategory] of string;
      MenuOptions: array [TMenuCategory] of array [1..10] of string;
      GoodNames: array[TGoodType] of string;
      UnitNames: array[TUnitType] of string;
      procedure InitialValues;
    public
      constructor Create;
      function GetOptionCount(const categ: TMenuCategory): Integer;
      function GetMenuLabel(const categ: TMenuCategory): string;
      function GetGoodName(const AGood: TGoodType): string;
      function GetUnitName(const AUnit: TUnitType): string;
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

function TLanguage.GetUnitName(const AUnit: TUnitType): string;
begin
  Result:= UnitNames[AUnit];
end;

end.
