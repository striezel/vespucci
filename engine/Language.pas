{ ***************************************************************************

    This file is part of Vespucci.
    Copyright (C) 2008, 2009, 2010, 2011  Dirk Stolle

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

unit Language;

interface

uses
  Goods, Units, Terrain, Nation, Colony {building type}, FoundingFathers,
  IndianNation, SysUtils;

type
  { enumeration type to represent a menu category }
  TMenuCategory = (mcNone, mcGame, mcView, mcOrders, mcReports, mcTrade);

  { enumeration types to identify a certain string }
  TSaveLoadString = (slsLoadChoose, slsLoadError, slsLoadSuccess, slsSaveChoose, slsSaveError, slsSaveSuccess, slsNoGameLoaded);
  TTransferString = (tsBoycotted, tsOutOfGold, tsOutOfSpace);
  TOtherString = (osLocation, osDestination, osFreight, osShip, osHighSea,
                  osNewWorld, osMoves, osEmpty, osNothing, osNoChanges, osTax,
                  osGold, osCost, osSaving, osEarnings, osName, osProgress, osUndefined);
  TEuroPortString = (epsManageHeading, epsNotOnShip, epsGoOnShip, epsArm, epsDisarm, epsGiveHorses, epsNoHorses, epsGiveTools, epsNoTools, epsTrainHeading, epsBuyHeading);
  TReportType = (rtNone, rtCongress, rtJob, rtEconomy, rtColony, rtFleet, rtForeign, rtIndian, rtScore);
  TColonyString = (csRenameQuestion, csRenameLabel, csAbandonYes, csAbandonNo, csAbandonQuestion);
  TColonyUnitString = (cusOptions, cusCancelOrders, cusOnBoard, cusFortify);
  TBuildingString = (bsUnderConstruction, bsSelectNext, bsNotify, bsMaxThree);
  TPioneerString = (psNoTools, psHasRoad, psIsPloughed, psIsCleared, psNeedsClearing, psWrongUnit);
  TReportLabelString = (//report for colonization score
                        rlsColonizationScore, rlsCitizens, rlsContinentalCongress,
                        rlsVillagesBurned, rlsTotalScore,
                        //foreign affairs report
                        rlsForeignAffairs, rlsRebels, rlsLoyalists, rlsWar,
                        rlsPeace, rlsColonies, rlsAverageColony, rlsPopulation,
                        rlsMilitaryPower, rlsNavalPower, rlsMerchantMarine,
                        //colony report
                        rlsMilitaryGarrisons, rlsSonsOfLiberty,
                        //job report
                        rlsJobReport,
                        //continental congress
                        rlsCongress, rlsNextCongress, rlsRebelAttitude,
                        rlsLoyalAttitude, rlsExpeditionForces, rlsFoundingFathers,
                        //Indians
                        rlsIndianReport, rlsIndianExterminated, rlsIndianNoData);
  TNotifyString = (nsJoinedCongress);
  TNewGameString = (ngsNewGame, ngsNewWorld, ngsAmerica, ngsLandmass, ngsLandmassSmall,
                    ngsLandmassMedium, ngsLandmassLarge, ngsTemperature,
                    ngsTemperatureCool, ngsTemperatureModerate, ngsTemperatureWarm,
                    ngsClimate, ngsClimateDry, ngsClimateNormal, ngsClimateWet,
                    ngsEuropeanPower, ngsEuropeanPowerEngland, ngsEuropeanPowerFrance,
                    ngsEuropeanPowerSpain, ngsEuropeanPowerHolland);
  TTechLevelString = (tlsLevelName, tlsOneSettlementName, tlsMultipleSettlementName);


  { ********
    **** TLanguage class
    ****
    **** purpose: holds all strings (captions, messages, names of buildings,
    ****          units, goods,...) in the game, that might be different in
    ****          different languages
    *******
  }
  TLanguage = class
    private
      //names of menu categories
      Menu: array[TMenuCategory] of string;
      //names of menu options
      MenuOptions: array [TMenuCategory] of array [1..10] of string;
      menu_helpers: array [TMenuCategory] of record
                                               max_len: Integer;
                                               count:  Integer;
                                             end;//rec
      //names of goods
      GoodNames: array[TGoodType] of string;
      //names of all nations, including Indians
      NationNames: array[cMinNations..cMaxIndian] of string;
      //default names of European leaders
      LeaderNames: array[cMinEuropean..cMaxEuropean] of string;
      //names of ports in Europe
      PortNames: array[cMinEuropean..cMaxEuropean] of string;
      //names of terrain types
      TerrainNames: array[TTerrainType] of string;
      //names of units
      UnitNames: array[TUnitType] of string;
      //names of the two seasons (spring, autumn)
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
      //for names of buildings on different levels
      Buildings: array[TBuildingType] of array [1..3] of string;
      //for units in buildings, moving them in and out, etc.
      BuildingStrings: array[TBuildingString] of string;
      //for managing units in european port
      EuroPortManage: array[TEuroPortString] of string;
      //for pioneer actions
      Pioneers: array[TPioneerString] of string;
      //for labels in reports
      ReportLabels: array[TReportLabelString] of string;
      //notification strings
      Notifications: array[TNotifyString] of string;
      //strings for new game messages/ map generation choices
      NewGame: array[TNewGameString] of string;
      //strings for Indian tech levels
      TechLevelStrings: array[TTechLevel] of array[TTechLevelString] of string;
      //strings for attitudes
      Attitudes: array[TIndianAttitude] of string;

      //names of founding fathers
      FoundingFathers: array[TFoundingFathers] of string;
      //types of founding fathers
      FoundingFatherTypes: array[TFoundingFatherType] of string;

      { sets initial values for all strings

        remarks:
            The initial strings set by this function are German, however you
            can change the language by calling LoadFromFile() and load language
            settings for another language.
      }
      procedure InitialValues;

      { sets initial names of colonies/ colony name proposals

        remarks:
            See remarks for InitialValues().
      }
      procedure InitialColonyNames;

      { sets names of buildings

        remarks:
            See remarks for InitialValues().
      }
      procedure InitialBuildingNames;

      { sets initial names of founding fathers

        remarks:
            See remarks for InitialValues().
      }
      procedure InitialFoundingFathers;

      { sets some auxillary, menu-related values }
      procedure SetMenuHelpers;

      { returns the number of menu options in a certain category

        parameters:
            categ - the menu category
      }
      function privGetOptionCount(const categ: TMenuCategory): Integer;
    public
      { constructor }
      constructor Create;
      // ---- menu related ----
      { returns the number of menu options in a certain menu category

        parameters:
            categ - the category whose number of options you want to know
      }
      function GetOptionCount(const categ: TMenuCategory): Integer;

      { returns the name/ label of a certain menu category

      parameters:
            categ - the category whose name you want to know
      }
      function GetMenuLabel(const categ: TMenuCategory): string;

      { returns the name/ label of a certain menu option

      parameters:
            categ  - the category where the option is located
            option - the number of the option (1-based)
      }
      function GetMenuOption(const categ: TMenuCategory; const option: Byte): string;

      // ---- menu helper ----
      { utility function that returns the number of characters of the longest
        option in a certain menu category

        parameters:
            categ - the menu category
      }
      function GetMaxLen(const categ: TMenuCategory): Integer;

      // ---- general stuff ----
      { returns the name of a certain good

        parameters:
            AGood - the good whose name is requested
      }
      function GetGoodName(const AGood: TGoodType): string;

      { returns the name of a certain nation

        parameters:
            NationNum - integer identifying the nation whose name is requested

        remarks:
            The value of NationNum has to be in [cMin_Nations..cMaxIndian], or
            this function just will return '(no nation)'.
      }
      function GetNationName(const NationNum: Integer): string;

      { returns the default name of a Eueopean nation's leader

        parameters:
            NationNum - integer identifying the nation whose name is requested

        remarks:
            The value of NationNum has to be in [cMinEurope..cMaxEurope], or
            this function just will return '(no leader name set)'.
      }
      function GetDefaultLeaderName(const NationNum: Integer): string;

      { returns the name of the European port of a certain nation

        parameters:
            NationNum - integer identifying the nation

        remarks:
            The value of NationNum has to be in [cMinEuropean..cMaxEuropean], or
            this function just will return '(no port)'.
      }
      function GetPortName(const NationNum: Integer): string;

      { returns the name of a terrain type

        parameters:
            ATerrain - the terrain type whose name is requested
      }
      function GetTerrainName(const ATerrain: TTerrainType): string;

      { returns the name of a unit type

        parameters:
            AUnit - the unit type whose name is requested
      }
      function GetUnitName(const AUnit: TUnitType): string;

      { returns the name of a season

        parameters:
            autumn - If set to true, the word for autumn will be returned.
                     If set to false, the word for spring will be returned.
                     The game splits years into two turns, so that there are
                     only two distinct seasons.
      }
      function GetSeason(const autumn: Boolean): string;

      { returns the name of a founding father

        parameters:
            ff - the founding father whose name is requested
      }
      function GetFoundingFatherName(const ff: TFoundingFathers): string;

      { returns the name of a founding father type

        parameters:
            fft - the fouding father type whose name is requested
      }
      function GetFoundingFatherTypeName(const fft: TFoundingFatherType): string;

      { returns a certain string that contains a message related to good
        transfer from ship to harbour or vice versa

        parameters:
            which_string - indicates which string is requested
      }
      function GetTransfer(const which_string: TTransferString): string;

      { returns "some other string", i.e. a (more or less) shorter string that
        does not fit into another message category

        parameters:
            which_one - identifies the requested string
      }
      function GetOthers(const which_one: TOtherString): string;

      { returns a certain message that is related to loading or saving the game

        parameters:
            which - indicates which of the strings is requested
      }
      function GetSaveLoad(const which: TSaveLoadString): string;

      { returns a string for the landfall message box

        parameters:
            which - number indicating which string is requested
      }
      function GetLandfall(const which: Byte): string;

      { returns a message related to the founding of a new colony

        parameters:
            which - number that indicates the requested message
      }
      function GetBuildColony(const which: Byte): string;

      { returns messages for some actions in a colony

        parameters:
            which - indicates the requested message
      }
      function GetColonyString(const which: TColonyString): string;

      { returns a name proposal for a new colony

        parameters:
            num_nation - integer that indicates the nation that owns the colony
            col_count  - number of current colonies of this nation
      }
      function GetColonyNames(const num_nation: LongInt; col_count: Byte): string;

      { returns a message string related to colonies within the colony square
        but outside of the colony

        parameters:
            which - indicates what message is requested
      }
      function GetColonyUnit(const which: TColonyUnitString): string;

      { returns the name of a certain building

        parameters:
            which - the type of the building
            level - the current construction level of the building

        remarks:
            The value of level must be within [1;3]. Otherwise a string that
            says "nothing" (or similar in the choosen language) is returned.
      }
      function GetBuildingName(const which: TBuildingType; const level: Byte): string;

      { returns a string related to construction of buildings

        parameters:
            which - indicates the requestes string/message
      }
      function GetBuildingString(const which: TBuildingString): string;

      { returns a string/message related to the European ports

        remarks:
            which - indicates the requested message
      }
      function GetEuroPort(const which: TEuroPortString): string;

      { returns a message related to pioneers and their work

        parameters:
            which - indicates the requested string
      }
      function GetPioneer(const which: TPioneerString): string;

      { returns a message related to reports

        parameters:
            which - indicates the requested string
      }
      function GetReportString(const which: TReportLabelString): string;

      { returns a notification message

        parameters:
            which - indicates the requested message
      }
      function GetNotification(const which: TNotifyString): string;

      { returns a string message related to starting a new game/ generating a
        map for a new game

        parameters:
            which - indicates the requested message
      }
      function GetNewGameString(const which: TNewGameString): string;

      { returns a string related to Indian nations' tech levels

        parameters:
            level - the tech level whose string is requested
            which - indicates the requested string
      }
      function GetTechLevelString(const level: TTechLevel; const which: TTechLevelString): string;

      { returns a string describing an Indian nations' attitude

        parameters:
            attitude - the attitude whose description is requested
      }
      function GetAttitudeString(const attitude: TIndianAttitude): string;

      { tries to save the language data to the given file and returns true in
        case of success

        parameters:
            FileName - path of the file the data will be saved to
      }
      function SaveToFile(const FileName: string): Boolean;

      { tries to load the language data from the given file and returns true in
        case of success

        parameters:
            FileName - path of the file the data will be loaded from

        remarks:
            "Success", i.e. the return value true, does not indicate how much
            data was read from the file - it just indicates that there was no
            error while reading from the file. In the worst case, the file might
            be completely empty, so that nothing was read. However, the function
            will return true in this case.
            Depending on the content of the given file, this function may also
            only load certain parts of the language data, i.e. if the file does
            not contain data for all strings that TLanguage can hold. It is
            therefore advised to call InitialValues() befor calling
            LoadFromFile(), because this way you'll have a defined content for
            the strings not covered in the given file.
      }
      function LoadFromFile(const FileName: string): Boolean;
  end;//class Language

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
  MenuOptions[mcGame, 1]:= 'Neues Spiel';
  MenuOptions[mcGame, 2]:= 'Speichern';
  MenuOptions[mcGame, 3]:= 'Laden';
  MenuOptions[mcGame, 4]:= 'Spiel beenden';
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
  MenuOptions[mcReports, 1]:= 'Kontinentalkongress';
  MenuOptions[mcReports, 2]:= 'Arbeitsbericht';
  MenuOptions[mcReports, 3]:= 'Wirtschaftsbericht';
  MenuOptions[mcReports, 4]:= 'Koloniebericht';
  MenuOptions[mcReports, 5]:= 'Flottenbericht';
  MenuOptions[mcReports, 6]:= 'Außenpolitikbericht';
  MenuOptions[mcReports, 7]:= 'Indianerberater';
  MenuOptions[mcReports, 8]:= 'Kolonialisierungspunkte';
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
  NationNames[cNationApache]:= 'Apachen';
  //leaders's default names
  LeaderNames[cNationEngland]:= 'Walter Raleigh';
  LeaderNames[cNationFrance]:= 'Jacques Cartier';
  LeaderNames[cNationSpain]:= 'Christoph Columbus';
  LeaderNames[cNationHolland]:= 'Michiel De Ruyter';
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
  Transfer[tsOutOfSpace]:= 'Unser Schiff hat nicht genug freien Laderaum, um auch diese '
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
  Others[osName]:= 'Name';
  Others[osProgress]:= 'Fortschritt';
  Others[osUndefined]:= 'Nicht definiert';
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
  //names of buildings
  InitialBuildingNames;
  //building string (not to be confused with previous one)
  for i:= Ord(Low(TBuildingString)) to Ord(High(TBuildingString)) do
    BuildingStrings[TBuildingString(i)]:= '';
  BuildingStrings[bsUnderConstruction]:= 'Im Bau';
  BuildingStrings[bsSelectNext]:= 'Wählen Sie das zu bauende Gebäude:';
  BuildingStrings[bsNotify]:= 'Ein Gebäude wurde fertiggestellt.';
  BuildingStrings[bsMaxThree]:= 'Es können maximal drei Einheiten im gleichen Gebäude arbeiten.';
  //for European ports
  EuroPortManage[epsManageHeading]:= 'Optionen für Siedler im europäischen Hafen:';
  EuroPortManage[epsNotOnShip]:= 'Nicht aufs nächste Schiff gehen';
  EuroPortManage[epsGoOnShip]:= 'An Bord des nächsten Schiffes gehen';
  EuroPortManage[epsArm]:= 'Mit Musketen bewaffnen';
  EuroPortManage[epsDisarm]:= 'Musketen verkaufen';
  EuroPortManage[epsGiveHorses]:= 'Mit Pferden ausrüsten';
  EuroPortManage[epsNoHorses]:= 'Pferde verkaufen';
  EuroPortManage[epsGiveTools]:= 'Mit Werkzeugen ausrüsten';
  EuroPortManage[epsNoTools]:= 'Werkzeuge verkaufen';
  EuroPortManage[epsTrainHeading]:= 'Die königliche Universität kann uns Spezialisten liefern,   '
                                   +'wenn wir die richtigen Leute bestechen. Welche Fertigkeit   '
                                   +'sollen wir anfordern?';
  EuroPortManage[epsBuyHeading]:= 'Welches Schiff sollen wir kaufen?';

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

  //for reports
  // --- score
  ReportLabels[rlsColonizationScore]:= 'Kolonialisierungspunkte';
  ReportLabels[rlsCitizens]:= 'Bürger';
  ReportLabels[rlsContinentalCongress]:= 'Kontinentalkongreß';
  ReportLabels[rlsVillagesBurned]:= 'Niedergebrannte Dörfer';
  ReportLabels[rlsTotalScore]:= 'Gesamtpunktzahl';
  // --- foreign affairs
  ReportLabels[rlsForeignAffairs]:= 'Außenpolitikbericht';
  ReportLabels[rlsRebels]:= 'Rebellen';
  ReportLabels[rlsLoyalists]:= 'Loyalisten';
  ReportLabels[rlsWar]:= 'Krieg';
  ReportLabels[rlsPeace]:= 'Frieden';
  ReportLabels[rlsColonies]:= 'Kolonien';
  ReportLabels[rlsAverageColony]:= 'Koloniedurchschnitt';
  ReportLabels[rlsPopulation]:= 'Bevölkerung';
  ReportLabels[rlsMilitaryPower]:= 'Militärmacht';
  ReportLabels[rlsNavalPower]:= 'Marinemacht';
  ReportLabels[rlsMerchantMarine]:= 'Handelsmarine';
  // --- colony report
  ReportLabels[rlsMilitaryGarrisons]:= 'Militärgarnison';
  ReportLabels[rlsSonsOfLiberty]:= 'Söhne der Freiheit';
  // --- job report
  ReportLabels[rlsJobReport]:= 'Arbeitsbericht';
  // --- continental congress
  ReportLabels[rlsCongress]:= 'Kontinentalkongress';
  ReportLabels[rlsNextCongress]:= 'Nächste Sitzung des Kontinentalkongresses';
  ReportLabels[rlsRebelAttitude]:= 'Rebellische Gesinnung';
  ReportLabels[rlsLoyalAttitude]:= 'Loyalistische Gesinnung';
  ReportLabels[rlsExpeditionForces]:= 'Expeditionsstreitkräfte';
  ReportLabels[rlsFoundingFathers]:= 'Gründerväter';
  // --- indian report
  ReportLabels[rlsIndianReport]:= 'Indianerberater';
  ReportLabels[rlsIndianExterminated]:= 'ausgerottet';
  ReportLabels[rlsIndianNoData]:= 'Sie haben noch keinen Kontakt zu den Ureinwohnern.';
  //notifications
  Notifications[nsJoinedCongress]:= 'Gründerväter geben bekannt, dass %s dem '
                                   +'Kontinentalkongress beigetreten ist.';
  //new game/ map generation
  NewGame[ngsNewGame]:= 'Neues Spiel:';
  NewGame[ngsNewWorld]:= 'Ein Spiel in der Neuen Welt starten';//random map
  NewGame[ngsAmerica]:= 'Ein Spiel in Amerika starten';//America map
  NewGame[ngsLandmass]:= 'Landmasse';
  NewGame[ngsLandmassSmall]:= 'klein';
  NewGame[ngsLandmassMedium]:= 'mittelgroß';
  NewGame[ngsLandmassLarge]:= 'groß';
  NewGame[ngsTemperature]:= 'Temperatur';
  NewGame[ngsTemperatureCool]:= 'kühl';
  NewGame[ngsTemperatureModerate]:= 'gemäßigt';
  NewGame[ngsTemperatureWarm]:= 'warm';
  NewGame[ngsClimate]:= 'Klima';
  NewGame[ngsClimateDry]:= 'trocken';
  NewGame[ngsClimateNormal]:= 'normal';
  NewGame[ngsClimateWet]:= 'feucht';
  NewGame[ngsEuropeanPower]:= 'Europäische Macht auswählen:';
  NewGame[ngsEuropeanPowerEngland]:= 'England (Immigration)';
  NewGame[ngsEuropeanPowerFrance]:= 'Frankreich (Kooperation)';
  NewGame[ngsEuropeanPowerSpain]:= 'Spanien (Eroberung)';
  NewGame[ngsEuropeanPowerHolland]:= 'Holland (Handel)';
  //Tech levels
  TechLevelStrings[tlNomadic, tlsLevelName]:= 'halb-nomadisch';
  TechLevelStrings[tlNomadic, tlsOneSettlementName]:= 'ein Lager';
  TechLevelStrings[tlNomadic, tlsMultipleSettlementName]:= 'Lager';
  TechLevelStrings[tlAgricultural, tlsLevelName]:= 'landwirtschaftlich';
  TechLevelStrings[tlAgricultural, tlsOneSettlementName]:= 'ein Dorf';
  TechLevelStrings[tlAgricultural, tlsMultipleSettlementName]:= 'Dörfer';
  TechLevelStrings[tlDeveloped, tlsLevelName]:= 'weiterentwickelt';
  TechLevelStrings[tlDeveloped, tlsOneSettlementName]:= 'eine Stadt';
  TechLevelStrings[tlDeveloped, tlsMultipleSettlementName]:= 'Städte';
  TechLevelStrings[tlCivilised, tlsLevelName]:= 'zivilisiert';
  TechLevelStrings[tlCivilised, tlsOneSettlementName]:= 'eine Stadt';
  TechLevelStrings[tlCivilised, tlsMultipleSettlementName]:= 'Städte';
  //attitudes
  Attitudes[iaPleased]:= 'zufrieden';
  Attitudes[iaWorried]:= 'besorgt';
  Attitudes[iaAnxious]:= 'unruhig';
  Attitudes[iaAngry]:= 'verärgert';
  Attitudes[iaBelligerent]:= 'kriegerisch';
  //founding fathers
  InitialFoundingFathers;

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

procedure TLanguage.InitialBuildingNames;
var i,j: Integer;
begin
{TBuildingType = (btNone, //nichts, dummy
                   btFort, //Einpfählung, Fort, Festung
                   btDock, //Hafenanlagen, Trockendock, Werft
                   btWarehouse, //Lagerhaus, Lagerhauserweiterung
                   btStable, //Ställe
                   btCustomHouse, //Zollhaus
                   btPress, //Druckerei, Verlag
                   btSchool, //Schule, College, Universität
                   btArmory, //Waffenkammer, Waffendepot, Waffenarsenal
                   btTownHall, //Rathaus
                   btWeaver, //Haus d. Webers, Weberei, Textilwerk
                   btTobacconist, //Haus d. Tabakhändlers, Tabakladen, Zigarrenfabrik
                   btDistiller, //Haus d. Rumbrenners, Rumbrennerei, Rumfabrik
                   btFurTrader, //Haus d. Gerbers, Gerberei, Pelzfabrik
                   btCarpenter, //Zimmerei, Sägewerk
                   btChurch, //Kirche, Kathedrale
                   btBlacksmith //Haus d. Schmieds, Schmiede, Eisenhütte
                  );}
  //initialise all to empty strings (saves us from setting unused strings afterwards)
  for i:= Ord(Low(TBuildingType)) to Ord(High(TBuildingType)) do
    for j:= 1 to 3 do
      Buildings[TBuildingType(i),j]:= '';

  //"real" content
  Buildings[btNone, 1]:= 'nichts';
  Buildings[btNone, 2]:= 'nichts';
  Buildings[btNone, 3]:= 'nichts';

  Buildings[btFort,1]:= 'Einpfählung';
  Buildings[btFort,2]:= 'Fort';
  Buildings[btFort,3]:= 'Festung';
  Buildings[btDock,1]:= 'Hafenanlagen';
  Buildings[btDock,2]:= 'Trockendock';
  Buildings[btDock,3]:= 'Werft';

  Buildings[btWarehouse,1]:= 'Lagerhaus';
  Buildings[btWarehouse,2]:= 'Lagerhauserweiterung';
  Buildings[btStable,1]:= 'Ställe';
  Buildings[btCustomHouse,1]:= 'Zollhaus';
  Buildings[btPress, 1]:= 'Druckerei';
  Buildings[btPress, 2]:= 'Verlag';

  Buildings[btSchool,1]:= 'Schule';
  Buildings[btSchool,2]:= 'College';
  Buildings[btSchool,3]:= 'Universität';
  Buildings[btArmory,1]:= 'Waffenkammer';
  Buildings[btArmory,2]:= 'Waffendepot';
  Buildings[btArmory,3]:= 'Waffenarsenal';
  Buildings[btTownhall,1]:= 'Rathaus';

  Buildings[btWeaver,1]:= 'Haus des Webers';
  Buildings[btWeaver,2]:= 'Weberei';
  Buildings[btWeaver,3]:= 'Textilwerk';

  Buildings[btTobacconist,1]:= 'Haus des Tabakhändlers';
  Buildings[btTobacconist,2]:= 'Tabakladen';
  Buildings[btTobacconist,3]:= 'Zigarrenfabrik';

  Buildings[btDistiller,1]:= 'Haus des Rumbrenners';
  Buildings[btDistiller,2]:= 'Rumbrennerei';
  Buildings[btDistiller,3]:= 'Rumfabrik';

  Buildings[btFurTrader,1]:= 'Haus des Gerbers';
  Buildings[btFurTrader,2]:= 'Pelzhandelsposten';
  Buildings[btFurTrader,3]:= 'Pelzfabrik';

  Buildings[btCarpenter,1]:= 'Zimmerei';
  Buildings[btCarpenter,2]:= 'Sägewerk';

  Buildings[btChurch,1]:= 'Kirche';
  Buildings[btChurch,2]:= 'Kathedrale';

  Buildings[btBlacksmith,1]:= 'Haus des Schmieds';
  Buildings[btBlacksmith,2]:= 'Schmiede';
  Buildings[btBlacksmith,3]:= 'Eisenhütte';
end;//proc

procedure TLanguage.InitialFoundingFathers;
begin
  //trade
  FoundingFathers[ffSmith]:= 'Adam Smith';
  FoundingFathers[ffFugger]:= 'Jakob Fugger';
  FoundingFathers[ffMinuit]:= 'Peter Minuit';
  FoundingFathers[ffStuyvesant]:= 'Peter Stuyvesant';
  FoundingFathers[ffDeWitt]:= 'Jan de Witt';

  //exploration
  FoundingFathers[ffCoronado]:= 'Francisco Coronado';
  FoundingFathers[ffHudson]:= 'Henry Hudson';
  FoundingFathers[ffLaSalle]:= 'La Salle';
  FoundingFathers[ffMagellan]:= 'Ferdinand Magellan';
  FoundingFathers[ffDeSoto]:= 'Hernando de Soto';

  //military
  FoundingFathers[ffCortes]:= 'Hernando Cortes';
  FoundingFathers[ffDrake]:= 'Francis Drake';
  FoundingFathers[ffJones]:= 'John Paul Jones';
  FoundingFathers[ffRevere]:= 'Paul Revere';
  FoundingFathers[ffWashington]:= 'George Washington';

  //political
  FoundingFathers[ffBolivar]:= 'Simon Bolivar';
  FoundingFathers[ffFranklin]:= 'Benjamin Franklin';
  FoundingFathers[ffJefferson]:= 'Thomas Jefferson';
  FoundingFathers[ffPaine]:= 'Thomas Paine';
  FoundingFathers[ffPocahontas]:= 'Pocahontas';

  //religious
  FoundingFathers[ffBrebeuf]:= 'Jean de Brebeuf';
  FoundingFathers[ffBrewster]:= 'William Brewster';
  FoundingFathers[ffLasCasas]:= 'Bartolome de las Casas';
  FoundingFathers[ffPenn]:= 'William Penn';
  FoundingFathers[ffSepulveda]:= 'Juan de Sepulveda';
  //none
  FoundingFathers[ffNone]:= Others[osNothing];

  //types of founding fathers
  FoundingFatherTypes[fftTrade]:= 'Handel';
  FoundingFatherTypes[fftExploration]:= 'Erkundung';
  FoundingFatherTypes[fftMilitary]:= 'Militär';
  FoundingFatherTypes[fftPolitical]:= 'Politik';
  FoundingFatherTypes[fftReligious]:= 'Religion';
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
  if (NationNum<cMinNations) or (NationNum>cMaxIndian) then Result:= '(no nation)'
  else Result:= NationNames[NationNum];
end;//func

function TLanguage.GetDefaultLeaderName(const NationNum: Integer): string;
begin
  if (NationNum<cMinEuropean) or (NationNum>cMaxEuropean) then Result:= '(no leader name set)'
  else Result:= LeaderNames[NationNum];
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

function TLanguage.GetFoundingFatherName(const ff: TFoundingFathers): string;
begin
  Result:= FoundingFathers[ff];
end;//func

function TLanguage.GetFoundingFatherTypeName(const fft: TFoundingFatherType): string;
begin
  Result:= FoundingFatherTypes[fft];
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

function TLanguage.GetBuildingName(const which: TBuildingType; const level: Byte): string;
begin
  case level of
    0: Result:= GetOthers(osNothing);
    1..3: begin
            Result:= Buildings[which, level];
            if (Result='') then Result:= GetOthers(osUndefined);
          end;//1..3
  else Result:= GetOthers(osUndefined);
  end;//case
end;//func

function TLanguage.GetBuildingString(const which: TBuildingString): string;
begin
  Result:= BuildingStrings[which];
end;//func

function TLanguage.GetEuroPort(const which: TEuroPortString): string;
begin
  Result:= EuroPortManage[which];
end;//func

function TLanguage.GetPioneer(const which: TPioneerString): string;
begin
  Result:= Pioneers[which];
end;//func

function TLanguage.GetReportString(const which: TReportLabelString): string;
begin
  Result:= ReportLabels[which];
end;//func

function TLanguage.GetNotification(const which: TNotifyString): string;
begin
  Result:= Notifications[which];
end;//func

function TLanguage.GetNewGameString(const which: TNewGameString): string;
begin
  Result:= NewGame[which];
end;//func

function TLanguage.GetTechLevelString(const level: TTechLevel; const which: TTechLevelString): string;
begin
  Result:= TechLevelStrings[level, which];
end;//func

function TLanguage.GetAttitudeString(const attitude: TIndianAttitude): string;
begin
  Result:= Attitudes[attitude];
end;//func

function TLanguage.SaveToFile(const FileName: string): Boolean;
var dat: TextFile;
    i, j: Integer;
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
  for i:= cMinNations to cMaxIndian do
    WriteLn(dat, NationNames[i]);
  WriteLn(dat);
  WriteLn(dat, '[Leaders]');
  for i:= cMinEuropean to cMaxEuropean do
    WriteLn(dat, LeaderNames[i]);
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
  WriteLn(dat, '[FoundingFathers]');
  for i:= Ord(Low(TFoundingFathers)) to Ord(High(TFoundingFathers)) do
    WriteLn(dat, FoundingFathers[TFoundingFathers(i)]);
  WriteLn(dat);
  WriteLn(dat, '[FoundingFatherTypes]');
  for i:= Ord(Low(TFoundingFatherType)) to Ord(High(TFoundingFatherType)) do
    WriteLn(dat, FoundingFatherTypes[TFoundingFatherType(i)]);
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
  WriteLn(dat);
  WriteLn(dat, '[ReportLabels]');
  for i:= Ord(Low(TReportLabelString)) to Ord(High(TReportLabelString)) do
    WriteLn(dat, ReportLabels[TReportLabelString(i)]);
  WriteLn(dat);
  WriteLn(dat, '[Notifications]');
  for i:= Ord(Low(TNotifyString)) to Ord(High(TNotifyString)) do
    WriteLn(dat, Notifications[TNotifyString(i)]);
  WriteLn(dat);
  WriteLn(dat, '[NewGame]');
  for i:= Ord(Low(TNewGameString)) to Ord(High(TNewGameString)) do
    WriteLn(dat, NewGame[TNewGameString(i)]);
  WriteLn(dat);
  WriteLn(dat, '[Buildings]');
  for i:= Ord(Succ(btNone)) to Ord(High(TBuildingType)) do
  begin
    for j:= 1 to 3 do
      if (j<=GetMaxBuildingLevel(TBuildingType(i))) then
        WriteLn(dat, Buildings[TBuildingType(i),j]);
  end;//for
  WriteLn(dat);
  WriteLn(dat, '[BuildingStrings]');
  for i:= Ord(Low(TBuildingString)) to Ord(High(TBuildingString)) do
    WriteLn(dat, BuildingStrings[TBuildingString(i)]);
  WriteLn(dat);
  WriteLn(dat, '[TechLevels]');
  for i:= Ord(Low(TTechLevel)) to Ord(High(TTechLevel)) do
    for j:= Ord(Low(TTechLevelString)) to Ord(High(TTechLevelString)) do
      WriteLn(dat, TechLevelStrings[TTechLevel(i), TTechLevelString(j)]);
  WriteLn(dat);
  WriteLn(dat, '[Attitudes]');
  for i:= Ord(Low(TIndianAttitude)) to Ord(High(TIndianAttitude)) do
    WriteLn(dat, Attitudes[TIndianAttitude(i)]);
  CloseFile(dat);
  Result:= True;
end;//func

function TLanguage.LoadFromFile(const FileName: string): Boolean;
var dat: TextFile;
    str1: AnsiString;
    i, j: Integer;
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
        i:= cMinNations;
        while (i<=cMaxIndian) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then NationNames[i]:= str1;
          i:= i+1;
        end;//while
      end//if
      else if str1='[Leaders]' then
      begin
        i:= cMinEuropean;
        while (i<=cMaxEuropean) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then LeaderNames[i]:= str1;
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
      else if str1='[FoundingFathers]' then
      begin
        i:= Ord(Low(TFoundingFathers));
        while (i<=Ord(High(TFoundingFathers))) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then FoundingFathers[TFoundingFathers(i)]:= str1;
          i:= i+1;
        end;//while
      end//if
      else if str1='[FoundingFatherTypes]' then
      begin
        i:= Ord(Low(TFoundingFatherType));
        while (i<=Ord(High(TFoundingFatherType))) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then FoundingFatherTypes[TFoundingFatherType(i)]:= str1;
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
      end//if
      else if str1='[ReportLabels]' then
      begin
        i:= Ord(Low(TReportLabelString));
        while (i<=Ord(High(TReportLabelString))) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then ReportLabels[TReportLabelString(i)]:= str1;
          i:= i+1;
        end;//while
      end//if
      else if str1='[Notifications]' then
      begin
        i:= Ord(Low(TNotifyString));
        while (i<=Ord(High(TNotifyString))) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then Notifications[TNotifyString(i)]:= str1;
          i:= i+1;
        end;//while
      end//if
      else if str1='[NewGame]' then
      begin
        i:= Ord(Low(TNewGameString));
        while (i<=Ord(High(TNewGameString))) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then NewGame[TNewGameString(i)]:= str1;
          i:= i+1;
        end;//while
      end//if
      else if str1='[Buildings]' then
      begin
        i:= Ord(Succ(btNone));
        while (i<=Ord(High(TBuildingType))) and not Eof(dat) do
        begin
          for j:= 1 to 3 do
          begin
            if (j<=GetMaxBuildingLevel(TBuildingType(i))) then
            begin
              ReadLn(dat, str1);
              str1:= Trim(str1);
              if str1<>'' then Pioneers[TPioneerString(i)]:= str1;
            end;//if
          end;//for j
          i:= i+1;
        end;//while
      end//if
      else if str1='[BuildingStrings]' then
      begin
        i:= Ord(Low(TBuildingString));
        while (i<=Ord(High(TBuildingString))) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then BuildingStrings[TBuildingString(i)]:= str1;
          i:= i+1;
        end;//while
      end//if
      else if str1='[TechLevels]' then
      begin
        i:= Ord(Low(TTechLevel));
        while (i<=Ord(High(TTechLevel))) and not Eof(dat) do
        begin
          for j:= Ord(Low(TTechLevelString)) to Ord(High(TTechLevelString)) do
          begin
            ReadLn(dat, str1);
            str1:= Trim(str1);
            if str1<>'' then TechLevelStrings[TTechLevel(i), TTechLevelString(j)]:= str1;
          end;//for j
          i:= i+1;
        end;//while
      end//if
      else if str1='[Attitudes]' then
      begin
        i:= Ord(Low(TIndianAttitude));
        while (i<=Ord(High(TIndianAttitude))) and not Eof(dat) do
        begin
          ReadLn(dat, str1);
          str1:= Trim(str1);
          if str1<>'' then Attitudes[TIndianAttitude(i)]:= str1;
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
