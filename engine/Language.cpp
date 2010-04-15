#include "Language.h"

int Ord(const TMenuCategory mcc)
{
  return static_cast<int> (mcc);
}

TMenuCategory High(const TMenuCategory mcc)
{
  return mcTrade;
}

TMenuCategory Low(const TMenuCategory mcc)
{
  return mcNone;
}

TMenuCategory Succ(const TMenuCategory mcc)
{
  if (mcc<mcTrade) return static_cast<TMenuCategory> (mcc+1);
  throw 42;
}

TMenuCategory Pred(const TMenuCategory mcc)
{
  if (mcc>mcNone) return static_cast<TMenuCategory> (mcc-1);
  throw 42;
}

int Ord(const TBuildingString bss)
{
  return static_cast<int> (bss);
}

TBuildingString Low(const TBuildingString bss)
{
  return bsUnderConstruction;
}

TBuildingString High(const TBuildingString bss)
{
  return bsMaxThree;
}

TBuildingString Succ(const TBuildingString bss)
{
  if (bss<bsMaxThree) return static_cast<TBuildingString> (bss+1);
  throw 42;
}


TLanguage::TLanguage()
{
  InitialValues();
}//func

TLanguage::~TLanguage()
{
  //empty
}

TLanguage& TLanguage::GetSingleton()
{
  static TLanguage Instance;
  return Instance;
}

void TLanguage::InitialValues()
{
  Menu[mcNone] = "";
  Menu[mcGame] = "Spiel";
  Menu[mcView] = "Ansicht";
  Menu[mcOrders] = "Befehle";
  Menu[mcReports] = "Berichte";
  Menu[mcTrade] = "Handel";
  //options (empty strings)
  LongInt i,j;
  for (i= Ord(Low(mcNone)); i<=Ord(High(mcNone)); ++i)
    for (j= 1; j<=10; ++j)
      MenuOptions[TMenuCategory(i)][j] = "";
  //present options
  // -- Spiel
  MenuOptions[mcGame][1] = "Speichern";
  MenuOptions[mcGame][2] = "Laden";
  MenuOptions[mcGame][3] = "Spiel beenden";
  // -- Ansicht
  MenuOptions[mcView][1] = "Europa-Status";
  MenuOptions[mcView][2] = "Ansicht zentrieren";
  // -- Befehle
  MenuOptions[mcOrders][1] = "Befestigen";
  MenuOptions[mcOrders][2] = "Gehe zu";
  MenuOptions[mcOrders][3] = "Wald roden";
  MenuOptions[mcOrders][4] = "Felder pflügen";
  MenuOptions[mcOrders][5] = "Straße bauen";
  MenuOptions[mcOrders][6] = "Keine Befehle";
  MenuOptions[mcOrders][7] = "Einheit auflösen";
  // -- Berichte
  MenuOptions[mcReports][1] = "Wirtschaftsbericht";
  MenuOptions[mcReports][2] = "Koloniebericht";
  MenuOptions[mcReports][3] = "Flottenbericht";
  // -- Handel
  MenuOptions[mcTrade][1] = "Handelsroute festlegen";
  MenuOptions[mcTrade][2] = "Handelsroute ändern";
  MenuOptions[mcTrade][3] = "Handelsroute löschen";

  //goods
  GoodNames[gtFood] = "Nahrungsmittel";
  GoodNames[gtSugar] = "Zuckerrohr";
  GoodNames[gtTobacco] = "Tabak";
  GoodNames[gtCotton] = "Baumwolle";
  GoodNames[gtFur] = "Felle";
  GoodNames[gtWood] = "Nutzholz";
  GoodNames[gtOre] = "Erz";
  GoodNames[gtSilver] = "Silber";
  GoodNames[gtHorses] = "Pferde";
  GoodNames[gtRum] = "Rum";
  GoodNames[gtCigar] = "Zigarren";
  GoodNames[gtCloth] = "Stoff";
  GoodNames[gtCoat] = "Mäntel";
  GoodNames[gtTradegoods] = "Handelswaren";
  GoodNames[gtTool] = "Werkzeuge";
  GoodNames[gtMusket] = "Musketen";
  GoodNames[gtHammer] = "Hämmer";
  GoodNames[gtLibertyBell] = "Freiheitsglocken";
  GoodNames[gtCross] = "Kreuze";
  //Nations
  // - Europeans
  NationNames[cNationEngland] = "England";
  NationNames[cNationFrance] = "Frankreich";
  NationNames[cNationSpain] = "Spanien";
  NationNames[cNationHolland] = "Holland";
  // - Indians
  NationNames[cNationArawak] = "Arukaner";
  NationNames[cNationAztec] = "Azteken";
  NationNames[cNationInca] = "Inka";
  NationNames[cNationTupi] = "Tupi";
  NationNames[cNationCherokee] = "Cherokee";
  NationNames[cNationIroquois] = "Irokesen";
  NationNames[cNationSioux] = "Sioux";
  NationNames[cNationApache] = "Apache";
  //ports in europe
  PortNames[cNationEngland] = "London";
  PortNames[cNationFrance] = "La Rochelle";
  PortNames[cNationSpain] = "Sevilla";
  PortNames[cNationHolland] = "Amsterdam";
  //units
  UnitNames[utCriminal] = "Kleinkriminelle";
  UnitNames[utServant] = "Verdingte Knechte";
  UnitNames[utColonist] = "Freie Siedler";
  UnitNames[utFarmer] = "Farmer";
  UnitNames[utFisher] = "Fischer";
  UnitNames[utFurHunter] = "Pelzjäger";
  UnitNames[utSilverMiner] = "Silberbergarbeiter";
  UnitNames[utWoodcutter] = "Holzfäller";
  UnitNames[utOreMiner] = "Erzbergarbeiter";
  UnitNames[utSugarplanter] = "Zuckerpflanzer";
  UnitNames[utCottonplanter] = "Baumwollpflanzer";
  UnitNames[utTobaccoplanter] = "Tabakpflanzer";
  UnitNames[utPreacher] = "Prediger";
  UnitNames[utStatesman] = "Staatsmann";
  UnitNames[utCarpenter] = "Schreiner";
  UnitNames[utDistiller] = "Rumbrenner";
  UnitNames[utWeaver] = "Weber";
  UnitNames[utTobacconist] = "Zigarrenhändler";
  UnitNames[utFurTrader] = "Pelzhändler";
  UnitNames[utSmith] = "Schmied";
  UnitNames[utWeaponSmith] = "Waffenschmied";
  UnitNames[utScout] = "Späher";
  UnitNames[utPioneer] = "Pioniere";
  UnitNames[utMissionary] = "Missionar";
  UnitNames[utRegular] = "Reguläre";
  UnitNames[utDragoon] = "Dragoner";
  UnitNames[utArtillery] = "Artillerie";
  UnitNames[utConvoy] = "Wagenzug";
  UnitNames[utCaravel] = "Karavelle";
  UnitNames[utTradingShip] = "Handelsschiff";
  UnitNames[utGalleon] = "Galleone";
  UnitNames[utPrivateer] = "Kaperschiff";
  UnitNames[utFrigate] = "Fregatte";
  UnitNames[utMan_o_War] = "Kriegsschiff";
  UnitNames[utBrave] = "Krieger";
  UnitNames[utBraveOnHorse] = "Berittener Krieger";
  //terrain types
  TerrainNames[ttArctic] = "Arktisch";
  TerrainNames[ttSea] = "Ozean";
  TerrainNames[ttOpenSea] = "Seeweg";
  TerrainNames[ttPlains] = "Flachland";
  TerrainNames[ttGrassland] = "Grünland";
  TerrainNames[ttPrairie] = "Prärie";
  TerrainNames[ttSavannah] = "Savanne";
  TerrainNames[ttMarsh] = "Feuchtgebiete";
  TerrainNames[ttSwamp] = "Sumpfland";
  TerrainNames[ttDesert] = "Wüste";
  TerrainNames[ttTundra] = "Tundra";
  TerrainNames[ttBoreal] = "Borealwald";
  TerrainNames[ttWetland] = "Feuchtwald";
  TerrainNames[ttScrubForest] = "Gestrüppwald";
  TerrainNames[ttBroadleaf] = "Laubwald";
  TerrainNames[ttMixedForest] = "Mischwald";
  TerrainNames[ttConiferForest] = "Nadelwald";
  TerrainNames[ttRainForest] = "Regenwald";
  TerrainNames[ttTropicalForest] = "Tropenwald";
  TerrainNames[ttHills] = "Hügellandschaft";
  TerrainNames[ttMountains] = "Berge";
  //seasons
  Seasons[0] = "Frühling";
  Seasons[1] = "Herbst";
  //good transfers
  Transfer[tsBoycotted] = "Diese Ware wird momentan vom Parlament boykottiert, Eure    "
                          +std::string("Exzellenz. Wir können damit nicht handeln, bis das Parlament")
                          +"den Boykott aufhebt.";
  Transfer[tsOutOfGold] = "Wir haben nicht genug Gold, um uns 100 Einheiten der gewün- "
                          +std::string("schten Ware zu leisten, Eure Exzellenz.");
  Transfer[tsOutOfSpace] = "Unser Schiff hat nicht genug freien Laderaum, um noch diese "
                           +std::string("Ware noch laden zu können.");
  //others
  Others[osLocation] = "Ort";
  Others[osDestination] = "Ziel";
  Others[osFreight] = "Fracht";
  Others[osShip] = "Schiff";
  Others[osHighSea] = "Hohe See";
  Others[osNewWorld] = "Neue Welt";
  Others[osMoves] = "Züge";
  Others[osEmpty] = "leer";
  Others[osNothing] = "nichts";
  Others[osNoChanges] = "Keine Veränderungen";
  Others[osTax] = "Steuer";
  Others[osGold] = "Gold";
  Others[osCost] = "Kosten";
  Others[osSaving] = "Einsparung";
  Others[osEarnings] = "Gewinn";
  Others[osUndefined] = "Nicht definiert";
  //save/ load messages
  SaveLoad[slsLoadChoose] = "Wählen Sie den zu ladenden Spielstand.";
  SaveLoad[slsLoadError] = "Fehler beim Laden des Spielstandes! Das geladene Spiel kann "
                          +std::string("unter Umständen unvorhersehbare Fehler verursachen, daher   ")
                          +"wird das aktuelle Spiel beendet. Versuchen Sie, ein anderes "
                          +"Spiel neu zu laden oder das Programm erneut zu starten.";
  SaveLoad[slsLoadSuccess] = "Spiel wurde erfolgreich geladen!";
  SaveLoad[slsSaveChoose] = "Wählen Sie den Slot, in welchem das Spiel gespeichert werden"
                           +std::string("soll.");
  SaveLoad[slsSaveError] = "Fehler beim Speichern des Spieles! Die gespeicherten Daten  "
                          +std::string("sind möglicherweise unbrauchbar und können beim Versuch,    ")
                          +"diese zu laden, zum Spielabbruch führen.";
  SaveLoad[slsSaveSuccess] = "Das Spiel wurde gespeichert.";
  SaveLoad[slsNoGameLoaded] = "Es ist kein Spiel geladen, welches gespeichert werden       "
                             +std::string("könnte.");
  //landfall
  Landfall[0] = "Sollen wir an Land gehen, Eure Exzellenz, und die Schiffe zurücklassen?";
  Landfall[1] = "Bei den Schiffen bleiben";
  Landfall[2] = "An Land gehen";
  //build colony
  BuildColony[0] = "Wie sollen wir diese Kolonie nennen?";
  BuildColony[1] = "Name:";
  // -- build colony error messages
  BuildColony[2] = "Sie können keine Kolonie im Wasser oder vom Schiff aus er-  "
                  +std::string("richten, sondern müssen erst Sielder an Land schicken.");
  BuildColony[3] = "Dieses Land liegt für eine neue Kolonie zu nah an einer     "
                  +std::string("schon bestehenden Kolonie, Eure Exzellenz.");
  BuildColony[4] = "Kolonien können nicht in den Bergen gebaut werden, Eure Exzellenz.";
  //renaming colonies & abandon colony
  ColonyStrings[csRenameQuestion] = "Wie soll diese Kolonie jetzt heißen?";
  ColonyStrings[csRenameLabel] = "Neuer Name";
  ColonyStrings[csAbandonYes] = "Ja, es ist mein Wille.";
  ColonyStrings[csAbandonNo] = "Nein, das wäre töricht!";
  ColonyStrings[csAbandonQuestion] = "Sollen wir unsere Kolonie wirklich aufgeben, Eure Exzellenz, so dass all unsere harte Arbeit hier umsonst war?";
  //units outside of colony but still in colony square
  ColonyUnit[cusOptions] = "Optionen für";
  ColonyUnit[cusCancelOrders] = "Befehle aufheben";
  ColonyUnit[cusOnBoard] = "Wache/ An Bord gehen";
  ColonyUnit[cusFortify] = "Befestigen";
  //colony names
  for (i= cMinEuropean; i<=cMaxEuropean; ++i)
    ColonyNames[i].clear();
  InitialColonyNames();
  //names of buildings
  InitialBuildingNames();
  //building string (not to be confused with previous one)
  for (i= Ord(Low(bsMaxThree)); i<=Ord(High(bsMaxThree)); ++i)
    BuildingStrings[TBuildingString(i)] = "";
  BuildingStrings[bsUnderConstruction] = "Im Bau";
  BuildingStrings[bsSelectNext] = "Wählen Sie das zu bauende Gebäude:";
  BuildingStrings[bsNotify] = "Ein Gebäude wurde fertiggestellt.";
  BuildingStrings[bsMaxThree] = "Es können maximal drei Einheiten im gleichen Gebäude arbeiten.";
  //for European ports
  EuroPortManage[epsManageHeading] = "Optionen für Siedler im europäischen Hafen:";
  EuroPortManage[epsNotOnShip] = "Nicht aufs nächste Schiff gehen";
  EuroPortManage[epsGoOnShip] = "An Bord des nächsten Schiffes gehen";
  EuroPortManage[epsArm] = "Mit Musketen bewaffnen";
  EuroPortManage[epsDisarm] = "Musketen verkaufen";
  EuroPortManage[epsGiveHorses] = "Mit Pferden ausrüsten";
  EuroPortManage[epsNoHorses] = "Pferde verkaufen";
  EuroPortManage[epsGiveTools] = "Mit Werkzeugen ausrüsten";
  EuroPortManage[epsNoTools] = "Werkzeuge verkaufen";
  EuroPortManage[epsTrainHeading] = "Die königliche Universität kann uns Spezialisten liefern,   "
                                   +std::string("wenn wir die richtigen Leute bestechen. Welche Fertigkeit   ")
                                   +"sollen wir anfordern?";
  EuroPortManage[epsBuyHeading] = "Welches Schiff sollen wir kaufen?";

  //for pionieers
  Pioneers[psNoTools] = "Die Einheit hat nicht genug Werkzeuge, um diese Aktion aus- "
                       +std::string("zuführen. Es sind mindestens 20 Werkzeuge nötig.");
  Pioneers[psHasRoad] = "Hier gibt es schon eine Straße, Eure Exzellenz.";
  Pioneers[psIsPloughed] = "Dieses Gebiet ist schon gepflügt, Eure Exzellenz.";
  Pioneers[psIsCleared] = "Dieses Gelände ist schon gerodet, Eure Exzellenz.";
  Pioneers[psNeedsClearing] = "Das Gebiet muss erst gerodet werden, bevor wir es pflügen   "
                             +std::string("können, Eure Exzellenz.");
  Pioneers[psWrongUnit] = "Nur Siedler oder Pioniere, welche mit Werkzeugen ausgerüstet"
                         +std::string(" sind, können diese Aktion durchführen, Eure Exzellenz.");

  SetMenuHelpers();
}//proc

void TLanguage::InitialColonyNames()
{
  ColonyNames[cNationEngland].clear();
  ColonyNames[cNationEngland].push_back("Jamestown");
  ColonyNames[cNationEngland].push_back("Plymouth");
  ColonyNames[cNationEngland].push_back("Roanoke");
  ColonyNames[cNationEngland].push_back("Barbados");
  ColonyNames[cNationEngland].push_back("Penobscot");
  ColonyNames[cNationEngland].push_back("Boston");
  ColonyNames[cNationEngland].push_back("Baltimore");
  ColonyNames[cNationEngland].push_back("Providence");
  ColonyNames[cNationEngland].push_back("Hartford");
  ColonyNames[cNationEngland].push_back("New Haven");
  ColonyNames[cNationEngland].push_back("New York");
  ColonyNames[cNationEngland].push_back("Albany");
  ColonyNames[cNationEngland].push_back("New Jersey");
  ColonyNames[cNationEngland].push_back("Charleston");
  ColonyNames[cNationEngland].push_back("Philadelphia");
  ColonyNames[cNationEngland].push_back("Newport");

  ColonyNames[cNationFrance].clear();
  ColonyNames[cNationFrance].push_back("Quebec");
  ColonyNames[cNationFrance].push_back("Montreal");
  ColonyNames[cNationFrance].push_back("Guadelupe");
  ColonyNames[cNationFrance].push_back("Cayenne");
  ColonyNames[cNationFrance].push_back("St. Louis");
  ColonyNames[cNationFrance].push_back("Martinique");
  ColonyNames[cNationFrance].push_back("Port Royal");
  ColonyNames[cNationFrance].push_back("Port au Prince");
  ColonyNames[cNationFrance].push_back("Trois Rivieres");
  ColonyNames[cNationFrance].push_back("New Orleans");
  ColonyNames[cNationFrance].push_back("Fort Caroline");
  ColonyNames[cNationFrance].push_back("Fort Detroit");
  ColonyNames[cNationFrance].push_back("Fort Frontenac");
  ColonyNames[cNationFrance].push_back("Fort Pontchartain");
  ColonyNames[cNationFrance].push_back("Fort Tadoussac");
  ColonyNames[cNationFrance].push_back("Fort Canada");

  ColonyNames[cNationSpain].clear();
  ColonyNames[cNationSpain].push_back("Isabella");
  ColonyNames[cNationSpain].push_back("Santo Domingo");
  ColonyNames[cNationSpain].push_back("San Salvador");
  ColonyNames[cNationSpain].push_back("Veracruz");
  ColonyNames[cNationSpain].push_back("Habana");
  ColonyNames[cNationSpain].push_back("Trinidad");
  ColonyNames[cNationSpain].push_back("San Juan");
  ColonyNames[cNationSpain].push_back("Panama");
  ColonyNames[cNationSpain].push_back("Cartagena");
  ColonyNames[cNationSpain].push_back("St. Augustine");
  ColonyNames[cNationSpain].push_back("Lima");
  ColonyNames[cNationSpain].push_back("Buenos Aires");
  ColonyNames[cNationSpain].push_back("Guatemala");
  ColonyNames[cNationSpain].push_back("Honduras");
  ColonyNames[cNationSpain].push_back("Santiago");
  ColonyNames[cNationSpain].push_back("Asuncion");

  ColonyNames[cNationHolland].clear();
  ColonyNames[cNationHolland].push_back("New Amsterdam");
  ColonyNames[cNationHolland].push_back("Fort Orange");
  ColonyNames[cNationHolland].push_back("Fort Nassau");
  ColonyNames[cNationHolland].push_back("Neuholland");
  ColonyNames[cNationHolland].push_back("Vlissingen");
  ColonyNames[cNationHolland].push_back("Curaco");
  ColonyNames[cNationHolland].push_back("Recife");
  ColonyNames[cNationHolland].push_back("Bahia");
  ColonyNames[cNationHolland].push_back("Paramaribo");
  ColonyNames[cNationHolland].push_back("Pernambuco");
  ColonyNames[cNationHolland].push_back("St. Martin");
  ColonyNames[cNationHolland].push_back("Surinam");
  ColonyNames[cNationHolland].push_back("Willemstad");
  ColonyNames[cNationHolland].push_back("Aruba");
  ColonyNames[cNationHolland].push_back("Utrecht");
  ColonyNames[cNationHolland].push_back("Haarlem");
}//proc

void TLanguage::InitialBuildingNames()
{
/*TBuildingType = (btNone, //nichts, dummy
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
                  );*/
  //initialise all to empty strings (saves us from setting unused strings afterwards)
  LongInt i,j;
  for (i= Ord(Low(btNone)); i<=Ord(High(btNone)); ++i)
    for (j= 1; j<=3; ++j)
      Buildings[TBuildingType(i)][j] = "";

  //"real" content
  Buildings[btNone][1] = "nichts";
  Buildings[btNone][2] = "nichts";
  Buildings[btNone][3] = "nichts";

  Buildings[btFort][1] = "Einpfählung";
  Buildings[btFort][2] = "Fort";
  Buildings[btFort][3] = "Festung";
  Buildings[btDock][1] = "Hafenanlagen";
  Buildings[btDock][2] = "Trockendock";
  Buildings[btDock][3] = "Werft";

  Buildings[btWarehouse][1] = "Lagerhaus";
  Buildings[btWarehouse][2] = "Lagerhauserweiterung";
  Buildings[btStable][1] = "Ställe";
  Buildings[btCustomHouse][1] = "Zollhaus";
  Buildings[btPress][1] = "Druckerei";
  Buildings[btPress][2] = "Verlag";

  Buildings[btSchool][1] = "Schule";
  Buildings[btSchool][2] = "College";
  Buildings[btSchool][3] = "Universität";
  Buildings[btArmory][1] = "Waffenkammer";
  Buildings[btArmory][2] = "Waffendepot";
  Buildings[btArmory][3] = "Waffenarsenal";
  Buildings[btTownHall][1] = "Rathaus";

  Buildings[btWeaver][1] = "Haus des Webers";
  Buildings[btWeaver][2] = "Weberei";
  Buildings[btWeaver][3] = "Textilwerk";

  Buildings[btTobacconist][1] = "Haus des Tabakhändlers";
  Buildings[btTobacconist][2] = "Tabakladen";
  Buildings[btTobacconist][3] = "Zigarrenfabrik";

  Buildings[btDistiller][1] = "Haus des Rumbrenners";
  Buildings[btDistiller][2] = "Rumbrennerei";
  Buildings[btDistiller][3] = "Rumfabrik";

  Buildings[btFurTrader][1] = "Haus des Gerbers";
  Buildings[btFurTrader][2] = "Pelzhandelsposten";
  Buildings[btFurTrader][3] = "Pelzfabrik";

  Buildings[btCarpenter][1] = "Zimmerei";
  Buildings[btCarpenter][2] = "Sägewerk";

  Buildings[btChurch][1] = "Kirche";
  Buildings[btChurch][2] = "Kathedrale";

  Buildings[btBlacksmith][1] = "Haus des Schmieds";
  Buildings[btBlacksmith][2] = "Schmiede";
  Buildings[btBlacksmith][3] = "Eisenhütte";
}//proc

void TLanguage::SetMenuHelpers()
{
  LongInt i, j;
  for (i= Ord(Low(mcTrade)); i<=Ord(High(mcTrade)); ++i)
  {
    menu_helpers[TMenuCategory(i)].count = privGetOptionCount(TMenuCategory(i));
    //determine max len
    menu_helpers[TMenuCategory(i)].max_len = GetMenuLabel(TMenuCategory(i)).length()-2;
    for (j= 1; j<=menu_helpers[TMenuCategory(i)].count; ++j)
    {
      LongInt temp = GetMenuOption(TMenuCategory(i), j).length();
      if (temp>menu_helpers[TMenuCategory(i)].max_len)
        menu_helpers[TMenuCategory(i)].max_len = temp;
    }//for
  }//for
}//func

LongInt TLanguage::privGetOptionCount(const TMenuCategory categ) const
{
  LongInt Result = 0;
  LongInt i;
  for (i=1; i<=10; ++i)
  {
   if (MenuOptions[categ][i]!="") Result = i; else break;
  }//for
  return Result;
}//func

LongInt TLanguage::GetOptionCount(const TMenuCategory categ) const
{
  return menu_helpers[categ].count;
}//func

std::string TLanguage::GetMenuLabel(const TMenuCategory categ) const
{
  return Menu[categ];
}//func

std::string TLanguage::GetMenuOption(const TMenuCategory categ, const Byte option) const
{
  if ((option>=1) and (option<=10)) return MenuOptions[categ][option];
  return "";
}//func

LongInt TLanguage::GetMaxLen(const TMenuCategory categ) const
{
  return menu_helpers[categ].max_len;
}//func

std::string TLanguage::GetGoodName(const TGoodType AGood) const
{
  return GoodNames[AGood];
}//func

std::string TLanguage::GetNationName(const LongInt NationNum) const
{
  if ((NationNum<cMin_Nations) or (NationNum>cMaxIndian)) return "(no nation)";
  return NationNames[NationNum];
}//func

std::string TLanguage::GetPortName(const LongInt NationNum) const
{
  if ((NationNum<cMinEuropean) or (NationNum>cMaxEuropean)) return "(no port name)";
  return PortNames[NationNum];
}//func

std::string TLanguage::GetTerrainName(const TTerrainType ATerrain) const
{
  return TerrainNames[ATerrain];
}//func

std::string TLanguage::GetUnitName(const TUnitType AUnit) const
{
  return UnitNames[AUnit];
}//func

std::string TLanguage::GetSeason(const bool autumn) const
{
  if (autumn) return Seasons[1];
  return Seasons[0];
}//func

std::string TLanguage::GetTransfer(const TTransferString which_string) const
{
  return Transfer[which_string];
}//func

std::string TLanguage::GetOthers(const TOtherString which_one) const
{
  return Others[which_one];
}//func

std::string TLanguage::GetSaveLoad(const TSaveLoadString which) const
{
  return SaveLoad[which];
}//func

std::string TLanguage::GetLandfall(const Byte which) const
{
  if (which>2) return "";
  return Landfall[which];
}//func

std::string TLanguage::GetBuildColony(const Byte which) const
{
  if (which>4) return "";
  return BuildColony[which];
}//func

std::string TLanguage::GetColonyString(const TColonyString which) const
{
  return ColonyStrings[which];
}//func

std::string TLanguage::GetColonyNames(const LongInt num_nation, const Byte col_count) const
{
  if ((num_nation>=cMinEuropean) and (num_nation<=cMaxEuropean))
  {
    if (col_count<ColonyNames[num_nation].size()) return ColonyNames[num_nation].at(col_count);
    return GetNationName(num_nation)+" #"+IntToStr(col_count);
  }
  return "(colony name)";
}//func

std::string TLanguage::GetColonyUnit(const TColonyUnitString which) const
{
  return ColonyUnit[which];
}//func

std::string TLanguage::GetBuildingName(const TBuildingType which, const Byte level) const
{
  switch (level)
  {
    case 0: return GetOthers(osNothing);
    case 1:
    case 2:
    case 3:
         if(""==Buildings[which][level]) return GetOthers(osUndefined);
         return Buildings[which][level];
    default:
         return GetOthers(osUndefined);
  }//case
}//func

std::string TLanguage::GetBuildingString(const TBuildingString which) const
{
  return BuildingStrings[which];
}//func

std::string TLanguage::GetEuroPort(const TEuroPortString which) const
{
  return EuroPortManage[which];
}//func

std::string TLanguage::GetPioneer(const TPioneerString which) const
{
  return Pioneers[which];
}//func

/*bool TLanguage::SaveToFile(const std::string& FileName) const
var dat: TextFile;
    i, j: Integer;
{
  std::ofstream dat;
  dat.open(FileName.c_str());
  if (!dat)
    return false;

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
  CloseFile(dat);
  Result:= True;
}//func

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
      end;//if
    end;//while
    CloseFile(dat);
    Result:= True;
    SetMenuHelpers;
  end;//else
end;//func */

