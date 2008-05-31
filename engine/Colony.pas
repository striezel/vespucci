unit Colony;

interface

uses
  Nation, Settlement, Goods, Units, Map{, Terrain};

type
  TBuildingType = (btFort, //Einpfählung, Fort, Festung
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
                  );

  TColony = class(TSettlement)
    public
      constructor Create(const X, Y: Integer; const ANation: PNation; const AName: string);
      destructor Destroy;
      function GetName: string;
      function GetMaxLevel(const bt: TBuildingType): Byte;
      procedure ConstructNextLevel(const bt: TBuildingType);
      function GetProduction(const bt: TBuildingType; const ut: TUnitType): Integer;
      procedure NewRound(const AMap: TMap);

    private
      m_Name: string;
      Store: array[TGoodType] of Word; //max. is 300, a word should do it;
      Buildings: array[TBuildingType] of Byte;
      UnitsInBuilding: array [btArmory..btBlacksmith] of array [0..2] of PUnit;
      UnitsInFields: array[-1..1] of array [-1..1] of record
                                                        u: PUnit;
                                                        GoesFor: TGoodType;
                                                      end;//rec
  end;//class
  PColony = ^TColony;

implementation

// **** TColony functions ****

constructor TColony.Create(const X, Y: Integer; const ANation: PNation; const AName: string);
var bt: TBuildingType;
    gt: TGoodType;
    i,j: Integer;
begin
  //sets position and nation
  inherited Create(X, Y, ANation);
  //set name
  m_Name:= AName;
  //set all goods to zero
  gt:= gtFood;
  while gt<High(TGoodType) do
  begin
    Store[gt]:= 0;
    gt:= Succ(gt);
  end;//while
  Store[High(TGoodType)]:= 0;
  //clear all buildings
  bt:= btFort;
  while bt<High(TBuildingType) do
  begin
    Buildings[bt]:= 0;
    bt:= Succ(bt);
    //clear units in buildings
    if (bt in [btArmory..btBlacksmith]) then
    begin
      UnitsInBuilding[bt,0]:= nil;
      UnitsInBuilding[bt,1]:= nil;
      UnitsInBuilding[bt,2]:= nil;
    end;//if
  end;//while
  Buildings[High(TBuildingType)]:= 0;
  //set initial buildins
  Buildings[btTownHall]:= 1;
  Buildings[btCarpenter]:= 1;
  Buildings[btBlacksmith]:= 1;
  Buildings[btTobacconist]:= 1;
  Buildings[btWeaver]:= 1;
  Buildings[btDistiller]:= 1;
  Buildings[btFurTrader]:= 1;
  //units in fields
  for i:= -1 to 1 do
    for j:= -1 to 1 do
    begin
      UnitsInFields[i,j].u:= nil;
      UnitsInFields[i,j].GoesFor:= gtFood;
    end;//for
end;//create

destructor TColony.Destroy;
begin
  //what shall we do?
  inherited Destroy;
end;//destruc

function TColony.GetName: string;
begin
  Result:= m_Name;
end;//func

function TColony.GetMaxLevel(const bt: TBuildingType): Byte;
begin
  case bt of
    btTownHall, btStable, btCustomHouse: Result:= 1;
    btWarehouse, btPress, btCarpenter, btChurch: Result:= 2;
  else Result:= 3;
  end;//case
end;//func

procedure TColony.ConstructNextLevel(const bt: TBuildingType);
begin
  if (Buildings[bt]<GetMaxLevel(bt)) then
    Buildings[bt]:= Buildings[bt]+1;
end;//proc

function TColony.GetProduction(const bt: TBuildingType; const ut: TUnitType): Integer;
begin
  case ut of
    utCriminal: Result:= 1;
    utServant: Result:= 2;
  else Result:= 3;
  end;//case

  //consider level of building
  if (bt in [btArmory..btBlacksmith]) then
    Result:= Result * Buildings[bt]
  else
    Result:= 0;

  //check for specialist's bonus
  case bt of
    btArmory: if (ut=utWeaponSmith) then Result:= Result*2;
    btTownHall: begin
                  if (ut=utStatesman) then Result:= Result*2;
                  if Buildings[btPress]=2 then Result:= (Result*9) div 4
                  else if Buildings[btPress]=1 then Result:= (Result*3) div 2;
                end;//case btTownHall
    btWeaver: if (ut=utWeaver) then Result:= Result*2;
    btTobacconist: if (ut=utTobacconist) then Result:= Result*2;
    btDistiller: if (ut=utDistiller) then Result:= Result*2;
    btFurTrader: if (ut=utFurTrader) then Result:= Result*2;
    btCarpenter: if (ut=utCarpenter) then Result:= Result*2;
    btChurch: if (ut=utPreacher) then Result:= Result*2;
    btBlacksmith: if (ut=utSmith) then Result:= Result*2;
  end;//case
end;//func

//only calculates the good changes due to production in buildings;
// (production in fields (i.e. by farmers) not included yet)
procedure TColony.NewRound(const AMap: TMap);
var i,j, prod: Integer;
begin
  //calculate production of units in surrounding fields
  if AMap<>nil then
  begin
    for i:= -1 to 1 do
      for j:= -1 to 1 do
        if (UnitsInFields[i,j].u<>nil) then
          if ((self.PosX+i>=0) and (self.PosX+i<cMap_X) and (self.PosY+j>=0) and (self.PosY+j<cMap_Y)) then
            Store[UnitsInFields[i,j].GoesFor]:= Store[UnitsInFields[i,j].GoesFor] +
             AMap.tiles[self.PosX+i, self.PosY+j].GetGoodProduction(UnitsInFields[i,j].GoesFor);
  end;//if
  //calculate production of all buildings
  //church first
  prod:= 0;
  for i:=0 to 2 do
    if UnitsInBuilding[btChurch][i]<>nil then
      prod:=prod+GetProduction(btChurch, UnitsInBuilding[btChurch][i]^.GetType);
  Store[gtCross]:= Store[gtCross]+prod;
  //town hall second
  prod:= 0;
  for i:=0 to 2 do
    if UnitsInBuilding[btTownHall][i]<>nil then
      prod:=prod+GetProduction(btTownHall, UnitsInBuilding[btTownHall][i]^.GetType);
  Store[gtLibertyBell]:= Store[gtLibertyBell]+prod;
  //carpenter third
  prod:= 0;
  for i:=0 to 2 do
    if UnitsInBuilding[btCarpenter][i]<>nil then
      prod:=prod+GetProduction(btCarpenter, UnitsInBuilding[btCarpenter][i]^.GetType);
  if Store[gtWood]<prod then prod:= Store[gtWood];
  Store[gtWood]:= Store[gtWood]-prod;
  Store[gtHammer]:= Store[gtHammer]+prod;
  //Blacksmith fourth
  prod:= 0;
  for i:=0 to 2 do
    if UnitsInBuilding[btBlacksmith][i]<>nil then
      prod:=prod+GetProduction(btBlacksmith, UnitsInBuilding[btBlacksmith][i]^.GetType);
  if Store[gtOre]<prod then prod:= Store[gtOre];
  Store[gtOre]:= Store[gtOre]-prod;
  Store[gtTool]:= Store[gtTool]+prod;
  //armory fifth
  prod:= 0;
  for i:=0 to 2 do
    if UnitsInBuilding[btArmory][i]<>nil then
      prod:=prod+GetProduction(btArmory, UnitsInBuilding[btArmory][i]^.GetType);
  if Store[gtTool]<prod then prod:= Store[gtTool];
  Store[gtTool]:= Store[gtTool]-prod;
  Store[gtMusket]:= Store[gtMusket]+prod;
  //fur traders sixth
  prod:= 0;
  for i:=0 to 2 do
    if UnitsInBuilding[btFurTrader][i]<>nil then
      prod:=prod+GetProduction(btFurTrader, UnitsInBuilding[btFurTrader][i]^.GetType);
  if Store[gtFur]<prod then prod:= Store[gtFur];
  Store[gtFur]:= Store[gtFur]-prod;
  Store[gtCoat]:= Store[gtCoat]+prod;
  //weaver seventh
  prod:= 0;
  for i:=0 to 2 do
    if UnitsInBuilding[btWeaver][i]<>nil then
      prod:=prod+GetProduction(btWeaver, UnitsInBuilding[btWeaver][i]^.GetType);
  if Store[gtCotton]<prod then prod:= Store[gtCotton];
  Store[gtCotton]:= Store[gtCotton]-prod;
  Store[gtCloth]:= Store[gtCloth]+prod;
  //tobacconist eight
  prod:= 0;
  for i:=0 to 2 do
    if UnitsInBuilding[btTobacconist][i]<>nil then
      prod:=prod+GetProduction(btTobacconist, UnitsInBuilding[btTobacconist][i]^.GetType);
  if Store[gtTobacco]<prod then prod:= Store[gtTobacco];
  Store[gtTobacco]:= Store[gtTobacco]-prod;
  Store[gtCigar]:= Store[gtCigar]+prod;
  //distiller ninth
  prod:= 0;
  for i:=0 to 2 do
    if UnitsInBuilding[btDistiller][i]<>nil then
      prod:=prod+GetProduction(btDistiller, UnitsInBuilding[btDistiller][i]^.GetType);
  if Store[gtSugar]<prod then prod:= Store[gtSugar];
  Store[gtSugar]:= Store[gtSugar]-prod;
  Store[gtRum]:= Store[gtRum]+prod;

  //cut the amount we can't store off
  for i:= Ord(Low(TGoodType)) to Ord(High(TGoodType)) do
    if (not(TGoodType(i) in [gtFood, gtHammer, gtLibertyBell, gtCross])
         and (Store[TGoodType(i)]>(1+buildings[btWarehouse])*100)) then
      Store[TGoodType(i)]:= (1+buildings[btWarehouse])*100;
end;//func

end.