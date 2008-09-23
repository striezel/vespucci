unit Colony;

interface

uses
  Settlement, Goods, Units, Map, Classes, Helper;

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
      constructor Create(const X, Y: Integer; const ANation: Integer; const AName: string);
      destructor Destroy;
      function GetName: string;
      procedure SetName(const new_name: string);
      function GetStore(const AGood: TGoodType): Word;
      function RemoveFromStore(const AGood: TGoodType; const amount: Byte): Byte;
      procedure AddToStore(const AGood: TGoodType; const amount: Byte);
      //try to avoid SetStore, use RemoveFromStore or AddToStore instead
      procedure SetStore(const AGood: TGoodType; const new_amount: Word);
      function GetMaxLevel(const bt: TBuildingType): Byte;
      procedure SetBuildingLevel(const bt: TBuildingType; const new_level: Byte);
      procedure ConstructNextLevel(const bt: TBuildingType);
      function GetProduction(const bt: TBuildingType; const ut: TUnitType): Integer;
      procedure NewRound(const AMap: TMap);
      function GetUnitInField(const x_shift, y_shift: Integer): TUnit;
      function GetUnitInFieldGood(const x_shift, y_shift: Integer): TGoodType;
      procedure SetUnitInField(const x_shift, y_shift: Integer; const AUnit: TUnit; const AGood: TGoodType=gtFood);
      function GetUnitInBuilding(const bt: TBuildingType; const place: Byte): TUnit;
      procedure SetUnitInBuilding(const bt: TBuildingType; const place: Byte; const AUnit: TUnit);
      function GetInhabitants: Word;
      function SaveToStream(var fs: TFileStream): Boolean;
    private
      m_Name: string;
      Store: array[TGoodType] of Word; //max. is 300, a word should do it;
      Buildings: array[TBuildingType] of Byte;
      UnitsInBuilding: array [btArmory..btBlacksmith] of array [0..2] of TUnit;
      UnitsInFields: array[-1..1] of array [-1..1] of record
                                                        u: TUnit;
                                                        GoesFor: TGoodType;
                                                      end;//rec
  end;//class
  PColony = ^TColony;

implementation

// **** TColony functions ****

constructor TColony.Create(const X, Y: Integer; const ANation: Integer; const AName: string);
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
var i,j: Integer;
begin
  //what shall we do?
  //set Units in fields free
  for i:= -1 to 1 do
    for j:= -1 to 1 do
      if UnitsInFields[i,j].u<>nil then
      begin
        UnitsInFields[i,j].u.WarpToXY(GetPosX, GetPosY, nil);
        UnitsInFields[i,j].u.SetLocation(ulAmerica);
        UnitsInFields[i,j].u:= nil;
      end;//func
  //set units in buildings free
  for i:= Ord(btArmory) to Ord(btBlacksmith) do
    for j:= 0 to 2 do
      if UnitsInBuilding[TBuildingType(i),j]<>nil then
      begin
        UnitsInBuilding[TBuildingType(i),j].WarpToXY(GetPosX, GetPosY, nil);
        UnitsInBuilding[TBuildingType(i),j].SetLocation(ulAmerica);
        UnitsInBuilding[TBuildingType(i),j]:= nil;
      end;//func
  inherited Destroy;
end;//destruc

function TColony.GetName: string;
begin
  Result:= m_Name;
end;//func

procedure TColony.SetName(const new_name: string);
begin
  if Trim(new_name)<>'' then m_Name:= Trim(new_name);
end;//func

function TColony.GetStore(const AGood: TGoodType): Word;
begin
  Result:= Store[AGood];
end;//func

function TColony.RemoveFromStore(const AGood: TGoodType; const amount: Byte): Byte;
begin
  if Store[AGood]>=amount then
  begin
    Store[AGood]:= Store[AGood]-amount;
    Result:= amount;
  end
  else begin
    Result:= Store[AGood];
    Store[AGood]:= 0;
  end;//else
end;//func

procedure TColony.AddToStore(const AGood: TGoodType; const amount: Byte);
begin
  //no function, it always succeeds. However, storage amount is cut to maximum
  // storage capacity during next call to TColony.NewRound.
  Store[AGood]:= Store[AGood]+amount;
end;//func

procedure TColony.SetStore(const AGood: TGoodType; const new_amount: Word);
begin
  Store[AGood]:= new_amount;
end;//proc

function TColony.GetMaxLevel(const bt: TBuildingType): Byte;
begin
  case bt of
    btTownHall, btStable, btCustomHouse: Result:= 1;
    btWarehouse, btPress, btCarpenter, btChurch: Result:= 2;
  else Result:= 3;
  end;//case
end;//func

procedure TColony.SetBuildingLevel(const bt: TBuildingType; const new_level: Byte);
begin
  if new_level>GetMaxLevel(bt) then Buildings[bt]:= GetMaxLevel(bt)
  else Buildings[bt]:= new_level;
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
  if AMap<>nil then
  begin
    //calculate production of units in surrounding fields
    for i:= -1 to 1 do
      for j:= -1 to 1 do
        if (UnitsInFields[i,j].u<>nil) then
          if ((self.PosX+i>=0) and (self.PosX+i<cMap_X) and (self.PosY+j>=0) and (self.PosY+j<cMap_Y)) then
            Store[UnitsInFields[i,j].GoesFor]:= Store[UnitsInFields[i,j].GoesFor] +
             AMap.tiles[self.PosX+i, self.PosY+j].GetGoodProduction(UnitsInFields[i,j].GoesFor);
    //calculate production in base (=central) field
    Store[gtFood]:= Store[gtFood]+AMap.tiles[PosX, PosY].GetColonyFood;
    Store[AMap.tiles[PosX, PosY].GetColonyGoodType]:=
          Store[AMap.tiles[PosX, PosY].GetColonyGoodType] +AMap.tiles[PosX, PosY].GetColonyGoodAmount;
  end;//if
  //calculate production of all buildings
  //church first
  prod:= 0;
  for i:=0 to 2 do
    if UnitsInBuilding[btChurch][i]<>nil then
      prod:=prod+GetProduction(btChurch, UnitsInBuilding[btChurch][i].GetType);
  Store[gtCross]:= Store[gtCross]+prod;
  //town hall second
  prod:= 0;
  for i:=0 to 2 do
    if UnitsInBuilding[btTownHall][i]<>nil then
      prod:=prod+GetProduction(btTownHall, UnitsInBuilding[btTownHall][i].GetType);
  Store[gtLibertyBell]:= Store[gtLibertyBell]+prod;
  //carpenter third
  prod:= 0;
  for i:=0 to 2 do
    if UnitsInBuilding[btCarpenter][i]<>nil then
      prod:=prod+GetProduction(btCarpenter, UnitsInBuilding[btCarpenter][i].GetType);
  if Store[gtWood]<prod then prod:= Store[gtWood];
  Store[gtWood]:= Store[gtWood]-prod;
  Store[gtHammer]:= Store[gtHammer]+prod;
  //Blacksmith fourth
  prod:= 0;
  for i:=0 to 2 do
    if UnitsInBuilding[btBlacksmith][i]<>nil then
      prod:=prod+GetProduction(btBlacksmith, UnitsInBuilding[btBlacksmith][i].GetType);
  if Store[gtOre]<prod then prod:= Store[gtOre];
  Store[gtOre]:= Store[gtOre]-prod;
  Store[gtTool]:= Store[gtTool]+prod;
  //armory fifth
  prod:= 0;
  for i:=0 to 2 do
    if UnitsInBuilding[btArmory][i]<>nil then
      prod:=prod+GetProduction(btArmory, UnitsInBuilding[btArmory][i].GetType);
  if Store[gtTool]<prod then prod:= Store[gtTool];
  Store[gtTool]:= Store[gtTool]-prod;
  Store[gtMusket]:= Store[gtMusket]+prod;
  //fur traders sixth
  prod:= 0;
  for i:=0 to 2 do
    if UnitsInBuilding[btFurTrader][i]<>nil then
      prod:=prod+GetProduction(btFurTrader, UnitsInBuilding[btFurTrader][i].GetType);
  if Store[gtFur]<prod then prod:= Store[gtFur];
  Store[gtFur]:= Store[gtFur]-prod;
  Store[gtCoat]:= Store[gtCoat]+prod;
  //weaver seventh
  prod:= 0;
  for i:=0 to 2 do
    if UnitsInBuilding[btWeaver][i]<>nil then
      prod:=prod+GetProduction(btWeaver, UnitsInBuilding[btWeaver][i].GetType);
  if Store[gtCotton]<prod then prod:= Store[gtCotton];
  Store[gtCotton]:= Store[gtCotton]-prod;
  Store[gtCloth]:= Store[gtCloth]+prod;
  //tobacconist eight
  prod:= 0;
  for i:=0 to 2 do
    if UnitsInBuilding[btTobacconist][i]<>nil then
      prod:=prod+GetProduction(btTobacconist, UnitsInBuilding[btTobacconist][i].GetType);
  if Store[gtTobacco]<prod then prod:= Store[gtTobacco];
  Store[gtTobacco]:= Store[gtTobacco]-prod;
  Store[gtCigar]:= Store[gtCigar]+prod;
  //distiller ninth
  prod:= 0;
  for i:=0 to 2 do
    if UnitsInBuilding[btDistiller][i]<>nil then
      prod:=prod+GetProduction(btDistiller, UnitsInBuilding[btDistiller][i].GetType);
  if Store[gtSugar]<prod then prod:= Store[gtSugar];
  Store[gtSugar]:= Store[gtSugar]-prod;
  Store[gtRum]:= Store[gtRum]+prod;

  //cut the amount we can't store off
  for i:= Ord(Low(TGoodType)) to Ord(High(TGoodType)) do
    if (not(TGoodType(i) in [gtFood, gtHammer, gtLibertyBell, gtCross])
         and (Store[TGoodType(i)]>(1+buildings[btWarehouse])*100)) then
      Store[TGoodType(i)]:= (1+buildings[btWarehouse])*100;

  //check for inhabitants and food needed
  i:= GetInhabitants*2;
  if RemoveFromStore(gtFood, i)<>i then
  begin
    //not enough food - we should put a message and probably let an inhabitant
    // starve from hunger here.
  end;
end;//func

function TColony.GetUnitInField(const x_shift, y_shift: Integer): TUnit;
begin
  if (abs(x_shift)>1) or (abs(y_shift)>1) then Result:= nil
  else Result:= UnitsInFields[x_shift, y_shift].u;
end;//func

function TColony.GetUnitInFieldGood(const x_shift, y_shift: Integer): TGoodType;
begin
  if (abs(x_shift)>1) or (abs(y_shift)>1) then Result:= gtFood
  else Result:= UnitsInFields[x_shift, y_shift].GoesFor;
end;//func

procedure TColony.SetUnitInField(const x_shift, y_shift: Integer; const AUnit: TUnit; const AGood: TGoodType=gtFood);
begin
  if (abs(x_shift)>1) or (abs(y_shift)>1) then Exit
  else begin
    //remove old unit
    if UnitsInFields[x_shift, y_shift].u<>nil then
    begin
      UnitsInFields[x_shift, y_shift].u.WarpToXY(GetPosX, GetPosY, nil);
      UnitsInFields[x_shift, y_shift].u.SetLocation(ulAmerica);
    end;//if
    //place new unit
    UnitsInFields[x_shift, y_shift].u:= AUnit;
    UnitsInFields[x_shift, y_shift].GoesFor:= AGood;
    if AUnit<>nil then UnitsInFields[x_shift, y_shift].u.SetLocation(ulInColony);
  end;//else
end;//proc

function TColony.GetUnitInBuilding(const bt: TBuildingType; const place: Byte): TUnit;
begin
  if ((place>2) or (not (bt in [btArmory..btBlacksmith]))) then Result:= nil
  else Result:= UnitsInBuilding[bt, place];
end;//func

procedure TColony.SetUnitInBuilding(const bt: TBuildingType; const place: Byte; const AUnit: TUnit);
begin
  if ((place>2) or (not (bt in [btArmory..btBlacksmith]))) then Exit
  else begin
    //remove old unit
    if UnitsInBuilding[bt, place]<>nil then
    begin
      UnitsInBuilding[bt, place].WarpToXY(GetPosX, GetPosY, nil);
      UnitsInBuilding[bt, place].SetLocation(ulAmerica);
    end;//if
    //place new unit
    UnitsInBuilding[bt, place]:= AUnit;
    if AUnit<>nil then UnitsInBuilding[bt, place].SetLocation(ulInColony);
  end;//else
end;//proc

function TColony.GetInhabitants: Word;
var i, j: Integer;
begin
  Result:=0;
  for i:= -1 to 1 do
    for j:= -1 to 1 do
      if UnitsInFields[i,j].u<>nil then Result:= Result+1;
  for i:= Ord(btArmory) to Ord(btBlacksmith) do
    for j:= 0 to 2 do
      if UnitsInBuilding[TBuildingType(i),j]<>nil then Result:= Result+1;
end;//func

function TColony.SaveToStream(var fs: TFileStream): Boolean;
var i,j: Integer;
    field_count, temp_b: Byte;
    bt: TBuildingType;
begin
  if fs=nil then
  begin
    Result:= False;
    Exit;
  end;//if
  Result:= (fs.Write(m_Nation, sizeof(Integer))=sizeof(Integer));
  Result:= Result and (fs.Write(PosX, sizeof(Integer))=sizeof(Integer));
  Result:= Result and (fs.Write(PosY, sizeof(Integer))=sizeof(Integer));
  //name
  i:= length(m_Name);
  Result:= Result and (fs.Write(i, sizeof(Integer))=sizeof(Integer));
  Result:= Result and (fs.Write(m_Name[1], length(m_Name))=length(m_Name));
  //store
  for i:= Ord(Low(TGoodType)) to Ord(High(TGoodType)) do
    Result:= Result and (fs.Write(Store[TGoodType(i)], sizeof(Word))=sizeof(Word));
  //buildings
  for i:= Ord(Low(TBuildingType)) to Ord(High(TBuildingType)) do
    Result:= Result and (fs.Write(Buildings[TBuildingType(i)], sizeof(Byte))=sizeof(Byte));
  //save units in fields
  //-- count them
  field_count:= 0;
  for i:= -1 to 1 do
    for j:= -1 to 1 do
      if UnitsInFields[i,j].u<>nil then field_count:= field_count+1;
  Result:= Result and (fs.Write(field_count, sizeof(Byte))=sizeof(Byte));
  // -- save them
  for i:= -1 to 1 do
    for j:= -1 to 1 do
      if UnitsInFields[i,j].u<>nil then
      begin
        Result:= Result and (fs.Write(i, sizeof(Integer))=sizeof(Integer));
        Result:= Result and (fs.Write(j, sizeof(Integer))=sizeof(Integer));
        Result:= Result and UnitsInFields[i,j].u.SaveToStream(fs);
        Result:= Result and (fs.Write(UnitsInFields[i,j].GoesFor, sizeof(TGoodType))=sizeof(TGoodType));
      end;//for
  //save units in buildings
  // -- count them
  field_count:= 0;
  for i:= Ord(btArmory) to Ord(btBlacksmith) do
    for j:= 0 to 2 do
      if UnitsInBuilding[TBuildingType(i), j]<>nil then field_count:= field_count+1;
  Result:= Result and (fs.Write(field_count, sizeof(Byte))=sizeof(Byte));
  // -- save them
  for i:= Ord(btArmory) to Ord(btBlacksmith) do
    for temp_b:= 0 to 2 do
      if UnitsInBuilding[TBuildingType(i), temp_b]<>nil then
      begin
        bt:= TBuildingType(i);
        Result:= Result and (fs.Write(bt, sizeof(TBuildingType))=sizeof(TBuildingType));
        Result:= Result and (fs.Write(temp_b, sizeof(Byte))=sizeof(Byte));
        Result:= Result and UnitsInBuilding[TBuildingType(i),temp_b].SaveToStream(fs);
      end;//if
end;//func

end.