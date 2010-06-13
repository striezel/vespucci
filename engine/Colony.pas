unit Colony;

interface

uses
  Settlement, Goods, Units, Map, Classes, Helper;

type
  { enumeration type to identify the different buildings of a colony }
  TBuildingType = (btNone, //nichts, dummy
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
                  );


  { ********
    **** TColony class
    ****
    **** purpose: represents an European settlement within the game, i.e. a
    ****          colony. Specialised version of TSettlement.
    *******
  }
  TColony = class(TSettlement)
    public
      { constructor
      
        parameters:
            X, Y    - position of the colony
            ANation - integer constant that identifies the nation who founded
                      the colony
            AName   - name of the colony
      }
      constructor Create(const X, Y: Integer; const ANation: LongInt; const AName: string);

      { destructor }
      destructor Destroy; override;

      { returns the name of the colony }
      function GetName: string;

      { sets a name for the colony
      
        parameters:
            new_name - the new name of the colony

        remarks:
            Do not use this to set the name directly. Use the constructor
            parameter to set the name. This is only used during loading process
            or name change.
      }
      procedure SetName(const new_name: string);

      { returns the amount of a certain good within the colony's storage
      
        parameters:
            AGood - the type of good you want to check for
      }
      function GetStore(const AGood: TGoodType): Word;
      
      { removes a certain amount of a certain good from the colony's storage
        and returns the amount that was actually removed
      
        parameters:
            AGood  - the type of good that has to be removed
            amount - the amount that has to be removed
      }
      function RemoveFromStore(const AGood: TGoodType; const amount: Word): Word;

      { adds a certain amount of a certain good to the colony's storage
      
        parameters:
            AGood  - the type of good that has to be added
            amount - the amount that has to be added
            
        remarks:
            This function will always add the given amount of a good to the
            storage (except in case of range overflow). However, each storage
            can only hold a certain amount of a good (the maximum is 300), and
            everything above that amount will be discarded at the next end of
            a turn.
      }
      procedure AddToStore(const AGood: TGoodType; const amount: Word);

      { directly sets the amount of a certain good in the colony's storage

        parameters:
            AGood      - the type of good that has to be set
            new_amount - the amount that has to be set

        remarks:
            Try to avoid SetStore, use RemoveFromStore or AddToStore instead.
            This function is only used during the loading process.
      }
      procedure SetStore(const AGood: TGoodType; const new_amount: Word);

      //for buildings
      { returns the current construction level of a certain building
      
        parameters:
            bt - the type of building
      }
      function GetBuildingLevel(const bt: TBuildingType): Byte;

     { set the current construction level of a certain building
      
        parameters:
            bt        - the type of building whose level has to be set
            new_level - the new level of that building
            
        remarks:
            Do not use this procedure directly, it's only used druning loading
            process.
      }
      procedure SetBuildingLevel(const bt: TBuildingType; const new_level: Byte);

      { returns the type of building that is currently constructed in the
        colony
      }
      function GetCurrentConstruction: TBuildingType;

      { sets the type of building that is currently constructed in the colony
      
        parameters:
            bt - the type of building that shall be constructed
      }
      procedure SetCurrentConstruction(const bt: TBuildingType);

      { constructs the next level of the currently constructed building and }
      procedure ConstructNextLevel;

      { returns the amount of goods that is currently prodcued in a certain
        building by a certain unit in that colony

        remarks:
            bt - the type of the building
            ut - the type of the unit
      }
      function GetProduction(const bt: TBuildingType; const ut: TUnitType): Integer;

      { starts a new round for that colony, i.e. adds produced goods to storage
        and so on

        remarks:
            AMap - the current map (needed to calculate production in some fields
      }
      procedure NewRound(const AMap: TMap);

      { returns the type of unit in a certain field, or nil if there is no unit
      
        parameters:
            x_shift - horizontal position of the field relative to colony's
                      position
            y_shift - vertical position of the field relative to colony's
                      position

        remarks:
            Both x_shift and y_shift have to be within the range [-1;1], and at
            least one of them has to be non-zero.
      }
      function GetUnitInField(const x_shift, y_shift: Integer): TUnit;

      { returns the type of good a unit in a certain field is producing
      
        parameters:
            x_shift - horizontal position of the field relative to colony's
                      position
            y_shift - vertical position of the field relative to colony's
                      position

        remarks:
            Both x_shift and y_shift have to be within the range [-1;1], and at
            least one of them has to be non-zero.
            The return value of that function only has a meaningful value, if
            there is a unit in that field, i.e. GetUnitinField() does not
            return nil for that field.
      }
      function GetUnitInFieldGood(const x_shift, y_shift: Integer): TGoodType;

      { sets the type unit that works in a certain field around the colony
      
        parameters:
            x_shift - horizontal position of the field relative to colony's
                      position
            y_shift - vertical position of the field relative to colony's
                      position
            AUnit   - the unit that has to work in that field (nil for no unit)
            AGood   - the type of good the unit is trying to "produce" there

        remarks:
            Both x_shift and y_shift have to be within the range [-1;1], and at
            least one of them has to be non-zero.
      }
      procedure SetUnitInField(const x_shift, y_shift: Integer; const AUnit: TUnit; const AGood: TGoodType=gtFood);

      { returns the unit in a certain building at a certain position (if any)
      
        parameters:
            bt    - the type of building
            place - the place of the unit in the building - a sort of offset
        
        remarks:
            place has to be in the range [0;2].
      }
      function GetUnitInBuilding(const bt: TBuildingType; const place: Byte): TUnit;

      { sets the unit in a certain building at a certain position
      
        parameters:
            bt    - the type of building
            place - the place of the unit in the building - a sort of offset
            AUnit - the new unit that shall be put into the building
        
        remarks:
            place has to be in the range [0;2].
      }
      procedure SetUnitInBuilding(const bt: TBuildingType; const place: Byte; const AUnit: TUnit);

      { utility function to "realign" the units in a certain building, i.e.
        adjust their positions so that there are no empty spaces between them
      
        parameters:
            bt - the type of the building
      }
      procedure RealignUnitsInBuilding(const bt: TBuildingType);

      { returns the first free slot within a certain building, or -1 if there
        is no free slot any more
      
        parameters:
            bt - the type of building
      }
      function GetFirstFreeBuildingSlot(const bt: TBuildingType): ShortInt;

      { returns the number of inhabitants of that colony }
      function GetInhabitants: Word;

      { returns true, if this colony is adjacent to a water square on the map
      
        parameters:
            AMap - the current map
      }
      function AdjacentWater(const AMap: TMap): Boolean;

      { tries to save the colony to a stream and returns true in case of success
      
        parameters:
            fs - the file stream the colony has to be saved to

        remarks:
            The file stream already has to be openend and has to be ready for
            writing.
      }
      function SaveToStream(var fs: TFileStream): Boolean;
    private
      m_Name: string;
      Store: array[TGoodType] of Word; //max. is 300, a word should do it;
      //current level of buildings
      Buildings: array[TBuildingType] of Byte;
      //indicates, which building is going to be constructed next
      // hint: value "btTownHall" indicates no construction in progress, since
      //max. town hall level is 1 and this level is set at colony creation
      CurrentConstruction: TBuildingType;
      UnitsInBuilding: array [btArmory..btBlacksmith] of array [0..2] of TUnit;
      UnitsInFields: array[-1..1] of array [-1..1] of record
                                                        u: TUnit;
                                                        GoesFor: TGoodType;
                                                      end;//rec
  end;//class TColony

  { array that can hold multiple colonies }
  TColonyArr = array of TColony;

  { returns the maximum level a certain building could reach
  
    parameters:
        bt - the type of the building
  }
  function GetMaxBuildingLevel(const bt: TBuildingType): Byte;

  { returns the type of good that is produced in a certain building
  
    parameters:
        bt - the type of the building
  }
  function GetProducedGood(const bt: TBuildingType): TGoodType;

  { returns the amount of hammers and tools that is needed to construct a
    certain level of a certain building
  
    parameters:
        bt      - the type of the building
        level   - the level of the building
        Hammers - the Word that is used to returned the amount of hammers
        Tools   - the Word that is used to returned the amount of tools
  }
  procedure GetBuildingCost(const bt: TBuildingType; const level: Byte; var Hammers, Tools: Word);

implementation

function GetMaxBuildingLevel(const bt: TBuildingType): Byte;
begin
  case bt of
    btNone: Result:= 0;
    btTownHall, btStable, btCustomHouse: Result:= 1;
    btWarehouse, btPress, btCarpenter, btChurch: Result:= 2;
  else Result:= 3;
  end;//case
end;//func

function GetProducedGood(const bt: TBuildingType): TGoodType;
begin
  case bt of
    btArmory: Result:= gtMusket;
    btTownHall: Result:= gtLibertyBell;
    btWeaver: Result:= gtCloth;
    btTobacconist: Result:= gtCigar;
    btDistiller: Result:= gtRum;
    btFurTrader: Result:= gtCoat;
    btCarpenter: Result:= gtHammer;
    btChurch: Result:= gtCross;
    btBlacksmith: Result:= gtTool;
  else Result:= gtFood; //gtFood here means: nothing. Food cannot be produced in buildings.
  end;//case
end;//func

procedure GetBuildingCost(const bt: TBuildingType; const level: Byte; var Hammers, Tools: Word);
begin
  Hammers:= 0;
  Tools:= 0;
  if ((bt<>btNone) and (level>0) and (level<=GetMaxBuildingLevel(bt))) then
  begin
    case bt of
      btFort: case level of
                1: Hammers:= 64;
                2: begin Hammers:= 120; Tools:= 100; end;
                3: begin Hammers:= 320; Tools:= 200; end;
              end;//case
      btDock: case level of
                1: Hammers:= 52;
                2: begin Hammers:= 80; Tools:= 50; end;
                3: begin Hammers:= 240; Tools:= 100; end;
              end;//case
      btWarehouse: case level of
                     1: Hammers:= 80;
                     2: begin Hammers:= 80; Tools:= 20; end;
                   end;//case
      btStable: if level=1 then Hammers:= 64;
      btCustomHouse: if level=1 then
                     begin
                       Hammers:= 160;
                       Tools:= 50;
                     end;
      btPress: case level of
                 1: begin Hammers:= 52; Tools:= 20; end;
                 2: begin Hammers:= 120; Tools:= 50; end;
               end;//case
      btSchool: case level of
                  1: Hammers:= 64;
                  2: begin Hammers:= 160; Tools:= 50; end;
                  3: begin Hammers:= 200; Tools:= 100; end;
                end;//case
      btArmory: case level of
                  1: Hammers:= 52;
                  2: begin Hammers:= 120; Tools:= 50; end;
                  3: begin Hammers:= 240; Tools:= 100; end;
                end;//case
      btWeaver, btTobacconist, btDistiller: case level of
                                              2: begin Hammers:= 64; Tools:= 20; end;
                                              3: begin Hammers:= 160; Tools:= 100; end;
                                            end;//case
      btFurTrader: case level of
                       2: begin Hammers:= 56; Tools:= 20; end;
                       3: begin Hammers:= 160; Tools:= 100; end;
                     end;//case
      btCarpenter: if (level=2) then Hammers:= 52;
      btChurch: case level of
                  1: Hammers:= 64;
                  2: begin Hammers:= 176; Tools:= 100; end;
                end;//case
      btBlacksmith: case level of
                      2: begin Hammers:= 64; Tools:= 20; end;
                      3: begin Hammers:= 240; Tools:= 100; end;
                    end;//case
    end;//case
  end;//if
end;//proc

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
  bt:= Low(TBuildingType);
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

  //value "btNone" indicates no construction in progress
  CurrentConstruction:= btNone;
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

function TColony.RemoveFromStore(const AGood: TGoodType; const amount: Word): Word;
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

procedure TColony.AddToStore(const AGood: TGoodType; const amount: Word);
begin
  //no function, it always succeeds. However, storage amount is cut to maximum
  // storage capacity during next call to TColony.NewRound.
  Store[AGood]:= Store[AGood]+amount;
end;//func

procedure TColony.SetStore(const AGood: TGoodType; const new_amount: Word);
begin
  Store[AGood]:= new_amount;
end;//proc

function TColony.GetBuildingLevel(const bt: TBuildingType): Byte;
begin
  Result:= Buildings[bt];
end;//func

procedure TColony.SetBuildingLevel(const bt: TBuildingType; const new_level: Byte);
begin
  if new_level>GetMaxBuildingLevel(bt) then Buildings[bt]:= GetMaxBuildingLevel(bt)
  else Buildings[bt]:= new_level;
end;//proc

function TColony.GetCurrentConstruction: TBuildingType;
begin
  Result:= CurrentConstruction;
end;//func

procedure TColony.SetCurrentConstruction(const bt: TBuildingType);
begin
  CurrentConstruction:= bt;
end;//proc

procedure TColony.ConstructNextLevel;
begin
  if (Buildings[CurrentConstruction]<GetMaxBuildingLevel(CurrentConstruction)) then
    Buildings[CurrentConstruction]:= Buildings[CurrentConstruction]+1;
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
// and production in fields (i.e. by farmers)
procedure TColony.NewRound(const AMap: TMap);
var i,j, prod: Integer;
    h, t: Word;
begin
  if AMap<>nil then
  begin
    //calculate production of units in surrounding fields
    for i:= -1 to 1 do
      for j:= -1 to 1 do
        if (UnitsInFields[i,j].u<>nil) then
          if ((self.PosX+i>=0) and (self.PosX+i<cMap_X) and (self.PosY+j>=0) and (self.PosY+j<cMap_Y)) then
            Store[UnitsInFields[i,j].GoesFor]:= Store[UnitsInFields[i,j].GoesFor] +
             AMap.tiles[self.PosX+i, self.PosY+j].GetGoodProduction(UnitsInFields[i,j].GoesFor,
                      HasExpertStatus(UnitsInFields[i,j].GoesFor, UnitsInFields[i,j].u.GetType));
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

  //check for buildings
  if (CurrentConstruction<>btNone) then
  begin
    GetBuildingCost(CurrentConstruction, Buildings[CurrentConstruction]+1, h, t);
    if ((Store[gtHammer]>=h) and (Store[gtTool]>=t) and (h+t>0)) then
    begin
      //enough material for new building
      ConstructNextLevel;
      CurrentConstruction:= btNone;
      RemoveFromStore(gtHammer, h);
      RemoveFromStore(gtTool, t);
    end;//if
  end;//if

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
      UnitsInFields[x_shift, y_shift].u.SetState(usNormal);
    end;//if
    //place new unit
    UnitsInFields[x_shift, y_shift].u:= AUnit;
    UnitsInFields[x_shift, y_shift].GoesFor:= AGood;
    if AUnit<>nil then
    begin
      UnitsInFields[x_shift, y_shift].u.SetLocation(ulInColony);
      UnitsInFields[x_shift, y_shift].u.SetState(usNormal);
    end;//if
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
      UnitsInBuilding[bt, place].SetState(usNormal);
    end;//if
    //place new unit
    UnitsInBuilding[bt, place]:= AUnit;
    if AUnit<>nil then
    begin
      UnitsInBuilding[bt, place].SetLocation(ulInColony);
      UnitsInBuilding[bt, place].SetState(usNormal);
    end;//if
  end;//else
end;//proc

procedure TColony.RealignUnitsInBuilding(const bt: TBuildingType);
var i: Byte;
begin
  if (bt in [btArmory..btBlacksmith]) then
  begin
    for i:= 0 to 1 do
      if (UnitsInBuilding[bt,i]=nil) then
      begin
        UnitsInBuilding[bt,i]:= UnitsInBuilding[bt,i+1];
        UnitsInBuilding[bt,i+1]:= nil;
      end;//if
  end;//if
end;//proc

function TColony.GetFirstFreeBuildingSlot(const bt: TBuildingType): ShortInt;
var i: ShortInt;
begin
  Result:= -1;
  if (bt in [btArmory..btBlacksmith]) then
  begin
    i:= 0;
    while i<=2 do
    begin
      if (UnitsInBuilding[bt, i]=nil) then
      begin
        Result:= i;
        Exit;
      end;//if
      i:= i+1;
    end;//while
  end;//if
end;//func

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

function TColony.AdjacentWater(const AMap: TMap): Boolean;
var i, j: Integer;
begin
  Result:= False;
  if AMap=nil then Exit;
  for i:= PosX-1 to PosX+1 do
    for j:= PosY-1 to PosY+1 do
      if (i in [0..cMap_X-1]) and (j in [0..cMap_Y-1]) then
        if AMap.tiles[i,j].IsWater then
        begin
          Result:= True;
          Exit;
        end;//if
end;//func

function TColony.SaveToStream(var fs: TFileStream): Boolean;
var i,j: LongInt;
    field_count, temp_b: Byte;
    bt: TBuildingType;
begin
  if fs=nil then
  begin
    Result:= False;
    Exit;
  end;//if
  Result:= (fs.Write(m_Nation, sizeof(LongInt))=sizeof(LongInt));
  Result:= Result and (fs.Write(PosX, sizeof(LongInt))=sizeof(LongInt));
  Result:= Result and (fs.Write(PosY, sizeof(LongInt))=sizeof(LongInt));
  //name
  i:= length(m_Name);
  Result:= Result and (fs.Write(i, sizeof(LongInt))=sizeof(LongInt));
  Result:= Result and (fs.Write(m_Name[1], length(m_Name))=length(m_Name));
  //store
  for i:= Ord(Low(TGoodType)) to Ord(High(TGoodType)) do
    Result:= Result and (fs.Write(Store[TGoodType(i)], sizeof(Word))=sizeof(Word));
  //buildings
  for i:= Ord(Low(TBuildingType)) to Ord(High(TBuildingType)) do
    Result:= Result and (fs.Write(Buildings[TBuildingType(i)], sizeof(Byte))=sizeof(Byte));
  //current building under construction
  Result:= Result and (fs.Write(CurrentConstruction, sizeof(TBuildingType))=sizeof(TBuildingType));
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
        Result:= Result and (fs.Write(i, sizeof(LongInt))=sizeof(LongInt));
        Result:= Result and (fs.Write(j, sizeof(LongInt))=sizeof(LongInt));
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