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

unit Data;

interface

uses
  Nation, EuropeanNation, IndianNation, Language, Units, Colony, Tribe, Map,
  Goods, FoundingFathers, Classes, SysUtils, Helper;

const
{$IFDEF Win32}
  path_delimiter = '\';
{$ELSE}
  path_delimiter = '/';
{$ENDIF}
  { string constants that hold various (relative) paths used by vespucci }
  data_path = 'data' + path_delimiter;
  //path of America map file
  america_map_path = data_path +'america'+path_delimiter+'america.vmd';
  img_path = data_path+'img'+path_delimiter;
  //path of images for good icons
  good_img_path = img_path+'goods'+path_delimiter;
  //path of terrain images
  terrain_img_path = img_path+'terrain'+path_delimiter;
  //path of unit icons
  unit_img_path = img_path+'units'+path_delimiter;
  //path of state icons for units
  state_img_path = unit_img_path+'state'+path_delimiter;
  //directory that holds the images of colonies
  colony_img_path = img_path+'colony'+path_delimiter;
  //directory that holds the images of buildings
  building_img_path = colony_img_path+'building'+path_delimiter;
  //directory that holds the images of Indian settlements
  tribe_img_path = img_path+'tribe'+path_delimiter;
  //directoy that holds the saved games
  save_path = data_path+'saves'+path_delimiter;

  { header signatures for data files }
  cDataFileHeader = 'VDD';
  cColonyFileHeader = 'VCD';
  cUnitFileHeader = 'VUD';
  cNationFileHeader = 'VND';

  { initial spawnpoints for European nations }
  cSpawnpointsAmerica: array [0..3] of record
                         x, y: Byte;
                       end
                       =(
                         (x: 36; y: 13),
                         (x: 46; y: 27),
                         (x: 55; y: 41),
                         (x: 50; y: 55)
                       );

type
  { record type which holds different parts of (player's) score }
  TScoreRecord = record
                   Citizens: LongInt;
                   Congress: LongInt;
                   Gold:     LongInt;
                   Villages: LongInt;
                   Total:    LongInt;
                 end;//rec

  { record which holds information about a nation (foreign affairs report) }
  TForeignRecord = record
                     Colonies:   LongInt;
                     Average:    Byte;
                     Population: LongInt;
                     Military:   LongInt;
                     Naval:      LongInt;
                     Merchant:   LongInt;
                     Rebels:     LongInt;
                     Loyals:     LongInt;
                     Diplomatic: array [cMinEuropean..cMaxEuropean] of TDiplomaticStatus;
                   end;//rec
  { array that holds information about unit types/ jobs }
  TWorkArray = array[utCriminal..utDragoon] of Word;

  { ********
    **** TData class
    ****
    **** purpose: holds all the data of a game, i.e. nations, units, settlements;
    ****          this class is also responsible for loading/saving games.
    *******
  }
  TData = class
            private
              { the current year in the game }
              Year: LongInt;
              { season flag: true, if it's autumn; false, if it's spring }
              Autumn: Boolean;
              { integer constant that defines the player's nation }
              player_nation: LongInt;
              //array that holds all nations
              Nations: array [cMinNations..cMaxIndian] of TNation;
              //the units
              m_Units: array of TUnit;
              Unit_max: Integer;
              //the colonies
              m_Colonies: array of TColony;
              Colony_max: Integer;
              //the tribes
              m_Tribes: array of TTribe;
              Tribe_max: Integer;
              //map
              m_Map: TMap;
              //language
              lang: TLanguage;
              //relative path
              base_dir: string;
              //loading routines (maybe save routines should be here, too?)
              { loads a unit from the stream and returns true on success

                parameters:
                    AUnit - the unit that will hold the loaded data
                    fs    - the file stream the unit will be loaded from
              }
              function LoadUnitFromStream(var AUnit: TUnit; var fs: TFileStream): Boolean;

              { loads a colony from the stream and returns true on success

                parameters:
                    AColony - the colony that will hold the loaded data
                    fs      - the file stream the colony will be loaded from
              }
              function LoadColonyFromStream(var AColony: TColony; var fs: TFileStream): Boolean;

              { sets the initial values for all nations }
              procedure InitializeNations;

              { initializes the map }
              procedure InitializeMap;

              { sets all tribes that are initially at the America map }
              procedure InitTribes_America;

              { deletes all colonies }
              procedure DeInitColonies;

              { deletes all tribes }
              procedure DeInitTribes;

              { deletes all units }
              procedure DeInitUnits;
            public
              { constructor

                parameters:
                    NumNation_Player - integer constant defining the player's
                                       nation
              }
              constructor Create(const NumNation_Player: LongInt=cNationEngland);

              { destructor }
              destructor Destroy; override;

              { returns the current game year }
              function GetYear: LongInt;

              { returns true, if it's currently autumn in the game }
              function IsAutumn: Boolean;

              { returns the player's nation }
              function PlayerNation: LongInt;

              { returns a certain nation

                parameters:
                    count - integer constant defining the desired nation
              }
              function GetNation(const count: Integer): TNation;

              { advances to next year and/or advances the season }
              procedure AdvanceYear;
              // ---- unit-related functions ----
              { creates a new unit and returns the created unit

                parameters:
                    TypeOfUnit - unit's type
                    ANation    - the nation the unit will belong to
                    X, Y       - initial position of the unit
              }
              function NewUnit(const TypeOfUnit: TUnitType; const ANation: Integer; X: Integer=1; Y: Integer=1): TUnit;

              { returns the first unit in a certain field/map square. If no
                unit was found, nil will be returned.

                parameters:
                    x,y         - position of the unit
                    OnlyAmerica - true, if only units in America should be
                                  considered (default behaviour)
              }
              function GetFirstUnitInXY(const x, y: Integer; const OnlyAmerica: Boolean=True): TUnit;

              { returns the first "lazy" unit of a nation, i.e. a unit that
                still has some moves left. If no unit was found, nil will be
                returned. Fortified units are not considered as "lazy", even if
                they have some moves left.

                parameters:
                    num_Nation - integer constant identifying the nation
              }
              function GetFirstLazyUnit(const num_Nation: Integer): TUnit;

              { returns all ships of a certain nation. If no ships were found,
                an array of length zero will be returned.

                parameters:
                    numNation - integer constant identifying the nation
              }
              function GetAllShips(const numNation: LongInt): TUnitArr;

              { returns all ships in a certain field/map square. If no ships
                were found, an array of length zero will be returned.

                parameters:
                    x,y         - position of the unit
                    OnlyAmerica - true, if only ships in America should be
                                  considered (default behaviour)
              }
              function GetAllShipsInXY(const x,y: Integer; const OnlyAmerica: Boolean=True): TUnitArr;

              { returns all ships a certain nation has in Europe. If no ships
                were found, an array of length zero will be returned.

                parameters:
                    num_nation - integer constant identifying the nation
              }
              function GetAllShipsInEurope(const num_nation: Integer): TUnitArr;

              { returns all non-ship units a certain nation has in Europe. If no
                units were found, an array of length zero will be returned.

                parameters:
                    num_nation - integer constant identifying the nation
              }
              function GetAllNonShipsInEurope(const num_nation: Integer): TUnitArr;

              { returns all ship of a certain nation that are sailing to Europe.
                If no ships were found, an array of length zero will be
                returned.

                parameters:
                    num_nation - integer constant identifying the nation
              }
              function GetAllShipsGoingToEurope(const num_nation: Integer): TUnitArr;

              { returns all ship of a certain nation that are sailing to the New
                World. If no ships were found, an array of length zero will be
                returned.

                parameters:
                    num_nation - integer constant identifying the nation
              }
              function GetAllShipsGoingToNewWorld(const num_nation: Integer): TUnitArr;

              { This procedure wraps all of the functionality of
                GetAllShipsInEurope(), GetAllNonShipsInEurope(),
                GetAllShipsGoingToEurope() and GetAllShipsGoingToNewWorld() in
                one procedure. It is faster than calling each of these functions
                separately, because it only has to go through the units once and
                not four times.

                parameters:
                    num_nation   - integer constant identifying the nation
                    Ship         - array that will hold all ships in Europe
                    People       - array that will hold all non-ship units in
                                   Europe
                    ExpectedSoon - array that will hold all ships sailing to
                                   Europe
                    ToNewWorld   - array that will hold all ships sailing to
                                   the new world
              }
              procedure GetEuropeanQuartett(const num_nation: Integer; var Ships, People, ExpectedSoon, ToNewWorld: TUnitArr);

              { returns all units of a nation, except ships and caravans

                parameters:
                    num_nation - integer constant identifying the nation
              }
              function GetAllNonCargoUnits(const num_nation: Integer): TUnitArr;

              { returns the score of the given nation

                parameters:
                    num_nation - integer constant identifying the nation
                    u_arr      - array that holds all units which should be
                                 considered for calculation of the score.
                                 Usually, this is the Result of the function
                                 GetAllNonCargoUnits().
              }
              function GetScore(const num_nation: Integer; const u_arr: TUnitArr): TScoreRecord;

              { returns the numbers of a nation which are shown in the foreign affairs report

                parameters:
                    num_nation - integer constant identifying the nation
              }
              function GetForeignReport(const num_nation: Integer): TForeignRecord;

              { returns the numbers of a nation which are shown in the job report

                parameters:
                    num_nation - integer constant identifying the nation
              }
              function GetWorkArray(const num_nation: Integer): TWorkArray;

              //units in colonies
              { returns all units that are in the field/map square of the given
                colony, except ships and caravans. If no units were found, an
                array of length zero will be returned.

                parameters:
                    ACol - the colony
              }
              function GetAllUnitsInColony(const ACol: TColony): TUnitArr;
              // ---- functions for colonies ----
              { creates a new colony and returns the created colony

                parameters:
                    x,y        - position of the colony
                    num_Nation - integer constant identifying the nation that
                                 founded the colony
                    AName      - the name of the colony
              }
              function NewColony(const x,y: Byte; const num_Nation: Integer; const AName: ShortString): TColony;

              { returns the colony in the field with coordinates (x;y), if any.
                If no colony is found, nil is returned.

                parameters:
                    x,y - coordinates of the map square
              }
              function GetColonyInXY(const x,y: Byte): TColony;

              { returns an array containing all the nations of a certain nation

                parameters:
                    num_nation - integer constant identifying the nation
              }
              function GetColonyList(const num_nation: Integer): TColonyArr;

              { tries to delete the colony at the given position and returns
                true, if a colony was found and deleted

                parameters:
                    x,y - coordinates of the colony's position
              }
              function DeleteColony(const x,y: Byte): Boolean;
              // ---- tribe-related functions ----
              { creates a new tribe and returns the new created tribe

                parameters:
                    x,y        - position of the tribe
                    num_Nation - integer identifying the indian nation the tribe
                                 belongs to
                    Teaches    - special skill the tribe can teach to Europeans
              }
              function NewTribe(const x,y: Byte; const num_Nation: Integer; const Teaches: TUnitType): TTribe;

              { returns the tribe at map location (x;y), if any. If no tribe is
                found, nil is returned.

                parameters:
                    x,y - coordinates of the map square
              }
              function GetTribeInXY(const x,y: Byte): TTribe;
              //general (settlements)
              { returns true, if the given coordinates and adjacent squares do
                not hold a settlement yet, and thus this coordinates can be used
                to build a settlement

                parameters:
                    x,y - coordinates of the map square
              }
              function FreeForSettlement(const x,y:Byte): Boolean;

              //others
              { starts a new round for the given nation

                parameters:
                    num_nation - integer identifying the nation
              }
              procedure NewRound(const num_Nation: Integer);

              { sets some AI tasks for computer-controlled nations

                parameters:
                    num_nation - integer identifying the nation
              }
              procedure UpdateAITasks(const num_Nation: LongInt);

              { sets some AI tasks for European computer-controlled nations

                parameters:
                    num_nation - integer identifying the nation
              }
              procedure UpdateAITasksEuropean(const num_Nation: LongInt);

              { sets some AI tasks for Indian computer-controlled nations

                parameters:
                    num_nation - integer identifying the nation
              }
              procedure UpdateAITasksIndian(const num_Nation: LongInt);

              { calls NewRound for all AI-controlled nations whose nation number
                is less than player's numer, i.e. all nations that move before
                the player
              }
              procedure ProcessNationsBeforePlayer;

              { calls NewRound for all AI-controlled nations whose nation number
                is greater than player's numer, i.e. all nations that move after
                the player, and advances the year
              }
              procedure ProcessNationsAfterPlayer;

              { tries to save the game to the n-th slot. Returns true in case of
                success.

                parameters:
                    n   - save game slot index (usually in [1;10])
                    err - string that will contain an error message if the
                          function failed
              }
              function SaveData(const n: Word; var err: string): Boolean;

              { tries to load the game from the n-th slot. Returns true in case
                of success.

                parameters:
                    n   - save game slot index (usually in [1;10])
                    err - string that will contain an error message if the
                          function failed
              }
              function LoadData(const n: Word; var err: string): Boolean;

              { returns a short description for the save game in the n-th slot.

                parameters:
                    n   - save game slot index (usually in [1;10])
              }
              function GetSaveInfo(const n: Word): string;

              { same as above, but for the first ten slots }
              function GetSaveSlots: TShortStrArr;

              { returns the basic path of the application

                remarks:
                    This is used to turn relative paths into absolute paths.
              }
              function GetPathBase: string;

              { returns the TLanguage class, that holds all language-related
                strings
              }
              function GetLang: TLanguage;

              { returns the current map }
              function GetMap: TMap;

              { returns the list of goods/their amount a unit would produce in
                a certain field of a colony

                parameters:
                    x_shift  - horizontal positional offset (has to be in [-1:1])
                    y_shift  - vertical positional offset (has to be in [-1:1])
                    Unittype - the type of the unit
                    ACol     - the colony where the unit works/ will work
              }
              function GetJobList(const x_shift, y_shift: ShortInt; const UnitType: TUnitType; ACol: TColony): TShortStrArr;

              { creates the initial units for an European nation, i.e. the ship and two passengers

                parameters:
                    num_nation - integer constant identifying the nation
                    x,y        - coordinates where to spawn the units
              }
              procedure SpawnEuropeanNation(const num_nation: LongInt; const x, y: Byte);
          end;//class TData

implementation

constructor TData.Create(const NumNation_Player: LongInt=cNationEngland);
var i: Integer;
begin
  base_dir:= '';
  if NumNation_Player in [cMinEuropean..cMaxEuropean] then
    player_nation:= NumNation_Player
  else player_nation:= cNationEngland;
  Year:= 1492;
  Autumn:= False;
  lang:= TLanguage.Create;
  //nations
  for i:= cMinNations to cMaxIndian do
    Nations[i]:= nil;
  InitializeNations;
  //units
  SetLength(m_Units, 0);
  Unit_max:= -1;
  //colonies
  SetLength(m_Colonies, 0);
  Colony_max:= -1;
  //tribes
  SetLength(m_Tribes, 0);
  Tribe_max:= -1;
  InitializeMap;
end;//construc

destructor TData.Destroy;
var i: Integer;
begin
  for i:= cMinNations to cMaxIndian do
    if Nations[i]<>nil then Nations[i].Destroy;
  DeInitColonies;
  DeInitTribes;
  DeInitUnits;
  lang.Destroy;
  m_Map.Destroy;
end;//destruc

procedure TData.InitializeNations;
var i: Integer;
begin
  if (Nations[cNationEngland]<>nil) then Nations[cNationEngland].Destroy;
  Nations[cNationEngland]:= TEuropeanNation.Create(cNationEngland, lang.GetNationName(cNationEngland), 'Walter Raleigh');
  if (Nations[cNationFrance]<>nil) then Nations[cNationFrance].Destroy;
  Nations[cNationFrance]:= TEuropeanNation.Create(cNationFrance, lang.GetNationName(cNationFrance), 'Jacques Cartier');
  if (Nations[cNationSpain]<>nil) then Nations[cNationSpain].Destroy;
  Nations[cNationSpain]:= TEuropeanNation.Create(cNationSpain, lang.GetNationName(cNationSpain), 'Christoph Columbus');
  if (Nations[cNationHolland]<>nil) then Nations[cNationHolland].Destroy;
  Nations[cNationHolland]:= TEuropeanNation.Create(cNationHolland, lang.GetNationName(cNationHolland), 'Michiel De Ruyter');
  for i:= cMinIndian to cMaxIndian do
  begin
    if (Nations[i]<>nil) then Nations[i].Destroy;
    Nations[i]:= TIndianNation.Create(i, lang.GetNationName(i));
  end;//for
end;//proc

procedure TData.InitTribes_America;
var i: Integer;
begin
  for i:= 0 to High(cTribeLocationsAmerica) do
  begin
    NewTribe(cTribeLocationsAmerica[i].x, cTribeLocationsAmerica[i].y, cTribeLocationsAmerica[i].Nation, utSugarPlanter);
  end;//for
end;//proc

procedure TData.InitializeMap;
begin
  m_Map:= TMap.Create;
  if FileExists(GetPathBase+america_map_path) then
  begin
    if m_Map.LoadFromFile(GetPathBase+america_map_path) then
    begin
      WriteLn('Map "'+GetPathBase+america_map_path+'" successfully loaded.');
      InitTribes_America;
    end//if
    else begin
      WriteLn('Couldn''t load map file "'+GetPathBase+america_map_path+'" properly. Using generation routine instead.');
      m_Map.Generate(0.7, @Map.h4);
    end;
  end
  else begin
    WriteLn('Couldn''t find map file "'+GetPathBase+america_map_path+'". Using generation routine instead.');
    m_Map.Generate(0.7, @Map.h4);
  end;
  m_Map.GenerateSpecials;
end;//proc

procedure TData.DeInitColonies;
var i: Integer;
begin
  for i:= Colony_max downto 0 do
    if m_Colonies[i]<>nil then m_Colonies[i].Destroy;
  SetLength(m_Colonies, 0);
  Colony_max:= -1;
end;//proc

procedure TData.DeInitTribes;
var i: Integer;
begin
  for i:= Tribe_max downto 0 do
    if m_Tribes[i]<>nil then m_Tribes[i].Destroy;
  SetLength(m_Tribes, 0);
  Tribe_max:= -1;
end;//proc

procedure TData.DeInitUnits;
var i: Integer;
begin
  for i:=Unit_max downto 0 do
    if m_Units[i]<>nil then m_Units[i].Destroy;
  SetLength(m_Units, 0);
  Unit_max:= -1;
end;//proc

function TData.GetYear: LongInt;
begin
  Result:= Year;
end;//func

function TData.IsAutumn: Boolean;
begin
  Result:= Autumn;
end;//func

procedure TData.AdvanceYear;
begin
  if Year<1600 then Year:= Year+1
  else begin
    if Autumn then
    begin
      //if we have autumn, start next year and set season to spring
      Year:= Year+1;
      Autumn:= False;
    end
    else Autumn:= True;
  end;//else
end;//proc

function TData.PlayerNation: LongInt;
begin
  Result:= player_nation;
end;//func

function TData.GetNation(const count: Integer): TNation;

begin
  if ((count>cMaxIndian) or (count<cMinNations)) then
    Result:= nil
  else
    Result:= Nations[count];
end;//func

function TData.NewUnit(const TypeOfUnit: TUnitType; const ANation: Integer; X: Integer=1; Y: Integer=1): TUnit;
var i: Integer;
begin
  if (Unit_max+1>High(m_Units)) then
  begin
    SetLength(m_Units, High(m_Units)+5);
    //"initialize" new units
    for i:=Unit_max+1 to High(m_Units) do
      m_Units[i]:= nil;
  end;//if
  m_Units[Unit_max+1]:= TUnit.Create(TypeOfUnit, ANation, X, Y);
  Unit_max:= Unit_max+1;
  Result:= m_Units[Unit_max];
end;//proc

function TData.GetFirstUnitInXY(const x, y: Integer; const OnlyAmerica: Boolean=True): TUnit;
var i: Integer;
begin
  Result:= nil;
  for i:= 0 to Unit_max do
    if ((m_Units[i].GetPosX=x) and (m_Units[i].GetPosY=y)) then
    begin
      if ((m_Units[i].GetLocation=ulAmerica) or not OnlyAmerica) then
      begin
        Result:= m_Units[i];
        break;
      end;//if
    end;//if
end;//func

function TData.GetFirstLazyUnit(const num_Nation: Integer): TUnit;
var i: Integer;
begin
  Result:= nil;
  for i:= 0 to Unit_max do
    if m_Units[i]<>nil then
    begin
      if ((m_Units[i].MovesLeft>0) and (m_Units[i].GetNation=num_Nation) and
          (m_Units[i].GetLocation=ulAmerica) and (m_Units[i].GetState<>usFortified)) then
      begin
        Result:= m_Units[i];
        break;
      end;//if
    end;//if
end;//func

function TData.GetAllShips(const numNation: LongInt): TUnitArr;
var i: Integer;
begin
  SetLength(Result, 0);
  for i:= 0 to Unit_max do
    if m_Units[i]<>nil then
      if (m_Units[i].GetNation=numNation) and (m_Units[i].IsShip) then
      begin
        SetLength(Result, length(Result)+1);
        Result[High(Result)]:= m_Units[i];
      end;//if
end;//func

function TData.GetAllShipsInXY(const x,y: Integer; const OnlyAmerica: Boolean=True): TUnitArr;
var i: Integer;
begin
  SetLength(Result, 0);
  for i:= 0 to Unit_max do
    if m_Units[i]<>nil then
    begin
      if ((m_Units[i].GetPosX=x) and (m_Units[i].GetPosY=y) and (m_Units[i].FreightCapacity>0)) then
      begin
        if ((m_Units[i].GetLocation=ulAmerica) or not OnlyAmerica) then
        begin
          SetLength(Result, length(Result)+1);
          Result[High(Result)]:= m_Units[i];
        end;//if
      end;//if
    end;//if <>nil
end;//func

function TData.GetAllShipsInEurope(const num_nation: Integer): TUnitArr;
var i: Integer;
begin
  SetLength(Result, 0);
  for i:= 0 to Unit_max do
    if m_Units[i]<>nil then
    begin
      if ((m_Units[i].GetLocation=ulEurope) and (m_Units[i].IsShip) and(m_Units[i].GetNation=num_nation)) then
      begin
        SetLength(Result, length(Result)+1);
        Result[High(Result)]:= m_Units[i];
      end;//if
    end;//if <>nil
end;//func

function TData.GetAllNonShipsInEurope(const num_nation: Integer): TUnitArr;
var i: Integer;
begin
  SetLength(Result, 0);
  for i:= 0 to Unit_max do
    if m_Units[i]<>nil then
    begin
      if ((m_Units[i].GetLocation=ulEurope) and(m_Units[i].GetNation=num_nation) and not m_Units[i].IsShip) then
      begin
        SetLength(Result, length(Result)+1);
        Result[High(Result)]:= m_Units[i];
      end;//if
    end;//if <>nil
end;//func

function TData.GetAllShipsGoingToEurope(const num_nation: Integer): TUnitArr;
var i: Integer;
begin
  SetLength(Result, 0);
  for i:= 0 to Unit_max do
    if m_Units[i]<>nil then
    begin
      if ((m_Units[i].GetLocation=ulGoToEurope) and(m_Units[i].GetNation=num_nation)) then
      begin
        SetLength(Result, length(Result)+1);
        Result[High(Result)]:= m_Units[i];
      end;//if
    end;//if <>nil
end;//func

function TData.GetAllShipsGoingToNewWorld(const num_nation: Integer): TUnitArr;
var i: Integer;
begin
  SetLength(Result, 0);
  for i:= 0 to Unit_max do
    if m_Units[i]<>nil then
    begin
      if ((m_Units[i].GetLocation=ulGoToNewWorld) and(m_Units[i].GetNation=num_nation)) then
      begin
        SetLength(Result, length(Result)+1);
        Result[High(Result)]:= m_Units[i];
      end;//if
    end;//if <>nil
end;//func

procedure TData.GetEuropeanQuartett(const num_nation: Integer; var Ships, People, ExpectedSoon, ToNewWorld: TUnitArr);
var i: Integer;
begin
  SetLength(Ships, 0);
  SetLength(People, 0);
  SetLength(ExpectedSoon, 0);
  SetLength(ToNewWorld, 0);
  for i:= 0 to Unit_max do
    if m_Units[i]<>nil then
    begin
      if (m_Units[i].GetNation=num_nation) then
      begin
        case m_Units[i].GetLocation of
          ulEurope: begin
                      if m_Units[i].FreightCapacity>0 then
                      begin
                        SetLength(Ships, length(Ships)+1);
                        Ships[High(Ships)]:= m_Units[i];
                      end//if
                      else begin
                        SetLength(People, length(People)+1);
                        People[High(People)]:= m_Units[i];
                      end;//else
                    end;//ulEurope
          ulGoToEurope: begin
                          SetLength(ExpectedSoon, length(ExpectedSoon)+1);
                          ExpectedSoon[High(ExpectedSoon)]:= m_Units[i];
                        end;//ulGoToEurope
          ulGoToNewWorld: begin
                            SetLength(ToNewWorld, length(ToNewWorld)+1);
                            ToNewWorld[High(ToNewWorld)]:= m_Units[i];
                          end;//ulGoToNewWorld
        end;//case
      end;//if
    end;//if
end;//proc

function TData.GetAllNonCargoUnits(const num_nation: Integer): TUnitArr;
var i: Integer;
begin
  SetLength(Result, 0);
  for i:= 0 to Unit_max do
    if m_Units[i]<>nil then
    begin
      if ((m_Units[i].GetNation=num_nation) and (not m_Units[i].IsShip) and(m_Units[i].GetType<>utConvoy)) then
      begin
        SetLength(Result, length(Result)+1);
        Result[High(Result)]:= m_Units[i];
      end;//if
    end;//if <>nil
end;//func

function TData.GetScore(const num_nation: Integer; const u_arr: TUnitArr): TScoreRecord;
var ANat: TNation;
    i: Integer;
begin
  Result.Citizens:= 0;
  for i:= 0 to High(u_arr) do
  begin
    case u_arr[i].GetType of
      utCriminal, utServant: Result.Citizens:= Result.Citizens+1;
      utColonist:            Result.Citizens:= Result.Citizens+2;
    else
      Result.Citizens:= Result.Citizens+4;
    end;//case
  end;//for
  Result.Congress:= 0; //not implemented yet
  Result.Gold:= 0;
  Result.Villages:= 0;
  ANat:= GetNation(num_nation);
  if (ANat.IsEuropean) then
  begin
    //founding fathers: +5 points per founding father
    for i:= Ord(Low(TFoundingFathers)) to Ord(High(TFoundingFathers)) do
      if (ANat as TEuropeanNation).HasFoundingFather(TFoundingFathers(i)) then
        Result.Congress:= Result.Congress+5;
    //gold: +1 point per 1000 gold
    Result.Gold:= (ANat as TEuropeanNation).GetGold div 1000;
    //villages: -1 point per burned village
    Result.Villages:= -(ANat as TEuropeanNation).GetVillagesBurned;
  end;
  Result.Total:= Result.Citizens+Result.Congress+Result.Gold+Result.Villages;
end;//func

function TData.GetForeignReport(const num_nation: Integer): TForeignRecord;
var i, sum: Integer;
begin
  Result.Colonies:= 0;
  Result.Average:= 0;
  Result.Population:= 0;
  Result.Military:= 0;
  Result.Naval:= 0;
  Result.Merchant:= 0;
  Result.Rebels:= 0;
  Result.Loyals:= 0;
  for i:= cMinEuropean to cMaxEuropean do
  begin
    Result.Diplomatic[i]:= dsUndefined;
  end;//for
  //abort here, if nation is no European nation
  if not (num_nation in [cMinEuropean..cMaxEuropean]) then Exit;
  // ---- colonies
  sum:= 0;
  for i:= 0 to Colony_max do
  begin
    if (m_Colonies[i].GetNation=num_nation) then
    begin
      Result.Colonies:= Result.Colonies+1;
      sum:= sum + m_Colonies[i].GetInhabitants;
    end;//if
  end;//for
  if Result.Colonies>0 then Result.Average:= sum div Result.Colonies
  else Result.Average:= 0;
  // ---- units
  for i:= 0 to Unit_max do
  begin
    if (m_Units[i].GetNation=num_nation) then
    begin
      case m_Units[i].GetType of
        //merchant ships
        utCaravel, utTradingShip, utGalleon:
             Result.Merchant:= Result.Merchant + m_Units[i].FreightCapacity;
        //naval ships
        utPrivateer, utFrigate, utMan_o_War:
            Result.Naval:= Result.Naval + m_Units[i].AttackStrength;
        //military
        utRegular, utDragoon, utScout, utArtillery:
            Result.Military:= Result.Military + m_Units[i].AttackStrength;
        //convoy
        utConvoy: //do nothing
      //any other unit
      else Result.Population:= Result.Population +1;
      end;//case
    end;//if Unit belongs to that nation
  end;//for
  //We haven't implemented Rebels/Loyalists yet, so assume the worst.
  Result.Loyals:= Result.Population;
  // ---- diplomatic status
  for i:= cMinEuropean to cMaxEuropean do
  begin
    Result.Diplomatic[i]:= (Nations[num_nation] as TEuropeanNation).GetDiplomatic(i);
  end;//for
end;//func

function TData.GetWorkArray(const num_nation: Integer): TWorkArray;
var ut: TUnitType;
    i: Integer;
begin
  ut:= utCriminal;
  while ut<=utDragoon do
  begin
    Result[ut]:= 0;
    ut:= Succ(ut);
  end;//while
  //units
  for i:= 0 to Unit_max do
    if m_Units[i]<>nil then
      if ((m_Units[i].GetNation=num_nation) and (m_Units[i].GetType in [utCriminal..utDragoon])) then
        Result[m_Units[i].GetType]:= Result[m_Units[i].GetType]+1;
end;//func

function TData.GetAllUnitsInColony(const ACol: TColony): TUnitArr;
var i: Integer;
begin
  SetLength(Result, 0);
  if ACol<>nil then
  begin
    for i:= 0 to Unit_max do
      if m_Units[i]<>nil then
        if ((m_Units[i].GetLocation=ulAmerica) and (m_Units[i].GetPosX=ACol.GetPosX) and (m_Units[i].GetPosY=ACol.GetPosY) and (m_Units[i].FreightCapacity=0)) then
        begin
          SetLength(Result, Length(Result)+1);
          Result[High(Result)]:= m_Units[i];
        end;//if
  end;//if
end;//func

function TData.NewColony(const x,y: Byte; const num_Nation: Integer; const AName: ShortString): TColony;
var i: Integer;
begin
  if (Colony_max+1>High(m_Colonies)) then
  begin
    SetLength(m_Colonies, High(m_Colonies)+5);
    //"initialize" new colonies
    for i:=Colony_max+1 to High(m_Colonies) do
      m_Colonies[i]:= nil;
  end;//if
  m_Colonies[Colony_max+1]:= TColony.Create(x, y, num_nation, AName);
  Colony_max:= Colony_max+1;
  Result:= m_Colonies[Colony_max];
end;//func

function TData.GetColonyInXY(const x,y: Byte): TColony;
var i: Integer;
begin
  Result:= nil;
  for i:= 0 to Colony_max do
    if m_Colonies[i]<>nil then
      if ((m_Colonies[i].GetPosX=x) and (m_Colonies[i].GetPosY=y)) then
      begin
        Result:= m_Colonies[i];
        break;
      end;//if
end;//func

function TData.GetColonyList(const num_nation: Integer): TColonyArr;
var i: Integer;
begin
  SetLength(Result, 0);
  for i:= 0 to Colony_max do
    if m_Colonies[i]<>nil then
    begin
      if (m_Colonies[i].GetNation=num_nation) then
      begin
          SetLength(Result, length(Result)+1);
          Result[High(Result)]:= m_Colonies[i];
      end;//if
    end;//if <>nil
end;//func

function TData.DeleteColony(const x,y: Byte): Boolean;
var i: Integer;
begin
  Result:= False;
  for i:= 0 to Colony_max do
    if m_Colonies[i]<>nil then
      if ((m_Colonies[i].GetPosX=x) and (m_Colonies[i].GetPosY=y)) then
      begin
        m_Colonies[i].Destroy;
        m_Colonies[i]:= nil;
        m_Colonies[i]:= m_Colonies[Colony_max];
        m_Colonies[Colony_max]:= nil;
        Colony_max:= Colony_max-1;
        Result:= True;
        break;
      end;//if
end;//func

function TData.NewTribe(const x,y: Byte; const num_Nation: Integer; const Teaches: TUnitType): TTribe;
var i: Integer;
begin
  if (Tribe_max+1>High(m_Tribes)) then
  begin
    SetLength(m_Tribes, High(m_Tribes)+5);
    //"initialize" new tribes
    for i:=Tribe_max+1 to High(m_Tribes) do
      m_Tribes[i]:= nil;
  end;//if
  m_Tribes[Tribe_max+1]:= TTribe.Create(x, y, num_nation, Teaches);
  Tribe_max:= Tribe_max+1;
  Result:= m_Tribes[Tribe_max];
end;//func

function TData.GetTribeInXY(const x,y: Byte): TTribe;
var i: Integer;
begin
  Result:= nil;
  for i:= 0 to Tribe_max do
    if m_Tribes[i]<>nil then
      if ((m_Tribes[i].GetPosX=x) and (m_Tribes[i].GetPosY=y)) then
      begin
        Result:= m_Tribes[i];
        break;
      end;//if
end;//func

function TData.FreeForSettlement(const x,y:Byte): Boolean;
var i,j: Integer;
begin
  Result:= True;
  for i:= x-2 to x+2 do
    for j:= y-2 to y+2 do
      if ((i>=0) and (j>=0)) then
        if (GetColonyInXY(i,j)<>nil) then
        begin
          Result:= False;
          Exit; //maybe simple "break;" won't do - we are in a "double loop"
        end;//if
  if (GetTribeInXY(x,y)<>nil) then Result:= False;
end;//func

procedure TData.NewRound(const num_Nation: Integer);
var i: Integer;
    ENat: TEuropeanNation;
    bells: Word;
    temp_ff: TFoundingFathers;
begin
  //call NewRound method for every unit of that nation
  for i:= 0 to Unit_max do
    if m_Units[i]<>nil then
      if (m_Units[i].GetNation=num_Nation) then
        m_Units[i].NewRound;
  if GetNation(num_nation)=nil then Exit;
  if (GetNation(num_nation).IsEuropean) then
  begin
    //call NewRound method for every colony
    ENat:= GetNation(num_Nation) as TEuropeanNation;
    if not Autumn then //only in spring we produce, to avoid production twice a year
    begin
      for i:= 0 to Colony_max do
        if m_Colonies[i]<>nil then
          if (m_Colonies[i].GetNation=num_Nation) then
          begin
            bells:= 0;
            m_Colonies[i].NewRound(m_Map, ENat.HasFoundingFather(ffHudson),
                                   ENat.HasFoundingFather(ffJefferson),
                                   ENat.HasFoundingFather(ffPenn), bells);
            ENat.AddLibertyBells(bells);
            //following should be implemented in TColony and not here
            if m_Colonies[i].GetStore(gtFood)>=200 then
            begin
              //time for new inhabitant
              m_Colonies[i].RemoveFromStore(gtFood, 200);
              //creates new unit and sets its location to America
              NewUnit(utColonist, num_nation, m_Colonies[i].GetPosX, m_Colonies[i].GetPosY).SetLocation(ulAmerica);
            end;//if
          end;//if
      //check if there are enough liberty bells for the next founding father
      if (ENat.GetNextFoundingFather<>ffNone) and
         (ENat.GetLibertyBells>=GetRequiredLibertyBells(ENat.GetPresentFoundingFathers+1)) then
      begin
        //save the founding father for later use
        temp_ff:= ENat.GetNextFoundingFather;
        //Sets founding father's presence to true; liberty bells and next ff will
        //   be adjusted by this procedure, too.
        ENat.SetFoundingFather(ENat.GetNextFoundingFather, true);
        { Add some effects of founding fathers that take effect immediately after
          they joind congress. }
        case temp_ff of
          //Jakob Fugger clears all boycotts.
          ffFugger: ENat.UndoAllBoycotts;
          //John Paul Jones gives a new frigate at no cost.
          ffJones: begin
                     //Does this nation have a valid spawnpoint?
                     if ENat.HasValidSpawnpoint then
                       //Yes, so spawn frigate at that point.
                       NewUnit(utFrigate, num_Nation, ENat.GetSpawnpointX, ENat.GetSpawnpointY).SetLocation(ulAmerica)
                     else
                       //Spawn frigate at the rightmost field in the middle of the map, it's the best guess here.
                       NewUnit(utFrigate, num_Nation, cMap_X-1, cMap_Y div 2).SetLocation(ulAmerica);
                   end;//case ffJones
        end;//case
        { To Do:
          ======
         **  - add message for selection of next Founding father in case of player
         **    nation or let AI choose the next founding father in case of other
         **    nation
         ************
        }
      end;//if enough liberty bells
    end;//if
  end; //if European
end;//proc

procedure TData.UpdateAITasks(const num_Nation: LongInt);
begin
  { To Do:
    ======
   **  - add A LOT more tasks for computer-controlled nations
   ****************
  }

  //handle tasks/AI for European nations
  if (cMinEuropean<=num_nation) and (num_Nation<=cMaxEuropean) then
    UpdateAITasksEuropean(num_Nation);

  //handle tasks/AI for Indians
  if (cMinIndian<=num_nation) and (num_Nation<=cMaxIndian) then
    UpdateAITasksIndian(num_Nation);
end;//proc

procedure  TData.UpdateAITasksEuropean(const num_Nation: LongInt);
const construction_list: array [0..3] of record
                           bt: TBuildingType;
                           level: Byte;
                         end //record
                         =(
                            (bt: btCarpenter;  level: 2), //Sägewerk
                            (bt: btWarehouse;  level: 1), //Lagerhaus
                            (bt: btChurch;     level: 1), //Kirche
                            (bt: btBlacksmith; level: 2) //Schmiede
                          );
var u_arr: TUnitArr;
    done, need_ship, ship_arrived: Boolean;
    i, j, k: Integer;
    new_task: TTask;
    preferredType: TFoundingFatherType;
    ff_arr: TFoundingFatherArray;
    EuroNat: TEuropeanNation;
    col_arr: TColonyArr;
    work_unit: TUnit;
    src_x, src_y, dest_x, dest_y: Integer;
    hammers, tools: Word;
    AIGoTo: TGoToTask;
begin
  //Is this really a European nation?
  if (num_Nation<cMinEuropean) or (num_Nation>cMaxEuropean) then Exit;

  //Does this nation have any units?
  // It usually should have, but maybe it lost them during war with another
  // nation. In this case we will give the usual start units for this nation.
  if (length(GetAllShips(num_Nation))=0) then
  begin
    SpawnEuropeanNation(num_nation,cSpawnpointsAmerica[0].x, cSpawnpointsAmerica[0].y);
  end;//if

  //Does this nation have any colonies yet?
  // If not, let a ship find a place to build a colony.
  if (length(GetColonyList(num_Nation))<1) then
  begin
    u_arr:= GetAllShips(num_Nation);
    done:= false;
    for i:=0 to high(u_arr) do
    begin
      if ((not done) and (u_arr[i].GetTask=nil)
         and (u_arr[i].EmbarkedPassengers>0)) then
      begin
        new_task:= TFindLandForColonyTask.Create(u_arr[i], GetMap, self);
        u_arr[i].SetTask(new_task);
        done:= true;
      end;
    end;//for
  end;//if

  EuroNat:= (GetNation(num_Nation) as TEuropeanNation);

  //Set next founding father to join national congress - computer-controlled, European players only.
  if ((num_Nation<>PlayerNation) and (GetNation(num_Nation).IsEuropean)) then
  begin
    if EuroNat.GetNextFoundingFather=ffNone then
    begin
      done := false;
      ff_arr:= EuroNat.GetFoundingFatherSelection;
      case num_Nation of
        cNationEngland: preferredType:= fftReligious;
        cNationFrance:  preferredType:= fftExploration;
        cNationSpain:   preferredType:= fftMilitary;
        cNationHolland: preferredType:= fftTrade;
      else preferredType:= fftPolitical; //should never occur
      end;//case

      i:= Low(ff_arr);
      while (i<=High(ff_arr)) and not done do
      begin
        if (ff_arr[i]<>ffNone) and (GetFoundingFatherType(ff_arr[i])=preferredType) then
        begin
          EuroNat.SetNextFoundingFather(ff_arr[i]);
          done := true;
        end;//if
        i:= i+1;
      end;//while

      //Has a new founding father been set yet?
      if not done then
      begin
        //still need to set one, so go through all of them
        for i:= Ord(Low(TFoundingFathers)) to Ord(High(TFoundingFathers)) do
          if (not EuroNat.HasFoundingFather(TFoundingFathers(i))
              and (ffNone<>TFoundingFathers(i))) then
          begin
            EuroNat.SetNextFoundingFather(TFoundingFathers(i));
          end;//if
      end;//if not done
    end;//if no next founding father
  end;//if European

  //now to the colonies
  col_arr:= GetColonyList(num_Nation);
  for i:=0 to High(col_arr) do
  begin
    //Is there any construction stuff going on?
    if (col_arr[i].GetCurrentConstruction=btNone) then
    begin
      //No, so let's select a new one.
      done:= false;
      //Check for dock, we build that first for all colonies at the coast.
      if (col_arr[i].GetBuildingLevel(btDock)=0) and (col_arr[i].AdjacentWater(GetMap)) then
      begin
        col_arr[i].SetCurrentConstruction(btDock);
        done:= true;
      end;//if
      //check the list of things to construct, if we have nothing else yet
      j:=0;
      while (not done) and (j<=High(construction_list)) do
      begin
        if (col_arr[i].GetBuildingLevel(construction_list[j].bt)<construction_list[j].level) then
        begin
          //building has not the level within the list yet, so construct it
          col_arr[i].SetCurrentConstruction(construction_list[j].bt);
          done := true;
        end;//if
        j:= j+1;
      end;//while
    end;//if no construction going on

    if (col_arr[i].GetCurrentConstruction<>btNone) then
    begin
      //If we have something to construct, we have to make sure that there is
      // someone who gets the wood and ore for the construction.
      GetBuildingCost(col_arr[i].GetCurrentConstruction,
                      col_arr[i].GetBuildingLevel(col_arr[i].GetCurrentConstruction)+1,
                      hammers, tools);

      { ************************************
        **** wood/hammers-related stuff ****
        ************************************ }
      if (col_arr[i].GetFieldProduction(GetMap, gtWood, false)=0) and (hammers<>0)
         and (col_arr[i].GetStore(gtWood)+col_arr[i].GetStore(gtHammer)<hammers) then
      begin
        //no wood production or not enough wood in store, set someone to chop sticks
        done:= false;
        for j:= -1 to 1 do
          for k:= -1 to 1 do
            if (not done) and (col_arr[i].GetUnitInField(j,k)<>nil) then
            begin
              work_unit:= col_arr[i].GetUnitInField(j,k);
              src_x:= j;
              src_y:= k;
              done:= true;
            end;//if
        if done then
        begin
          //found someone, find a field where it would produce the most wood
          dest_x:= 0;
          dest_y:= 0;
          col_arr[i].GetBestFieldForGood(dest_x, dest_y, GetMap, gtWood, work_unit);
          //found a place?
          if (dest_x<>0) or (dest_y<>0) then
          begin
            //remove unit from old field
            col_arr[i].SetUnitInField(src_x, src_y, nil);
            //set the unit to its new destination
            col_arr[i].SetUnitInField(dest_x, dest_y, work_unit, gtWood);
          end;//if destination found
        end;//if done
      end;//if not enough wood production

      //check for carpenter
      //Aren't there enough hammers?
      if ((col_arr[i].GetStore(gtHammer)<hammers) and (hammers<>0)
        //...and is there enough wood in store to finish the building?
        and (col_arr[i].GetStore(gtHammer)+col_arr[i].GetStore(gtWood)>=hammers)
        //... and is there no carpenter yet?
        and (col_arr[i].GetTotalProduction(btCarpenter, false, false)<=0)) then
      begin
        //We need a carpenter, select one.
        done:= false;
        src_x:= 0;
        src_y:= 0;
        work_unit:= nil;
        //search for a unit in the fields
        for j:= -1 to 1 do
          for k:= -1 to 1 do
            //We just pick the first unit we can get - not really smart, but
            // that will do for now.
            if (not done) and (col_arr[i].GetUnitInField(j,k)<>nil)
              and ((j<>0) or (k<>0)) then
            begin
              src_x:= j;
              src_y:= k;
              work_unit:= col_arr[i].GetUnitInField(j,k);
              done:= true;
            end;//if
        //So let's hope we found someone.
        if (done and (work_unit<>nil)) then
        begin
          //remove unit from field
          col_arr[i].SetUnitInField(src_x, src_y, nil);
          //...and let it do the carpenter's job
          // --- this line should always return zero at the moment, but better
          //     be safe than sorry
          dest_x:= col_arr[i].GetFirstFreeBuildingSlot(btCarpenter);
          if (dest_x<>-1) then
            col_arr[i].SetUnitInBuilding(btCarpenter, dest_x, work_unit);
        end;//if unit to place in carpenter's house found
      end;//if carpenter required

      //If there is no wood left, the carpenter unit can be removed and chop
      // wood instead.
      if (col_arr[i].GetStore(gtWood)=0)
        and (col_arr[i].GetTotalProduction(btCarpenter, false, false)>0) then
      begin
        src_x:= col_arr[i].GetLastOccupiedBuildingSlot(btCarpenter);
        if (src_x<>-1) then
        begin
          done:= false;
          col_arr[i].GetBestFieldForGood(dest_x, dest_y, GetMap, gtWood);
          //found a place?
          if (dest_x<>0) or (dest_y<>0) then
          begin
            work_unit:= col_arr[i].GetUnitInBuilding(btCarpenter, src_x);
            //remove unit from building
            col_arr[i].SetUnitInBuilding(btCarpenter, src_x, nil);
            //set the unit to its new destination
            col_arr[i].SetUnitInField(dest_x, dest_y, work_unit, gtWood);
          end;//if destination found
        end;//if unit found
      end;//if carpenter can be removed

      { *********************************
        **** ore/tools-related stuff ****
        ********************************* }
      //Do we need someone to mine ore for tools?
      if (tools>0)
        //Make sure we have hammers before going for tools.
        and (col_arr[i].GetStore(gtWood)+col_arr[i].GetStore(gtHammer)>=hammers)
        //...and we don't have enough ore yet
        and (col_arr[i].GetStore(gtOre)+col_arr[i].GetStore(gtTool)<tools)
        //...and we don't have any ore production yet
        and (col_arr[i].GetFieldProduction(GetMap, gtOre, false)=0) then
      begin
        //no ore production and not enough ore in store, set someone to mine for ore
        done:= false;
        for j:= -1 to 1 do
          for k:= -1 to 1 do
            if (not done) and (col_arr[i].GetUnitInField(j,k)<>nil) then
            begin
              work_unit:= col_arr[i].GetUnitInField(j,k);
              src_x:= j;
              src_y:= k;
              done:= true;
            end;//if
        if done then
        begin
          //found someone, find a field where it would produce the most ore
          dest_x:= 0;
          dest_y:= 0;
          col_arr[i].GetBestFieldForGood(dest_x, dest_y, GetMap, gtOre, work_unit);
          //found a place?
          if (dest_x<>0) or (dest_y<>0) then
          begin
            //remove unit from old field
            col_arr[i].SetUnitInField(src_x, src_y, nil);
            //set the unit to its new destination and mine ore
            col_arr[i].SetUnitInField(dest_x, dest_y, work_unit, gtOre);
          end;//if destination found
        end;//if done
      end;//if ore miner needed

      //Are there enough tools?
      if ((col_arr[i].GetStore(gtTool)<tools) and (tools<>0)
        //...and is there enough ore in store to finish the building?
        and (col_arr[i].GetStore(gtTool)+col_arr[i].GetStore(gtOre)>=tools)
        //... and is there no smith yet?
        and (col_arr[i].GetTotalProduction(btBlacksmith, false, false)<=0)) then
      begin
        //We need a smith, select one.
        done:= false;
        src_x:= 0;
        src_y:= 0;
        work_unit:= nil;
        //search for a unit in the fields
        for j:= -1 to 1 do
          for k:= -1 to 1 do
            //We just pick the first unit we can get - not really smart, but
            // that will do for now.
            if (not done) and (col_arr[i].GetUnitInField(j,k)<>nil)
              and ((j<>0) or (k<>0)) then
            begin
              src_x:= j;
              src_y:= k;
              work_unit:= col_arr[i].GetUnitInField(j,k);
              done:= true;
            end;//if
        //So let's hope we found someone.
        if (done and (work_unit<>nil)) then
        begin
          //remove unit from field
          col_arr[i].SetUnitInField(src_x, src_y, nil);
          //...and let it do the smith's job
          // --- this line should always return zero at the moment, but better
          //     be safe than sorry
          dest_x:= col_arr[i].GetFirstFreeBuildingSlot(btBlacksmith);
          if (dest_x<>-1) then
            col_arr[i].SetUnitInBuilding(btBlacksmith, dest_x, work_unit);
        end;//if unit to place in smith's house found
      end;//if smith required

      //If there is no ore left, the smith unit can be removed and mine ore
      // instead.
      if (col_arr[i].GetStore(gtOre)=0)
        and (col_arr[i].GetTotalProduction(btBlacksmith, false, false)>0) then
      begin
        src_x:= col_arr[i].GetLastOccupiedBuildingSlot(btBlacksmith);
        if (src_x<>-1) then
        begin
          done:= false;
          col_arr[i].GetBestFieldForGood(dest_x, dest_y, GetMap, gtOre);
          //found a place?
          if (dest_x<>0) or (dest_y<>0) then
          begin
            work_unit:= col_arr[i].GetUnitInBuilding(btBlacksmith, src_x);
            //remove unit from building
            col_arr[i].SetUnitInBuilding(btBlacksmith, src_x, nil);
            //set the unit to its new destination
            col_arr[i].SetUnitInField(dest_x, dest_y, work_unit, gtOre);
          end;//if destination found
        end;//if unit found
      end;//if smith can be removed
    end;//if construction in progress

    // ---- check for need to sell goods ----
    need_ship:= false;
    src_x:= (col_arr[i].GetBuildingLevel(btWarehouse)+1)*80; //set limit to 80 of 100
    //check produced goods
    for j:= Ord(gtCoat) downto Ord(gtRum) do
    begin
      if not need_ship and (col_arr[i].GetStore(TGoodType(j))>=src_x)
         and (not EuroNat.IsBoycotted(TGoodType(j))) then
        need_ship:= true;
    end;//for j
    //check planted stuff/ ores
    for j:= Ord(gtSilver) downto Ord(gtSugar) do
    begin
      if not need_ship and (col_arr[i].GetStore(TGoodType(j))>=src_x)
         and (not EuroNat.IsBoycotted(TGoodType(j))) then
        need_ship:= true;
    end;//for j
    
    if need_ship then
    begin
      //try to find ship
      u_arr:= GetAllShips(num_Nation);
      done:= false;
      ship_arrived:= false;
      for k:= 0 to High(u_arr) do
      begin
        if (not done) then
        begin
          if (u_arr[k].GetTask<>nil) then
          begin
            if (u_arr[k].GetTask.GetType=ttGoTo) then
            begin
              AIGoTo:= u_arr[k].GetTask as TGoToTask;
              if (AIGoTo.DestinationX=col_arr[i].GetPosX) and (AIGoTo.DestinationY=col_arr[i].GetPosY) then
                //there already is a ship on the way, don't worry
                done:= true;
            end;//if
          end//Task<>nil
          else begin
            //if the ship is already there and has some free slots, then this could be ours
            done:= (u_arr[k].GetPosX=col_arr[i].GetPosX) and (u_arr[k].GetPosY=col_arr[i].GetPosY)
                   and (u_arr[k].GetLocation=ulAmerica) and (u_arr[k].FreeCapacity>0);
            if done then
            begin
              ship_arrived:= true;
              dest_x:= k;//save index for later use
            end;  
          end;//else  
        end;//if not done
      end;//for k
      
      //Are we done yet?
      if not done then
      begin
        //No, we aren't, let's find a free ship.
        for k:= 0 to High(u_arr) do
        begin
          if not done and (u_arr[k].GetTask=nil) and (u_arr[k].FreeCapacity>0)
            and (u_arr[k].GetLocation=ulAmerica) then
          begin
            //found our ship, let's move it to the colony
            AIGoTo:= TGoToTask.Create(u_arr[k], col_arr[i].GetPosX, col_arr[i].GetPosY,
                                      GetMap, col_arr[i].GetPosX, col_arr[i].GetPosY);
            u_arr[k].SetTask(AIGoTo);
            done:= true;
          end;//if
        end;//for k
      end;//if not done
      
      //Has a ship arrived at colony?
      if ship_arrived then
      begin
        //load all possible stuff onto the ship
        for j:= Ord(gtCoat) downto Ord(gtRum) do
        begin
          if (col_arr[i].GetStore(TGoodType(j))>=src_x) and (not EuroNat.IsBoycotted(TGoodType(j)))
             and (u_arr[dest_x].FreeCapacity>0) then
          begin
            //load stuff onto ship
            while (col_arr[i].GetStore(TGoodType(j))>0) and (u_arr[dest_x].FreeCapacity>0) do
            begin
              dest_y:= col_arr[i].GetStore(TGoodType(j));
              if (dest_y>100) then dest_y:= 100; //cut down to 100 at most
              if u_arr[dest_x].LoadGood(TGoodType(j), dest_y) then
                col_arr[i].RemoveFromStore(TGoodType(j), dest_y);
              //We can ignore RemoveFromStore()'s return value here, because we
              // know there is enough of that good.
            end;//while
          end;//if
        end;//for j
        
        for j:= Ord(gtSilver) downto Ord(gtSugar) do
        begin
          if (col_arr[i].GetStore(TGoodType(j))>=src_x) and (not EuroNat.IsBoycotted(TGoodType(j)))
             and (u_arr[dest_x].FreeCapacity>0) then
          begin
            //load stuff onto ship
            while (col_arr[i].GetStore(TGoodType(j))>0) and (u_arr[dest_x].FreeCapacity>0) do
            begin
              dest_y:= col_arr[i].GetStore(TGoodType(j));
              if (dest_y>100) then dest_y:= 100; //cut down to 100 at most
              if u_arr[dest_x].LoadGood(TGoodType(j), dest_y) then
                col_arr[i].RemoveFromStore(TGoodType(j), dest_y);
              //We can ignore RemoveFromStore()'s return value here, because we
              // know there is enough of that good.
            end;//while
          end;//if
        end;//for j
        
        //Ship is loaded with goods, send it to Europe.
        if EuroNat.HasValidSpawnpoint then
          AIGoTo:= TGoToEuropeTask.Create(u_arr[dest_x], EuroNat.GetSpawnpointX, EuroNat.GetSpawnpointY, GetMap)
        else
          AIGoTo:= TGoToEuropeTask.Create(u_arr[dest_x], cMap_X-1, cMap_Y div 2, GetMap);
        u_arr[dest_x].SetTask(AIGoTo);
      end;//if ship_arrived
    end;//if need_ship
  end;//for i (col_arr)
  
  //Handle ships in Europe.
  u_arr:= GetAllShipsInEurope(num_Nation);
  for i:= 0 to High(u_arr) do
  begin
    //unload/sell stuff
    for j:= 0 to 5 do
    begin
      if (u_arr[i].GetCargoAmountBySlot(j)>0) and (not EuroNat.IsBoycotted(u_arr[i].GetCargoGoodBySlot(j)))
         and (u_arr[i].GetCargoGoodBySlot(j) in [gtSugar..gtSilver, gtRum..gtCoat]) then
      begin
        //unload it
        dest_x:= u_arr[i].UnloadGood(u_arr[i].GetCargoGoodBySlot(j), u_arr[i].GetCargoAmountBySlot(j));
        //recalculate nation's gold
        EuroNat.SellGood(u_arr[i].GetCargoGoodBySlot(j), dest_x);
      end;//if
    end;//for j
    //get back to America
    u_arr[i].SendToNewWorld;
  end;//for i (u_arr / ships in Europe)
  
end;//proc

procedure  TData.UpdateAITasksIndian(const num_Nation: LongInt);
begin
  { * **TODO:********************* *
    * still empty, more work to do *
    * **************************** * }
end;//proc

procedure TData.ProcessNationsBeforePlayer;
var i: LongInt;
begin
  for i:= cMinNations to PlayerNation-1 do
  begin
    UpdateAITasks(i);
    NewRound(i);
  end;//for
end;//proc

procedure TData.ProcessNationsAfterPlayer;
var i: LongInt;
begin
  for i:= PlayerNation+1 to cMaxNations do
  begin
    UpdateAITasks(i);
    NewRound(i);
  end;//for
  AdvanceYear;
end;//proc

function TData.SaveData(const n: Word; var err: string): Boolean;
var fs: TFileStream;
    i, temp: Integer;
    temp_str: string;
begin
  { files:
      data<n>.vdd - simple data
      map<n>.vmd - map itself
      units<n>.vud - all units
      colony<n>.vcd - all colonies
      nations<n>.vnd - european nations
  }
  if m_Map=nil then
  begin
    err:= 'TData.SaveData: no map supplied.';
    Result:= False;
    Exit;
  end;//if
  err:= 'no error';
  if not DirectoryExists(GetPathBase+save_path) then
    if not ForceDirectories(GetPathBase+save_path) then
    begin
      err:= 'TData.SaveData: could not create directory "'+GetPathBase+save_path+'" for saves.';
      Result:= False;
      Exit;
    end;//if

  fs:= nil;
  try
    fs:= TFileStream.Create(GetPathBase+save_path +'data'+IntToStr(n)+'.vdd', fmCreate or fmShareDenyNone);
  except
    if fs<>nil then fs.Free;
    err:= 'TData.SaveData: could not create file "'+GetPathBase+save_path +'data'+IntToStr(n)+'.vdd'+'".';
    Result:= False;
    Exit;
  end;//tryxcept

  Result:= (fs.Write(cDataFileHeader[1], sizeof(cDataFileHeader))=sizeof(cDataFileHeader));
  Result:= Result and (fs.Write(Year, sizeof(Year))=sizeof(Year));
  Result:= Result and (fs.Write(Autumn, sizeof(Autumn))=sizeof(Autumn));
  Result:= Result and (fs.Write(player_nation, sizeof(player_nation))=sizeof(player_nation));
  //write player's name
  temp_str:= TEuropeanNation(GetNation(player_nation)).GetLeaderName;
  temp:= length(temp_str);
  Result:= Result and (fs.Write(temp, sizeof(Integer))=sizeof(Integer));
  Result:= Result and (fs.Write(temp_str[1], temp)=temp);
  fs.Free;
  fs:= nil;
  if not Result then
  begin
    err:= 'TData.SaveData: Error while writing data file "'+GetPathBase+save_path +'data'+IntToStr(n)+'.vdd';
    Exit;
  end;//if

  //map
  if m_Map<>nil then
  begin
    Result:= Result and m_Map.SaveToFile(GetPathBase+save_path +'map'+IntToStr(n)+'.vmd');
  end//if
  else begin
    //no map specified
    Result:= False;
    Exit;
  end;//func
  if not Result then
  begin
    err:= 'TData.SaveData: Error while writing map file "'+GetPathBase+save_path +'map'+IntToStr(n)+'.vmd';
    Exit;
  end;//if

  //units
  temp:= 0;
  for i:=0 to Unit_max do
    if m_Units[i]<>nil then
      if not (m_Units[i].GetLocation in [ulEmbarked, ulInColony]) then temp:= temp+1;
  try
    fs:= TFileStream.Create(GetPathBase+save_path +'units'+IntToStr(n)+'.vud', fmCreate or fmShareDenyNone);
  except
    if fs<>nil then fs.Free;
    Result:= False;
    err:= 'TData.SaveData: could not create unit file "'+GetPathBase+save_path +'units'+IntToStr(n)+'.vud".';
    Exit;
  end;//tryxcept
  Result:= Result and (fs.Write(cUnitFileHeader[1], sizeof(cUnitFileHeader))=sizeof(cUnitFileHeader));
  Result:= Result and (fs.Write(temp, sizeof(Integer))=sizeof(Integer));
  for i:= 0 to Unit_max do
    if m_Units[i]<>nil then
      if not (m_Units[i].GetLocation in [ulEmbarked, ulInColony]) then
        Result:= Result and m_Units[i].SaveToStream(fs);
  fs.Free;
  fs:= nil;
  if not Result then
  begin
    err:= 'TData.SaveData: Error while writing unit file "'+GetPathBase+save_path +'units'+IntToStr(n)+'.vud';
    Exit;
  end;//if

  //colonies
  temp:= 0;
  for i:=0 to Colony_max do
    if m_Colonies[i]<>nil then temp:= temp+1;
  try
    fs:= TFileStream.Create(GetPathBase+save_path +'colony'+IntToStr(n)+'.vcd', fmCreate or fmShareDenyNone);
  except
    if fs<>nil then fs.Free;
    Result:= False;
    Exit;
  end;//tryxcept
  Result:= Result and (fs.Write(cColonyFileHeader[1], sizeof(cColonyFileHeader))=sizeof(cColonyFileHeader));
  Result:= Result and (fs.Write(temp, sizeof(Integer))=sizeof(Integer));
  for i:= 0 to Colony_max do
    if m_Colonies[i]<>nil then Result:= Result and m_Colonies[i].SaveToStream(fs);
  fs.Free;
  fs:= nil;
  if not Result then
  begin
    err:= 'TData.SaveData: Error while writing colony file "'+GetPathBase+save_path +'colony'+IntToStr(n)+'.vcd';
    Exit;
  end;

  //nations
  try
    fs:= TFileStream.Create(GetPathBase+save_path +'nations'+IntToStr(n)+'.vnd', fmCreate or fmShareDenyNone);
  except
    if fs<>nil then fs.Free;
    Result:= False;
    Exit;
  end;//tryxcept

  for i:= cMinEuropean to cMaxEuropean do
    Result:= Result and Nations[i].SaveToStream(fs);
  fs.Free;
  fs:= nil;
  if not Result then
    err:= 'TData.SaveData: Error while writing nation file "'+GetPathBase+save_path +'nations'+IntToStr(n)+'.vnd';

end;//func SaveData

function TData.LoadData(const n: Word; var err: string): Boolean;
var fs: TFileStream;
    temp_str: string;
    i, temp: Integer;
    temp_nat: TNation;
    temp_unit: TUnit;
    temp_colony: TColony;
begin
  Result:= False;
  DeInitTribes;
  if m_Map=nil then InitializeMap;
  if not DirectoryExists(GetPathBase+save_path) then
  begin
    err:= 'TData.LoadData: could not find directory "'+GetPathBase+save_path+'".';
    Exit;
  end;//if
  { files:
      data<n>.vdd - simple data
      map<n>.vmd - map itself
      units<n>.vud - all units
      colony<n>.vcd - all colonies
      nations<n>.vnd - european nations
  }
  if not (FileExists(GetPathBase+save_path+'data'+IntToStr(n)+'.vdd') and
         FileExists(GetPathBase+save_path+'map'+IntToStr(n)+'.vmd') and
         FileExists(GetPathBase+save_path+'units'+IntToStr(n)+'.vud') and
         FileExists(GetPathBase+save_path+'colony'+IntToStr(n)+'.vcd')) then
  begin
    err:= 'TData.LoadData: could not find one or more of the needed files.';
    Exit;
  end;//if
  err:= 'no error';

  //data file
  try
    fs:= TFileStream.Create(GetPathBase+save_path +'data'+IntToStr(n)+'.vdd', fmOpenRead or fmShareDenyNone);
  except
    if fs<>nil then fs.Free;
    err:= 'TData.LoadData: could not open file "'+GetPathBase+save_path +'data'+IntToStr(n)+'.vdd'+'" for reading.';
    Result:= False;
    Exit;
  end;//tryxcept

  temp_str:= cDataFileHeader;
  Result:= (fs.Read(temp_str[1], sizeof(cDataFileHeader))=sizeof(cDataFileHeader));
  if temp_str<>cDataFileHeader then
  begin
    fs.Free;
    Result:= False;
    err:= 'TData.LoadData: invalid data file header.';
    Exit;
  end;//if
  Result:= Result and (fs.Read(Year, sizeof(Year))=sizeof(Year));
  Result:= Result and (fs.Read(Autumn, sizeof(Autumn))=sizeof(Autumn));
  Result:= Result and (fs.Read(player_nation, sizeof(player_nation))=sizeof(player_nation));
  if player_nation<0 then
  begin
    err:= 'TData.LoadData: got invalid nation count from data file.';
    Result:= False;
    fs.Free;
    Exit;
  end;//if
  //read player's name
  temp:=0;
  Result:= Result and (fs.Read(temp, sizeof(Integer))=sizeof(Integer));
  if (temp<1) or (temp>255) then
  begin
    fs.Free;
    err:= 'TData.LoadData: got invalid string length from data file.';
    Result:= False;
    Exit;
  end;//if
  temp_str:= SpaceString(temp);
  Result:= Result and (fs.Read(temp_str[1], temp)=temp);
  fs.Free;
  fs:= nil;
  if not Result then
  begin
    err:= 'TData.LoadData: Error while reading data file "'+GetPathBase+save_path +'data'+IntToStr(n)+'.vdd';
    Exit;
  end;//if
  for i:= cMinNations to cMaxIndian do
    if Nations[i]<>nil then
    begin
      Nations[i].Destroy;
      Nations[i]:= nil;
    end;//if
  InitializeNations;
  temp_nat:= GetNation(player_nation);
  if temp_nat<>nil then
  begin
    if temp_nat.IsEuropean then
      TEuropeanNation(GetNation(player_nation)).ChangeLeaderName(temp_str)
    else begin
      err:= 'TData.LoadData: got Indian nation for player.';
      Result:= False;
      Exit;
    end;//else
  end;//if

  //load the map
  if m_Map<>nil then Result:= Result and m_Map.LoadFromFile(GetPathBase+save_path+'map'+IntToStr(n)+'.vmd')
  else begin
    err:= 'TData.LoadData: no map supplied.';
    Result:= False;
    Exit;
  end;//if
  if not Result then
  begin
    err:= 'TData.LoadData: error while loading map from "'+GetPathBase+save_path+'map'+IntToStr(n)+'.vmd".';
    Exit;
  end;//if

  //load units
  DeInitUnits;

  try
    fs:= TFileStream.Create(GetPathBase+save_path +'units'+IntToStr(n)+'.vud', fmOpenRead or fmShareDenyNone);
  except
    if fs<>nil then fs.Free;
    Result:= False;
    err:= 'TData.LoadData: could not open unit file "'+GetPathBase+save_path +'units'+IntToStr(n)+'.vud" for reading.';
    Exit;
  end;//tryxcept
  temp_str:= cUnitFileHeader;
  Result:= Result and (fs.Read(temp_str[1], sizeof(cUnitFileHeader))=sizeof(cUnitFileHeader));
  if temp_str<>cUnitFileHeader then
  begin
    Result:= False;
    fs.Free;
    err:= 'TData.LoadData: got invalid unit file header.';
    Exit;
  end;//if
  temp:=0;
  Result:= Result and (fs.Read(temp, sizeof(Integer))=sizeof(Integer));
  if temp<0 then
  begin
    fs.Free;
    Result:= False;
    err:= 'TData.LoadData: got invalid unit count.';
    Exit;
  end;//if

  for i:= 1 to temp do
  begin
    temp_unit:= NewUnit(utCriminal, cNationEngland, 1,1);
    Result:= Result and LoadUnitFromStream(temp_unit, fs);
  end;//for
  fs.Free;
  fs:= nil;

  if not Result then
  begin
    err:= 'TData.LoadData: error while reading unit file "'+GetPathBase+save_path +'units'+IntToStr(n)+'.vud".';
    Exit;
  end;//if

  //load colonies
  DeInitColonies;
  try
    fs:= TFileStream.Create(GetPathBase+save_path +'colony'+IntToStr(n)+'.vcd', fmOpenRead or fmShareDenyNone);
  except
    if fs<>nil then fs.Free;
    Result:= False;
    err:= 'TData.LoadData: could not open colony file "'+GetPathBase+save_path +'colony'+IntToStr(n)+'.vcd".';
    Exit;
  end;//tryxcept
  temp_str:= cColonyFileHeader;
  Result:= Result and (fs.Read(temp_str[1], sizeof(cColonyFileHeader))=sizeof(cColonyFileHeader));
  if temp_str<>cColonyFileHeader then
  begin
    fs.Free;
    Result:= False;
    err:= 'TData.LoadData: invalid colony file header.';
    Exit;
  end;//if
  Result:= Result and (fs.Read(temp, sizeof(Integer))=sizeof(Integer)); //colony count
  if temp<0 then
  begin
    fs.Free;
    Result:= False;
    err:= 'TData.LoadData: got invalid colony count.';
    Exit;
  end;//if

  for i:= 1 to temp do
  begin
    temp_colony:= NewColony(1,1, cNationEngland, 'new colony '+IntToStr(i));
    Result:= Result and LoadColonyFromStream(temp_colony, fs);
  end;//for
  fs.Free;
  fs:= nil;

  if not Result then
  begin
    err:= 'TData.LoadData: error while loading colonies.';
    Exit;
  end;//if

  //nations
  for i:= cMinNations to cMaxIndian do
    if Nations[i]<>nil then
    begin
      Nations[i].Destroy;
      Nations[i]:= nil;
    end;//if
  InitializeNations;
  try
    fs:= TFileStream.Create(GetPathBase+save_path +'nations'+IntToStr(n)+'.vnd', fmOpenRead or fmShareDenyNone);
  except
    if fs<>nil then fs.Free;
    err:= 'TData.LoadData: could not open file "'+GetPathBase+save_path +'nations'+IntToStr(n)+'.vnd'+'" for reading.';
    Result:= False;
    Exit;
  end;//tryxcept
  for i:= cMinEuropean to cMaxEuropean do
  begin
    Result:= Result and Nations[i].LoadFromStream(fs);
    Result:= Result and (Nations[i].GetCount=i);
  end;//for
  fs.Free;
  fs:= nil;

  if not Result then
  begin
    err:= 'TData.LoadData: error while loading nations.';
  end;//if
end;//func LoadData

function TData.LoadUnitFromStream(var AUnit: TUnit; var fs: TFileStream): Boolean;
var i, px, py: LongInt;
    count: Byte;
    temp_unit: TUnit;
    ut: TUnitType;
    us: TUnitState;
    ul: TUnitLocation;
    gt: TGoodType;
begin
  if ((fs=nil) or (AUnit=nil)) then
  begin
    Result:= False;
    Exit;
  end;
  Result:= (fs.Read(AUnit.MovesLeft, sizeof(AUnit.MovesLeft))=sizeof(AUnit.MovesLeft));
  Result:= Result and (fs.Read(px, sizeof(LongInt))=sizeof(LongInt));
  Result:= Result and (fs.Read(py, sizeof(LongInt))=sizeof(LongInt));
  Result:= Result and AUnit.WarpToXY(px, py, nil);
  Result:= Result and (fs.Read(ut, sizeof(TUnitType))=sizeof(TUnitType));
  AUnit.ChangeType(ut);
  Result:= Result and (fs.Read(ul, sizeof(TUnitLocation))=sizeof(TUnitLocation));
  AUnit.SetLocation(ul);
  Result:= Result and (fs.Read(us, sizeof(TUnitState))=sizeof(TUnitState));
  AUnit.SetState(us);
  Result:= Result and (fs.Read(count, sizeof(Byte))=sizeof(Byte));
  AUnit.SetRoundsInOpenSea(count);
  Result:= Result and (fs.Read(i, sizeof(LongInt))=sizeof(LongInt));
  AUnit.ChangeNation(i);
  Result:= Result and (fs.Read(count, sizeof(Byte))=sizeof(Byte));
  AUnit.ChangeAllItems(count);
  //cargo load
  for i:= 0 to 5 do
  begin
    Result:= Result and (fs.Read(count, sizeof(Byte))=sizeof(Byte));
    Result:= Result and (fs.Read(gt, sizeof(TGoodType))=sizeof(TGoodType));
    AUnit.SetCargo(i, count, gt);
  end;//func
  //load passengers
  AUnit.DropAllPassengers;
  count:= 0;
  Result:= Result and (fs.Read(count, sizeof(Byte))=sizeof(Byte));
  if count>6 then
  begin
    Result:= False;
    Exit;
  end;//func
  for i:= 0 to count-1 do
  begin
    temp_unit:= NewUnit(utCriminal, cNationEngland, 1,1);
    Result:= Result and LoadUnitFromStream(temp_unit, fs);
    Result:= Result and AUnit.LoadUnit(temp_unit);
  end;//for
  //tasks are not yet saved, and thus not loaded
  AUnit.SetTask(nil);
end;//func

function TData.LoadColonyFromStream(var AColony: TColony; var fs: TFileStream): Boolean;
var i, j, f_x, f_y: LongInt;
    bt: TBuildingType;
    gt: TGoodType;
    count, temp_b: Byte;
    temp_unit: TUnit;
    temp_str: string;
    temp_Word: Word;
begin
  if ((fs=nil) or (AColony=nil)) then
  begin
    Result:= False;
    Exit;
  end;//if
  Result:= (fs.Read(i, sizeof(Integer))=sizeof(Integer));
  AColony.ChangeNation(i);
  Result:= Result and (fs.Read(f_x, sizeof(Integer))=sizeof(Integer));
  Result:= Result and (fs.Read(f_y, sizeof(Integer))=sizeof(Integer));
  AColony.SetPosition(f_x, f_y);
  //name
  Result:= Result and (fs.Read(i, sizeof(Integer))=sizeof(Integer));
  if (not Result) or ((i<1)) then
  begin
    Result:= False;
    Exit;
  end;//if
  temp_str:= SpaceString(i);
  Result:= Result and (fs.Read(temp_str[1], i)=i);
  AColony.SetName(temp_str);
  //store
  for i:= Ord(Low(TGoodType)) to Ord(High(TGoodType)) do
  begin
    Result:= Result and (fs.Read(temp_Word, sizeof(Word))=sizeof(Word));
    AColony.SetStore(TGoodType(i), temp_Word);
  end;//for
  //buildings
  for i:= Ord(Low(TBuildingType)) to Ord(High(TBuildingType)) do
  begin
    Result:= Result and (fs.Read(temp_b, sizeof(Byte))=sizeof(Byte));
    AColony.SetBuildingLevel(TBuildingType(i), temp_b);
  end;//for
  //current building under construction
  Result:= Result and (fs.Read(bt, sizeof(TBuildingType))=sizeof(TBuildingType));
  AColony.SetCurrentConstruction(bt);

  //*** units in buildings and units in fields ***
  //fields
  for i:= -1 to 1 do
    for j:= -1 to 1 do
      AColony.SetUnitInField(i,j, nil, gtFood);
  //load fields
  // -- count
  Result:= Result and (fs.Read(count, sizeof(Byte))=sizeof(Byte));
  if count>8 then
  begin
    Result:= False;
    Exit;
  end;//if
  for i:= 1 to count do
  begin
    Result:= Result and (fs.Read(f_x, sizeof(LongInt))=sizeof(LongInt));
    Result:= Result and (fs.Read(f_y, sizeof(LongInt))=sizeof(LongInt));
    if ((abs(f_x)>1) or (abs(f_y)>1) or (AColony.GetUnitInField(f_x,f_y)<>nil)) then
    begin
      //invalid x/y-values or unit in field is already present
      Result:= False;
      Exit;
    end;//func
    temp_unit:= NewUnit(utCriminal, cNationEngland, 1,1);
    Result:= Result and LoadUnitFromStream(temp_unit, fs);
    Result:= Result and (fs.Read(gt, sizeof(TGoodType))=sizeof(TGoodType));
    AColony.SetUnitInField(f_x,f_y, temp_unit, gt);
  end;//func
  //buildings
  for i:= Ord(btArmory) to Ord(btBlacksmith) do
    for j:= 0 to 2 do
      AColony.SetUnitInBuilding(TBuildingType(i),j,nil);
  //load units in buildings
  // -- count
  Result:= Result and (fs.Read(count, sizeof(Byte))=sizeof(Byte));
  for i:= 1 to count do
  begin
    temp_unit:= NewUnit(utCriminal, cNationEngland, 1,1);
    Result:= Result and (fs.Read(bt, sizeof(TBuildingType))=sizeof(TBuildingType));
    Result:= Result and (fs.Read(temp_b, sizeof(Byte))=sizeof(Byte));
    if (not (bt in [btArmory..btBlacksmith])) or (temp_b>2) or (AColony.GetUnitInBuilding(bt, temp_b)<>nil) then
    begin
      Result:= False;
      Exit;
    end;//if
    Result:= Result and LoadUnitFromStream(temp_unit, fs);
    AColony.SetUnitInBuilding(bt, temp_b, temp_unit);
  end;//for
end;//func

function TData.GetSaveInfo(const n: Word): string;
var fs: TFileStream;
    status, temp_Autumn: Boolean;
    temp_str: string;
    temp_Year, temp_nation, temp_len: LongInt;
begin
  if ((n=0) or not FileExists(GetPathBase+save_path +'data'+IntToStr(n)+'.vdd')) then
    Result:= '('+lang.GetOthers(osEmpty)+')'
  else begin
    fs:= nil;
    status:= True;
    try
      fs:= TFileStream.Create(GetPathBase+save_path +'data'+IntToStr(n)+'.vdd', fmOpenRead or fmShareDenyNone);
    except
      Result:= '('+lang.GetOthers(osEmpty)+')';
      if fs<>nil then fs.Free;
      Exit;
    end;//tryxcept
    temp_str:= cDataFileHeader;
    status:= (fs.Read(temp_str[1], sizeof(cDataFileHeader))=sizeof(cDataFileHeader));
    if not status or (temp_str<>cDataFileHeader) then
    begin
      fs.Free;
      Result:= '('+lang.GetOthers(osEmpty)+')';
      Exit;
    end;//if
    status:= status and (fs.Read(temp_Year, sizeof(temp_Year))=sizeof(temp_Year));
    status:= status and (fs.Read(temp_Autumn, sizeof(temp_Autumn))=sizeof(temp_Autumn));
    status:= status and (fs.Read(temp_Nation, sizeof(player_nation))=sizeof(player_nation));
    //read player's name
    status:= status and (fs.Read(temp_len, sizeof(Integer))=sizeof(Integer));
    temp_str:= SpaceString(temp_len);
    status:= status and (fs.Read(temp_str[1], temp_len)=temp_len);
    fs.Free;
    fs:= nil;
    if status then
      Result:= temp_str+', '+lang.GetNationName(temp_nation)+', '
              +lang.GetSeason(temp_Autumn)+' '+IntToStr(temp_Year)
    else Result:='('+lang.GetOthers(osEmpty)+')';
  end;//else
end;//func

function TData.GetSaveSlots: TShortStrArr;
var i: Integer;
begin
  {data<n>.vdd - simple data
      map<n>.vmd - map itself
      units<n>.vud - all units
      colony<n>.vcd - all colonies
      nations<n>.vnd - european nations}

  SetLength(Result, 10);
  for i:=1 to 10 do
  begin
    if (FileExists(GetPathBase+save_path +'data'+IntToStr(i)+'.vdd') and FileExists(GetPathBase+save_path +'map'+IntToStr(i)+'.vmd') and FileExists(GetPathBase+save_path +'units'+IntToStr(i)+'.vud') and FileExists(GetPathBase+save_path +'colony'+IntToStr(i)+'.vcd') and FileExists(GetPathBase+save_path +'nations'+IntToStr(i)+'.vnd')) then
    Result[i-1]:= GetSaveInfo(i)
    else Result[i-1]:= '('+lang.GetOthers(osEmpty)+')';
  end;//for
end;//func

function TData.GetPathBase: string;
var i: Integer;
begin
  if base_dir='' then
  begin
    base_dir:= ParamStr(0);
    i:= length(base_dir);
    while i>=1 do
    begin
      if base_dir[i]=path_delimiter then break;
      i:= i-1;
    end;//while
    base_dir:= copy(base_dir, 1, i);
  end;//if
  Result:= base_dir;
end;//func

function TData.GetLang: TLanguage;
begin
  Result:= lang;
end;//func

function TData.GetMap: TMap;
begin
  Result:= m_Map;
end;//func

function TData.GetJobList(const x_shift, y_shift: ShortInt; const UnitType: TUnitType; ACol: TColony): TShortStrArr;
var i: Integer;
begin
  SetLength(Result, Ord(gtSilver)-Ord(gtFood)+1);
  for i:= 0 to High(Result) do
    Result[i]:= lang.GetOthers(osEmpty);
  if ((abs(x_shift)>1) or (abs(y_shift)>1) or (ACol=nil) or (m_Map=nil)) then Exit;

  for i:= Ord(gtFood) to Ord(gtSilver) do
  begin
    Result[i]:= lang.GetUnitName(GetUnitForGood(TGoodType(i)))+':  '
               +IntToStr(m_Map.tiles[ACol.GetPosX+x_shift,ACol.GetPosY+y_shift].GetGoodProduction(TGoodType(i), HasExpertStatus(TGoodType(i), UnitType)))
               +' '+lang.GetGoodName(TGoodType(i));
  end;//for
end;//func

procedure TData.SpawnEuropeanNation(const num_nation: LongInt; const x, y: Byte);
var Ship: TUnit;
    passenger: TUnit;
begin
  if (GetNation(num_nation)<>nil) then
  begin
    if not (GetNation(num_nation) as TEuropeanNation).HasValidSpawnpoint then
      (GetNation(num_nation) as TEuropeanNation).SetSpawnpoint(x, y);
  end;//if
  if num_nation<>cNationHolland then
    Ship:= NewUnit(utCaravel, num_nation, x, y)
  else Ship:= NewUnit(utTradingShip, num_nation, x, y);
  if not GetMap.tiles[Ship.GetPosX, Ship.GetPosY].IsWater then
    Ship.WarpToXY(cMap_X-1, Ship.GetPosY, GetMap);
  passenger:= NewUnit(utColonist, num_nation, Ship.GetPosX, Ship.GetPosY);
  passenger.GiveTools(100);
  Ship.LoadUnit(passenger);
  passenger:= NewUnit(utRegular, num_nation, Ship.GetPosX, Ship.GetPosY);
  Ship.LoadUnit(passenger);
end; //proc

end.