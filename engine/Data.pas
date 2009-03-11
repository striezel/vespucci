unit Data;

interface

uses
  Nation, Language, Units, Colony, Tribe, Map, Goods, Classes, SysUtils, Helper;

const
{$IFDEF Win32}
  path_delimiter = '\';
{$ELSE}
  path_delimiter = '/';
{$ENDIF}
  data_path = 'data' + path_delimiter;
  america_map_path = data_path +'america'+path_delimiter+'america.vmd';
  img_path = data_path+'img'+path_delimiter;
  good_img_path = img_path+'goods'+path_delimiter;
  terrain_img_path = img_path+'terrain'+path_delimiter;
  unit_img_path = img_path+'units'+path_delimiter;
  state_img_path = unit_img_path+'state'+path_delimiter;
  colony_img_path = img_path+'colony'+path_delimiter;
  building_img_path = colony_img_path+'building'+path_delimiter;
  tribe_img_path = img_path+'tribe'+path_delimiter;
  save_path = data_path+'saves'+path_delimiter;

  cDataFileHeader = 'VDD';
  cColonyFileHeader = 'VCD';
  cUnitFileHeader = 'VUD';
  cNationFileHeader = 'VND';

type
  TData = class
            private
              Year: LongInt;
              Autumn: Boolean;
              player_nation: LongInt;
              Nations: array [cMin_Nations..cMaxIndian] of TNation;
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
              function LoadUnitFromStream(var AUnit: TUnit; var fs: TFileStream): Boolean;
              function LoadColonyFromStream(var AColony: TColony; var fs: TFileStream): Boolean;
              function LoadNationFromStream(var ANat: TNation; var fs: TFileStream): Boolean;
              procedure InitializeNations;
              procedure InitializeMap;
              procedure InitTribes_America;
              procedure DeInitColonies;
              procedure DeInitTribes;
              procedure DeInitUnits;
            public
              constructor Create(const NumNation_Player: LongInt=cNationEngland);
              destructor Destroy;
              function GetYear: LongInt;
              function IsAutumn: Boolean;
              function PlayerNation: LongInt;
              function GetNation(const count: Integer): TNation;
              procedure AdvanceYear;
              //units
              function NewUnit(const TypeOfUnit: TUnitType; const ANation: Integer; X: Integer=1; Y: Integer=1): TUnit;
              function GetFirstUnitInXY(const x, y: Integer; const OnlyAmerica: Boolean=True): TUnit;
              function GetFirstLazyUnit(const num_Nation: Integer): TUnit;
              function GetAllShips(const numNation: LongInt): TUnitArr;
              function GetAllShipsInXY(const x,y: Integer; const OnlyAmerica: Boolean=True): TUnitArr;
              function GetAllShipsInEurope(const num_nation: Integer): TUnitArr;
              function GetAllNonShipsInEurope(const num_nation: Integer): TUnitArr;
              function GetAllShipsGoingToEurope(const num_nation: Integer): TUnitArr;
              function GetAllShipsGoingToNewWorld(const num_nation: Integer): TUnitArr;
              procedure GetEuropeanQuartett(const num_nation: Integer; var Ships, People, ExpectedSoon, ToNewWorld: TUnitArr);
              //units in colonies
              function GetAllUnitsInColony(const ACol: TColony): TUnitArr;
              //colonies
              function NewColony(const x,y: Byte; const num_Nation: Integer; const AName: ShortString): TColony;
              function GetColonyInXY(const x,y: Byte): TColony;
              function GetColonyList(const num_nation: Integer): TColonyArr;
              function DeleteColony(const x,y: Byte): Boolean;
              //tribes
              function NewTribe(const x,y: Byte; const num_Nation: Integer; const Teaches: TUnitType): TTribe;
              function GetTribeInXY(const x,y: Byte): TTribe;
              //general (settlements)
              function FreeForSettlement(const x,y:Byte): Boolean;
              //others
              procedure NewRound(const num_Nation: Integer);
              function SaveData(const n: Word; var err: string): Boolean;
              function LoadData(const n: Word; var err: string): Boolean;
              function GetSaveInfo(const n: Word): string;
              function GetSaveSlots: TShortStrArr;
              function GetPathBase: string;
              function GetLang: TLanguage;
              function GetMap: TMap;
              function GetJobList(const x_shift, y_shift: ShortInt; const UnitType: TUnitType; ACol: TColony): TShortStrArr;
          end;//class

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
  for i:= cMin_Nations to cMaxIndian do
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
  for i:= cMin_Nations to cMaxIndian do
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
  Nations[cNationEngland]:= TEuropeanNation.Create(cNationEngland, lang.GetNationName(cNationEngland), 'Walter Raleigh');
  Nations[cNationFrance]:= TEuropeanNation.Create(cNationFrance, lang.GetNationName(cNationFrance), 'Jacques Cartier');
  Nations[cNationSpain]:= TEuropeanNation.Create(cNationSpain, lang.GetNationName(cNationSpain), 'Christoph Columbus');
  Nations[cNationHolland]:= TEuropeanNation.Create(cNationHolland, lang.GetNationName(cNationHolland), 'Michiel De Ruyter');
  for i:= cMinIndian to cMaxIndian do
    Nations[i]:= TIndianNation.Create(i, lang.GetNationName(i));
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
      m_Map.Generate(0.7);
    end;
  end
  else begin
    WriteLn('Couldn''t find map file "'+GetPathBase+america_map_path+'". Using generation routine instead.');
    m_Map.Generate(0.7);
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
  if ((count>cMaxIndian) or (count<cMin_Nations)) then
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
      if ((m_Units[i].MovesLeft>0) and (m_Units[i].GetNation=num_Nation) and (m_Units[i].GetLocation=ulAmerica)) then
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
begin
  //call NewRound method for every unit of that nation
  for i:= 0 to Unit_max do
    if m_Units[i]<>nil then
      if (m_Units[i].GetNation=num_Nation) then
        m_Units[i].NewRound;
  //call NewRound method for every colony
  if not Autumn then //only in spring we produce, to avoid production twice a year
    for i:= 0 to Colony_max do
      if m_Colonies[i]<>nil then
        if (m_Colonies[i].GetNation=num_Nation) then
        begin
          m_Colonies[i].NewRound(m_Map);
          //following should be implemented in TColony and not here
          if m_Colonies[i].GetStore(gtFood)>=200 then
          begin
            //time for new inhabitant
            m_Colonies[i].RemoveFromStore(gtFood, 200);
            //creates new unit and sets its location to America
            NewUnit(utColonist, num_nation, m_Colonies[i].GetPosX, m_Colonies[i].GetPosY).SetLocation(ulAmerica);
          end;//if
        end;//if
end;//func

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
    err:= 'TData.SaveData: Error while writing colony file "'+GetPathBase+save_path +'colony'+IntToStr(n)+'.vcd';

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
  for i:= cMin_Nations to cMaxIndian do
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
  for i:= cMin_Nations to cMaxIndian do
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
    Result:= Result and LoadNationFromStream(Nations[i], fs);
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
    temp_unit:= NewUnit(utCriminal, cNationEngland, 1,1);
    Result:= Result and LoadUnitFromStream(temp_unit, fs);
    AColony.SetUnitInBuilding(bt, temp_b, temp_unit);
  end;//for
end;//func

function TData.LoadNationFromStream(var ANat: TNation; var fs: TFileStream): Boolean;
var i: LongInt;
    temp_str: string;
    tr: Byte;
    boycott: Boolean;
begin
  Result:= False;
  if ((fs=nil) or (ANat=nil)) then  Exit;
  Result:= (fs.Read(i, sizeof(LongInt))=sizeof(LongInt));
  ANat.ChangeCount(i);
  Result:= Result and (fs.Read(i, sizeof(LongInt))=sizeof(LongInt));
  if (i<=0) or (i>255) then
  begin
    Result:= False; //string to short or to long
    Exit;
  end;//if
  temp_str:= SpaceString(i);
  Result:= Result and (fs.Read(temp_str[1], i)=i);
  temp_str:= Trim(temp_str);
  ANat.ChangeName(temp_str);
  if ANat.IsEuropean then
  begin
    Result:= Result and (fs.Read(i, sizeof(LongInt))=sizeof(LongInt));
    if (i<=0) or (i>255) then
    begin
      Result:= False; //string to short or to long
      Exit;
    end;//if
    temp_str:= SpaceString(i);
    Result:= Result and (fs.Read(temp_str[1], i)=i);
    (ANat as TEuropeanNation).ChangeLeaderName(Trim(temp_str));
    Result:= Result and (fs.Read(i, sizeof(LongInt))=sizeof(LongInt));
    (ANat as TEuropeanNation).IncreaseGold(i-(ANat as TEuropeanNation).GetGold);
    Result:= Result and (fs.Read(tr, sizeof(Byte))=sizeof(Byte));
    (ANat as TEuropeanNation).ChangeTaxRate(tr);
    for i:= Ord(Low(TGoodType)) to Ord(High(TGoodType)) do
    begin
      Result:= Result and (fs.Read(Boycott, sizeof(Boolean))=sizeof(Boolean));
      if Boycott then (ANat as TEuropeanNation).DoBoycott(TGoodType(i)) else (ANat as TEuropeanNation).UndoBoycott(TGoodType(i));
      Result:= Result and (fs.Read(tr, sizeof(Byte))=sizeof(Byte));
      (ANat as TEuropeanNation).ChangePrice(TGoodType(i), tr);
    end;//for
  end;//if
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

end.