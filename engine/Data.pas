unit Data;

interface

uses
  Nation, Language, Units, Colony, Map, Goods, Classes, SysUtils, Helper;

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
  colony_img_path = img_path+'colony'+path_delimiter;
  save_path = data_path+'saves'+path_delimiter;
  
  cDataFileHeader = 'VDD';
  cColonyFileHeader = 'VCD';

type
  TData = class
            private
              Year: Integer;
              Autumn: Boolean;
              Nations: array [cMin_Nations..cMaxIndian] of TNation;
              //the units
              m_Units: array of TUnit;
              Unit_max: Integer;
              //the colonies
              m_Colonies: array of TColony;
              Colony_max: Integer;
              //language
              lang: TLanguage;
              //relative path
              base_dir: string;
              function GetSaveInfo(const n: Word): string;
            public
              player_nation: Integer;
              constructor Create;
              destructor Destroy;
              function GetYear: Integer;
              function IsAutumn: Boolean;
              function GetNation(const count: Integer): TNation;
              function GetNationPointer(const count: Integer): PNation;
              procedure AdvanceYear;
              //units
              function NewUnit(const TypeOfUnit: TUnitType; const ANation: Integer; X: Integer=1; Y: Integer=1): TUnit;
              function GetFirstUnitInXY(const x, y: Integer; const OnlyAmerica: Boolean=True): TUnit;
              function GetFirstLazyUnit(const num_Nation: Integer): TUnit;
              //colonies
              function NewColony(const x,y: Byte; const num_Nation: Integer; const AName: ShortString): TColony;
              function GetColonyInXY(const x,y: Byte): TColony;
              function FreeForSettlement(const x,y:Byte): Boolean;
              //others
              procedure NewRound(const num_Nation: Integer; AMap: TMap);
              function SaveData(const n: Word; AMap: TMap; var err: string): Boolean;
              function GetSaveSlots: TShortStrArr;
              function GetPathBase: string;
              function GetLang: TLanguage;
          end;//class

implementation

constructor TData.Create;
var i: Integer;
begin
  player_nation:= cNationEngland;
  Year:= 1492;
  Autumn:= False;
  lang:= TLanguage.Create;
  Nations[cNationEngland]:= TEuropeanNation.Create(cNationEngland, lang.GetNationName(cNationEngland), 'Walter Raleigh');
  Nations[cNationFrance]:= TEuropeanNation.Create(cNationFrance, lang.GetNationName(cNationFrance), 'Jacques Cartier');
  Nations[cNationSpain]:= TEuropeanNation.Create(cNationSpain, lang.GetNationName(cNationSpain), 'Christoph Columbus');
  Nations[cNationHolland]:= TEuropeanNation.Create(cNationHolland, lang.GetNationName(cNationHolland), 'Michiel De Ruyter');
  for i:= cMinIndian to cMaxIndian do
      Nations[i]:= TIndianNation.Create(i, lang.GetNationName(i));
  //units
  SetLength(m_Units, 0);
  Unit_max:= -1;
  //colonies
  SetLength(m_Colonies, 0);
  Colony_max:= -1;
  base_dir:= '';
end;//construc

destructor TData.Destroy;
var i: Integer;
begin
  for i:= cMin_Nations to cMaxIndian do
    if Nations[i]<>nil then Nations[i].Destroy;
  for i:=Unit_max downto 0 do
    if m_Units[i]<>nil then m_Units[i].Destroy;
  SetLength(m_Units, 0);
  for i:= Colony_max downto 0 do
    if m_Colonies[i]<>nil then m_Colonies[i].Destroy;
  SetLength(m_Colonies, 0);
  lang.Destroy;
end;//destruc

function TData.GetYear: Integer;
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

function TData.GetNation(const count: Integer): TNation;
begin
  if ((count>cMaxIndian) or (count<cMin_Nations)) then
    Result:= nil
  else
    Result:= Nations[count];
end;//func

function TData.GetNationPointer(const count: Integer): PNation;
begin
  if ((count>cMaxIndian) or (count<cMin_Nations)) then
    Result:= nil
  else
    Result:= @Nations[count];
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

function TData.FreeForSettlement(const x,y:Byte): Boolean;
var i,j: Integer;
begin
  Result:= True;
  for i:= x-2 to x+2 do
    for j:= y-2 to y+2 do
      if ((i>=0) and (j>=0)) then
        if GetColonyInXY(i,j)<>nil then
        begin
          Result:= False;
          Exit; //maybe simple "break;" won't do - we are in a "double loop"
        end;//if
end;//func

procedure TData.NewRound(const num_Nation: Integer; AMap: TMap);
var i: Integer;
begin
  //call NewRound method for every unit of that nation
  for i:= 0 to Unit_max do
    if m_Units[i]<>nil then
      if (m_Units[i].GetNation=num_Nation) then
        m_Units[i].NewRound;
  //call NewRound method for every colony
  for i:= 0 to Colony_max do
    if m_Colonies[i]<>nil then
      if (m_Colonies[i].GetNation=num_Nation) then
      begin
        //we should try to get a valid map instead of just nil in next line
        m_Colonies[i].NewRound(AMap);
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

function TData.SaveData(const n: Word; AMap: TMap; var err: string): Boolean;
var fs: TFileStream;
    i, temp: Integer;
    temp_str: string;
begin
  { files:
      data<n>.vdd - simple data
      map<n>.vmd - map itself
      units<n>.vud - all units
      colony.vcd - all colonies
  }
  if AMap=nil then
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
  if AMap<>nil then
  begin
    Result:= Result and AMap.SaveToFile(GetPathBase+save_path +'map'+IntToStr(n)+'.vmd');
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
    if m_Units[i]<>nil then temp:= temp+1;
  try
    fs:= TFileStream.Create(GetPathBase+save_path +'units'+IntToStr(n)+'.vud', fmCreate or fmShareDenyNone);
  except
    if fs<>nil then fs.Free;
    Result:= False;
    Exit;
  end;//tryxcept
  Result:= Result and (fs.Write(cUnitFileHeader[1], sizeof(cUnitFileHeader))=sizeof(cUnitFileHeader));
  Result:= Result and (fs.Write(temp, sizeof(Integer))=sizeof(Integer));
  for i:= 0 to Unit_max do
    if m_Units[i]<>nil then Result:= Result and m_Units[i].SaveToStream(fs);
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
end;//func

function TData.GetSaveInfo(const n: Word): string;
var fs: TFileStream;
    status, temp_Autumn: Boolean;
    temp_str: string;
    temp_Year, temp_nation, temp_len: Integer;
begin
  if ((n=0) or not FileExists(GetPathBase+save_path +'data'+IntToStr(n)+'.vdd')) then
    Result:= '('+lang.GetEmpty+')'
  else begin
    fs:= nil;
    status:= True;
    try
      fs:= TFileStream.Create(GetPathBase+save_path +'data'+IntToStr(n)+'.vdd', fmOpenRead or fmShareDenyNone);
    except
      Result:= '('+lang.GetEmpty+')';
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
    else Result:='('+lang.GetEmpty+')';
  end;//else
end;//func

function TData.GetSaveSlots: TShortStrArr;
var i: Integer;
begin
  {data<n>.vdd - simple data
      map<n>.vmd - map itself
      units<n>.vud - all units
      colony.vcd - all colonies}

  SetLength(Result, 10);
  for i:=1 to 10 do
  begin
    if (FileExists(GetPathBase+save_path +'data'+IntToStr(i)+'.vdd') and FileExists(GetPathBase+save_path +'map'+IntToStr(i)+'.vmd') and FileExists(GetPathBase+save_path +'units'+IntToStr(i)+'.vud') and FileExists(GetPathBase+save_path +'colony'+IntToStr(i)+'.vcd')) then
    Result[i-1]:= GetSaveInfo(i)
    else Result[i-1]:= '('+lang.GetEmpty+')';
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

end.
