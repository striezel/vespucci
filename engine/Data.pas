unit Data;

interface

uses
  Nation, Language, Units, Colony, Map, Goods;

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
            public
              player_nation: Integer;
              constructor Create(var aLang: TLanguage);
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
          end;//class

implementation

constructor TData.Create(var aLang: TLanguage);
var i: Integer;
begin
  player_nation:= cNationEngland;
  Year:= 1492;
  Autumn:= False;
  Nations[cNationEngland]:= TEuropeanNation.Create(cNationEngland, aLang.GetNationName(cNationEngland), 'Walter Raleigh');
  Nations[cNationFrance]:= TEuropeanNation.Create(cNationFrance, aLang.GetNationName(cNationFrance), 'Jacques Cartier');
  Nations[cNationSpain]:= TEuropeanNation.Create(cNationSpain, aLang.GetNationName(cNationSpain), 'Christoph Columbus');
  Nations[cNationHolland]:= TEuropeanNation.Create(cNationHolland, aLang.GetNationName(cNationHolland), 'Michiel De Ruyter');
  for i:= cMinIndian to cMaxIndian do
      Nations[i]:= TIndianNation.Create(i, aLang.GetNationName(i));
  //units
  SetLength(m_Units, 0);
  Unit_max:= -1;
  //colonies
  SetLength(m_Colonies, 0);
  Colony_max:= -1;
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
  m_Units[Unit_max+1]:= TUnit.Create(TypeOfUnit, GetNationPointer(ANation), X, Y);
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
      if ((m_Units[i].MovesLeft>0) and (m_Units[i].GetNation^.GetCount=num_Nation) and (m_Units[i].GetLocation=ulAmerica)) then
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
  m_Colonies[Colony_max+1]:= TColony.Create(x, y, GetNation(num_nation), AName);
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
      if ((x>=0) and (y>=0)) then
        if GetColonyInXY(x,y)<>nil then
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
      if (m_Units[i].GetNation^.GetCount=num_Nation) then
        m_Units[i].NewRound;
  //call NewRound method for every colony
  for i:= 0 to Colony_max do
    if m_Colonies[i]<>nil then
      if m_Colonies[i].GetNation<>nil then
        if (m_Colonies[i].GetNation.GetCount=num_Nation) then
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

end.