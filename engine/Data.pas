unit Data;

interface

uses
  Nation, Language, Units;

const
{$IFDEF Win32}
  data_path = 'data\';
  path_delimiter = '\';
{$ELSE}
  data_path = 'data/';
  path_delimiter = '/';
{$ENDIF}
  america_map_path = data_path +'america'+path_delimiter+'america.vmd';

type
  TData = class
            private
              Year: Integer;
              Autumn: Boolean;
              Nations: array [cMin_Nations..cMaxIndian] of TNation;
              //the units
              m_Units: array of TUnit;
              Unit_length: Integer;
              Unit_max: Integer;
            public
              constructor Create(var aLang: TLanguage);
              destructor Destroy;
              function GetYear: Integer;
              function IsAutumn: Boolean;
              function GetNation(const count: Integer): TNation;
              function GetNationPointer(const count: Integer): PNation;
              procedure AdvanceYear;
              function NewUnit(const TypeOfUnit: TUnitType; const ANation: Integer; X: Integer=1; Y: Integer=1): TUnit;
              function GetFirstUnitInXY(const x, y: Integer): TUnit;
          end;//class

implementation

constructor TData.Create(var aLang: TLanguage);
var i: Integer;
begin
  Year:= 1492;
  Autumn:= False;
  Nations[cNationEngland]:= TEuropeanNation.Create(cNationEngland, aLang.GetNationName(cNationEngland), 'Walter Raleigh');
  Nations[cNationFrance]:= TEuropeanNation.Create(cNationFrance, aLang.GetNationName(cNationFrance), 'Jacques Cartier');
  Nations[cNationSpain]:= TEuropeanNation.Create(cNationSpain, aLang.GetNationName(cNationSpain), 'Christoph Columbus');
  Nations[cNationHolland]:= TEuropeanNation.Create(cNationHolland, aLang.GetNationName(cNationHolland), 'Michiel De Ruyter');
  for i:= cMinIndian to cMaxIndian do
      Nations[i]:= TIndianNation.Create(i, aLang.GetNationName(i));
  
  //units
  Unit_length:= 0;
  SetLength(m_Units, Unit_length);
  Unit_max:= -1;
  
end;//construc

destructor TData.Destroy;
var i: Integer;
begin
  for i:= cMin_Nations to cMaxIndian do
    if Nations[i]<>nil then Nations[i].Destroy;
  for i:=Unit_max downto 0 do
    if m_Units[i]<>nil then m_Units[i].Destroy;
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
begin
  if (Unit_max+2>Unit_length) then
  begin
    SetLength(m_Units, Unit_length+4);
    Unit_length:= Unit_length+4;
  end;//if
  m_Units[Unit_max+1]:= TUnit.Create(TypeOfUnit, GetNationPointer(ANation), X, Y);
  Unit_max:= Unit_max+1;
  Result:= m_Units[Unit_max+1];
end;//proc

function TData.GetFirstUnitInXY(const x, y: Integer): TUnit;
var i: Integer;
begin
  Result:= nil;
  for i:= 0 to Unit_max do
    if ((m_Units[i].GetPosX=x) and (m_Units[i].GetPosY=y)) then
    begin
      Result:= m_Units[i];
      break;
    end;//if
end;//func

end.