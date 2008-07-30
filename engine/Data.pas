unit Data;

interface

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
            public
              constructor Create;
              function GetYear: Integer;
              function IsAutumn: Boolean;
              procedure AdvanceYear;
          end;//class

implementation

constructor TData.Create;
begin
  Year:= 1492;
  Autumn:= False;
end;//construc

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

end.