unit Tribe;

interface

uses
  Settlement, Nation, Units, MsgSys;

type
  TTribe = class(TSettlement)
    public
      constructor Create(const X, Y: Integer; const ANation: Integer; KnownFor: TUnitType);
      procedure Teach(var AUnit: TUnit);
    private
      m_KnownFor: TUnitType;
      m_HasTought: array[cMin_Nations..cMaxEuropean] of Boolean;
  end;//class
  PTribe = ^TTribe;

implementation

// **** TTribe functions ****

constructor TTribe.Create(const X, Y: Integer; const ANation: Integer; KnownFor: TUnitType);
var i: Integer;
begin
  //sets Nation and position
  inherited Create(X, Y, ANation);
  for i:= cMin_Nations to cMaxEuropean do
    m_HasTought[i]:= False;
  //set the unit type the tribe is known for
  m_KnownFor:= KnownFor;
end;//construc

procedure TTribe.Teach(var AUnit: TUnit);
begin
  //only servants or free colonists can learn something
  if ((AUnit.GetType<>utServant) and (AUnit.GetType<>utColonist)) then
  begin
    if AUnit.GetType in [utFarmer..utPioneer] then
    begin
      //Show message: "It's a pleasure to see a skilled <whatever>..."
      // Still ToDo();
    end
    else begin
      //Show message, informing player about inappropriate unit type
      // Still ToDo();
    end;//else
  end
  //only European Units can learn from Indians
  else if (AUnit.GetNation in [cMin_Nations..cMaxEuropean]) then
  begin
    //check, if Indians already did teach that nation's units a new skill
    if m_HasTought[AUnit.GetNation] then
    begin
      //Show message, something like "Ihr habt schon etwas von uns gelernt"
      // Still ToDo();
    end//if
    else begin
      //actually teach the unit something
      AUnit.ChangeType(m_KnownFor);
      m_HasTought[AUnit.GetNation]:= True;
    end;//else
  end;//if
end;//proc

end.