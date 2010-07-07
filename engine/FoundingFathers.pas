unit FoundingFathers;

interface

type
  { enumeration type for founding fathers }
  TFoundingFathers = (//trade
                      ffSmith, ffFugger, ffMinuit, ffStuyvesant, ffDeWitt,
                      //exploration
                      ffCoronado, ffHudson, ffLaSalle, ffMagellan, ffDeSoto,
                      //military
                      ffCortes, ffDrake, ffJones, ffRevere, ffWashington,
                      //political
                      ffBolivar, ffFranklin, ffJefferson, ffPaine, ffPocahontas,
                      //religious
                      ffBrebeuf, ffBrewster, ffLasCasas, ffPenn, ffSepulveda);

  { enumeration type for area of founding fathers }
  TFoundingFatherType = (fftTrade, fftExploration, fftMilitary, fftPolitical,
                         fftReligious);
  TFoundingType = TFoundingFatherType; //alias to ease coding a bit

  function GetFoundingFatherType(const ff: TFoundingFathers): TFoundingType;

implementation

function GetFoundingFatherType(const ff: TFoundingFathers): TFoundingType;
begin
  case ff of
    ffSmith..ffDeWitt: Result:= fftTrade;
    ffCoronado..ffDeSoto: Result:= fftExploration;
    ffCortes..ffWashington: Result:= fftMilitary;
    ffBolivar..ffPocahontas: Result:= fftPolitical;
  else Result:= fftReligious;
  end;//case
end;//func

end.
