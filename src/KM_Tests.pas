unit KM_Tests;
interface


type
  TKMLauncherTests = class
  private
    class procedure TestKP_FullVersion13;
    class procedure TestKP_Patch13;
  public
    class procedure Run;
  end;


implementation
uses
  System.Classes, Winapi.Windows,
  KM_Settings,
  KM_GameVersion;


{ TKMLauncherTests }
class procedure TKMLauncherTests.Run;
begin
  TestKP_FullVersion13;
  TestKP_Patch13;

  // Consider tests passed if we did not fail on asserts
  OutputDebugString('Tests passed');
end;


class procedure TKMLauncherTests.TestKP_FullVersion13;
begin
  var fn := 'Knights Province Alpha 13.2.17986.7z';
  var gv := TKMGameVersion.NewFromString(fn);
  Assert(gv.VersionFrom = 0);
  Assert(gv.VersionTo = 17986);
end;


class procedure TKMLauncherTests.TestKP_Patch13;
begin
  var fn := 'Knights Province Alpha r17866-r17915.zip';
  var gv := TKMGameVersion.NewFromString(fn);
  Assert(gv.VersionFrom = 17866);
  Assert(gv.VersionTo = 17915);
end;


end.
