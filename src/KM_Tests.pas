unit KM_Tests;
interface


type
  TKMLauncherTests = class
  private
    class procedure TestKMR_FullVersion;
    class procedure TestKMR_Patch;
    class procedure TestKMR_Tools;

    class procedure TestKP_FullVersion13;
    class procedure TestKP_FullVersion14;
    class procedure TestKP_Patch13;
    class procedure TestKP_Patch14;
    class procedure TestKP_Tools13;
    class procedure TestKP_Tools14;
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
  TestKMR_FullVersion;
  TestKMR_Patch;
  TestKMR_Tools;

  TestKP_FullVersion13;
  TestKP_FullVersion14;

  TestKP_Patch13;
  TestKP_Patch14;

  TestKP_Tools13;
  TestKP_Tools14;

  // Consider tests passed if we did not fail on asserts
  OutputDebugString('Tests passed');
end;


class procedure TKMLauncherTests.TestKMR_FullVersion;
begin
  //
end;


class procedure TKMLauncherTests.TestKMR_Patch;
begin
  //
end;


class procedure TKMLauncherTests.TestKMR_Tools;
begin
  var fn := 'KaM_Remake_Servers_r16397.7z';
  var gv := TKMGameVersion.NewFromString(fn);
  Assert(gv.VersionFrom = 0);
  Assert(gv.VersionTo = 0);
end;


class procedure TKMLauncherTests.TestKP_FullVersion13;
begin
  var fn13wip := 'kp2026-02-07 (Alpha 13 wip r17531).7z';
  var gv13wip := TKMGameVersion.NewFromString(fn13wip);
  Assert(gv13wip.VersionFrom = 0);
  Assert(gv13wip.VersionTo = 17531);

  var fn132 := 'kp2026-02-07 (Alpha 13.2 r17915).7z';
  var gv132 := TKMGameVersion.NewFromString(fn132);
  Assert(gv132.VersionFrom = 0);
  Assert(gv132.VersionTo = 17915);

  var fn132new := 'Knights Province Alpha 13.2.17986.7z';
  var gv132new := TKMGameVersion.NewFromString(fn132new);
  Assert(gv132new.VersionFrom = 0);
  Assert(gv132new.VersionTo = 17986);
end;


class procedure TKMLauncherTests.TestKP_FullVersion14;
begin
  var fn := 'Knights Province 0.14.0.19800.7z';
  var gv := TKMGameVersion.NewFromString(fn);
  Assert(gv.VersionFrom = 0);
  Assert(gv.VersionTo = 19800);
end;


class procedure TKMLauncherTests.TestKP_Patch13;
begin
  var fnwip := 'Knights Province Alpha wip r17541-r17594.zip';
  var gvwip := TKMGameVersion.NewFromString(fnwip);
  Assert(gvwip.VersionFrom = 17541);
  Assert(gvwip.VersionTo = 17594);

  var fn := 'Knights Province Alpha r17866-r17915.zip';
  var gv := TKMGameVersion.NewFromString(fn);
  Assert(gv.VersionFrom = 17866);
  Assert(gv.VersionTo = 17915);

  // 13 did clip the file extension

  var fn3 := 'Knights Province Alpha wip r17541-r17594';
  var gv3 := TKMGameVersion.NewFromString(fn3);
  Assert(gv3.VersionFrom = 17541);
  Assert(gv3.VersionTo = 17594);

  var fn4 := 'Knights Province Alpha r17866-r17915';
  var gv4 := TKMGameVersion.NewFromString(fn4);
  Assert(gv4.VersionFrom = 17866);
  Assert(gv4.VersionTo = 17915);
end;


class procedure TKMLauncherTests.TestKP_Patch14;
begin
  var fn := 'Knights Province Patch r19800-r19880.zip';
  var gv := TKMGameVersion.NewFromString(fn);
  Assert(gv.VersionFrom = 19800);
  Assert(gv.VersionTo = 19880);
end;


class procedure TKMLauncherTests.TestKP_Tools13;
begin
  var fn := 'KnightsProvince DedicatedServer r16500.7z';
  var gv := TKMGameVersion.NewFromString(fn);
  Assert(gv.VersionFrom = 0);
  Assert(gv.VersionTo = 0);
end;


class procedure TKMLauncherTests.TestKP_Tools14;
begin
  var fn := 'Knights Province DedicatedServer r16500.7z';
  var gv := TKMGameVersion.NewFromString(fn);
  Assert(gv.VersionFrom = 0);
  Assert(gv.VersionTo = 0);
end;


end.
