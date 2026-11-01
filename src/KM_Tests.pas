unit KM_Tests;
interface


type
  TKMLauncherTests = class
  private
    class procedure TestKMR_Folder;
    class procedure TestKMR_Patch;
    class procedure TestKMR_Tools;

//    class procedure TestKP13_Version;
//    class procedure TestKP13_Folder;
//    class procedure TestKP13_Package;
//    class procedure TestKP13_Patch;
//    class procedure TestKP13_Tools;

    class procedure TestKP14_Version;
    class procedure TestKP14_Folder;
    class procedure TestKP14_Package;
    class procedure TestKP14_Patch;
    class procedure TestKP14_Tools;
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
  TestKMR_Folder;
  TestKMR_Patch;
  TestKMR_Tools;

//  TestKP13_Version;
//  TestKP13_Folder;
//  TestKP13_Package;
//  TestKP13_Patch;
//  TestKP13_Tools;

  TestKP14_Version;
  TestKP14_Folder;
  TestKP14_Package;
  TestKP14_Patch;
  TestKP14_Tools;

  // Consider tests passed if we did not fail on asserts
  OutputDebugString('Tests passed');
end;


class procedure TKMLauncherTests.TestKMR_Folder;
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
  Assert(gv.RevisionFrom = 0);
  Assert(gv.RevisionTo = 0);
end;


// Version file (without game name)
//class procedure TKMLauncherTests.TestKP13_Version;
//begin
//  var fn1 := 'Alpha 13 wip r17531)';
//  var gv1 := TKMGameVersion.NewFromString(fn1);
//  Assert(gv1.RevisionFrom = 0);
//  Assert(gv1.RevisionTo = 17531);
//
//  var fn2 := 'Alpha 13.2 r17915)';
//  var gv2 := TKMGameVersion.NewFromString(fn2);
//  Assert(gv2.RevisionFrom = 0);
//  Assert(gv2.RevisionTo = 17915);
//
//  // This one fails - no "r" prefix
////  var fn3 := 'Alpha 13.2.17986';
////  var gv3 := TKMGameVersion.NewFromString(fn3);
////  Assert(gv3.RevisionFrom = 0);
////  Assert(gv3.RevisionTo = 17986);
//end;


// Folders used to create the patch
//class procedure TKMLauncherTests.TestKP13_Folder;
//begin
//  var fn1 := 'kp2026-02-07 (Alpha 13 wip r17531)';
//  var gv1 := TKMGameVersion.NewFromString(fn1);
//  Assert(gv1.RevisionFrom = 0);
//  Assert(gv1.RevisionTo = 17531);
//
//  var fn2 := 'kp2026-02-07 (Alpha 13.2 r17915)';
//  var gv2 := TKMGameVersion.NewFromString(fn2);
//  Assert(gv2.RevisionFrom = 0);
//  Assert(gv2.RevisionTo = 17915);
//
//  // This one fails - no "r" prefix
////  var fn3 := 'Knights Province Alpha 13.2.17986';
////  var gv3 := TKMGameVersion.NewFromString(fn3);
////  Assert(gv3.RevisionFrom = 0);
////  Assert(gv3.RevisionTo = 17986);
//end;


// Full builds on server (used to tell player his version is not the latest)
//class procedure TKMLauncherTests.TestKP13_Package;
//begin
//  var fn1 := 'kp2026-02-07 (Alpha 13 wip r17531).7z';
//  var gv1 := TKMGameVersion.NewFromString(fn1);
//  Assert(gv1.RevisionFrom = 0);
//  Assert(gv1.RevisionTo = 17531);
//
//  var fn2 := 'kp2026-02-07 (Alpha 13.2 r17915).7z';
//  var gv2 := TKMGameVersion.NewFromString(fn2);
//  Assert(gv2.RevisionFrom = 0);
//  Assert(gv2.RevisionTo = 17915);
//
//  // This one fails - no "r" prefix
////  var fn3 := 'Knights Province Alpha 13.2.17986.7z';
////  var gv3 := TKMGameVersion.NewFromString(fn3);
////  Assert(gv3.RevisionFrom = 0);
////  Assert(gv3.RevisionTo = 17986);
//end;


// Folders used to create the patch
class procedure TKMLauncherTests.TestKP14_Folder;
begin
  var fn := 'Knights Province 0.14.0 r19800';
  var gv := TKMGameVersion.NewFromString(fn);
  Assert(gv.IsValid);
  Assert(gv.Epoch = 0);
  Assert(gv.Major = 14);
  Assert(gv.Minor = 0);
  Assert(gv.RevisionFrom = 0);
  Assert(gv.RevisionTo = 19800);
end;


// Full builds on server (used to tell player his version is not the latest)
class procedure TKMLauncherTests.TestKP14_Package;
begin
  var fn := 'Knights Province 0.14.0 r19800.7z';
  var gv := TKMGameVersion.NewFromString(fn);
  Assert(gv.IsValid);
  Assert(gv.Epoch = 0);
  Assert(gv.Major = 14);
  Assert(gv.Minor = 0);
  Assert(gv.RevisionFrom = 0);
  Assert(gv.RevisionTo = 19800);
end;


class procedure TKMLauncherTests.TestKP14_Version;
begin
  var fn1a := '0.14.0 r19800';
  var gv1a := TKMGameVersion.NewFromString(fn1a);
  Assert(gv1a.IsValid);
  Assert(gv1a.Epoch = 0);
  Assert(gv1a.Major = 14);
  Assert(gv1a.Minor = 0);
  Assert(gv1a.RevisionFrom = 0);
  Assert(gv1a.RevisionTo = 19800);

  var fn2a := 'Knights Province 0.14.0 r19800-stable';
  var gv2a := TKMGameVersion.NewFromString(fn2a);
  Assert(gv2a.IsValid);
  Assert(gv2a.Epoch = 0);
  Assert(gv2a.Major = 14);
  Assert(gv2a.Minor = 0);
  Assert(gv2a.RevisionFrom = 0);
  Assert(gv2a.RevisionTo = 19800);

// Enforce "version" always containing a full version descriptor
//  var fn1b := 'r19800';
//  var gv1b := TKMGameVersion.NewFromString(fn1b);
//  Assert(gv1b.IsValid);
//  Assert(gv1b.RevisionFrom = 0);
//  Assert(gv1b.RevisionTo = 19800);
end;


//class procedure TKMLauncherTests.TestKP13_Patch;
//begin
//  var fnwip := 'Knights Province Alpha wip r17541-r17594.zip';
//  var gvwip := TKMGameVersion.NewFromString(fnwip);
//  Assert(gvwip.RevisionFrom = 17541);
//  Assert(gvwip.RevisionTo = 17594);
//
//  var fn := 'Knights Province Alpha r17866-r17915.zip';
//  var gv := TKMGameVersion.NewFromString(fn);
//  Assert(gv.RevisionFrom = 17866);
//  Assert(gv.RevisionTo = 17915);
//
//  // 13 did clip the file extension
//
//  var fn3 := 'Knights Province Alpha wip r17541-r17594';
//  var gv3 := TKMGameVersion.NewFromString(fn3);
//  Assert(gv3.RevisionFrom = 17541);
//  Assert(gv3.RevisionTo = 17594);
//
//  var fn4 := 'Knights Province Alpha r17866-r17915';
//  var gv4 := TKMGameVersion.NewFromString(fn4);
//  Assert(gv4.RevisionFrom = 17866);
//  Assert(gv4.RevisionTo = 17915);
//end;


class procedure TKMLauncherTests.TestKP14_Patch;
begin
  var fn := 'Knights Province Patch r19800-r19880.zip';
  var gv := TKMGameVersion.NewFromString(fn);
  Assert(gv.IsValid);
  Assert(gv.Epoch = 0);
  Assert(gv.Major = 0);
  Assert(gv.Minor = 0);
  Assert(gv.RevisionFrom = 19800);
  Assert(gv.RevisionTo = 19880);
end;


//class procedure TKMLauncherTests.TestKP13_Tools;
//begin
//  var fn := 'KnightsProvince DedicatedServer r16500.7z';
//  var gv := TKMGameVersion.NewFromString(fn);
//  Assert(gv.RevisionFrom = 0);
//  Assert(gv.RevisionTo = 0);
//end;


class procedure TKMLauncherTests.TestKP14_Tools;
begin
  var fn := 'Knights Province DedicatedServer r16500.7z';
  var gv := TKMGameVersion.NewFromString(fn);
  Assert(not gv.IsValid);
  Assert(gv.Epoch = 0);
  Assert(gv.Major = 0);
  Assert(gv.Minor = 0);
  Assert(gv.RevisionFrom = 0);
  Assert(gv.RevisionTo = 0);
end;


end.
