unit KM_Mutex;
interface


function SingleInstanceLock(const aMutex: string; var aHandle: NativeUInt): Boolean;
procedure SingleInstanceUnlock(var aHandle: NativeUInt);


implementation
uses
  Windows, SysUtils;


function SingleInstanceLock(const aMutex: string; var aHandle: NativeUInt): Boolean;
var
  mtx: string;
begin
  // Sanitize and remove structural elements
  mtx := StringReplace(aMutex, '\', '-', [rfReplaceAll]);
  mtx := StringReplace(mtx, ':', '', [rfReplaceAll]);

  // We want the Event to exist even between different sessions on the server
  //mtx := 'Global\' + mtx;

  aHandle := CreateEvent(nil, True, False, PChar(mtx));
  if aHandle = 0 then
    RaiseLastOSError;

  Result := (aHandle <> 0) and (GetLastError <> ERROR_ALREADY_EXISTS);
end;


procedure SingleInstanceUnlock(var aHandle: NativeUInt);
begin
  if aHandle = 0 then Exit; // We didn't have a lock
  CloseHandle(aHandle);
  aHandle := 0;
end;


end.
