unit KM_ServerAPI;
interface
uses
  Classes, SysUtils, SyncObjs,
  REST.Client,
  REST.Types,
  WinInet;


type
  TKMServerAPI = class
  private
    fServerAddress: string;
    fClientName: string;

    fRestCS: TCriticalSection; // REST does not cope well with threaded usage
    fRESTClient: TRESTClient;
    fRESTRequest: TRESTRequest;

    procedure Request2(aMethod: TRESTRequestMethod; const aResource: string; aOnDone, aOnFail: TProc<string>);
    procedure RequestAsync(aMethod: TRESTRequestMethod; const aResource: string; aOnDone, aOnFail: TProc<string>);
  public
    constructor Create(const aServerAddress: string; const aClientName: string);
    destructor Destroy; override;

    procedure FileListGet(aOnDone, aOnFail: TProc<string>);
    procedure FileGet(const aUrl: string; aStream: TStream; aOnProgress: TProc);
  end;


implementation
uses
  KM_Settings;


{ TKMServerAPI }
constructor TKMServerAPI.Create(const aServerAddress: string; const aClientName: string);
begin
  inherited Create;

  fServerAddress := aServerAddress;
  fClientName := aClientName;

  fRestCS := TCriticalSection.Create;

  fRESTClient := TRESTClient.Create(nil);
  fRESTClient.Accept := 'application/json';
  fRESTClient.AcceptCharset := 'UTF-8';
  fRESTClient.AcceptEncoding := 'identity';
  fRESTClient.BaseURL := fServerAddress;
  fRESTClient.ContentType := 'application/x-www-form-urlencoded';
  fRESTClient.HandleRedirects := True;
  fRESTClient.UserAgent := fClientName;

  fRESTRequest := TRESTRequest.Create(nil);
  fRESTRequest.Accept := 'application/json';
  fRESTRequest.AcceptCharset := 'UTF-8';
  fRESTRequest.Client := fRESTClient;
  fRESTRequest.Timeout := 10000;
end;


destructor TKMServerAPI.Destroy;
begin
  fRESTRequest.Free;
  fRESTClient.Free;
  fRestCS.Free;

  inherited;
end;


// Queue a request and return when it's done
// aOnDone is a json 'data' object in a string
// All errors are returned through aOnFail
procedure TKMServerAPI.Request2(aMethod: TRESTRequestMethod; const aResource: string; aOnDone, aOnFail: TProc<string>);
var
  resCode: Integer;
  resText: string;
  resContent: string;
  s: string;
begin
  try
    fRestCS.Enter;
    try
      fRESTRequest.Method := aMethod;
      fRESTRequest.Resource := aResource;

      fRESTRequest.Params.Clear;

      fRESTRequest.Execute;

      resCode := fRESTRequest.Response.StatusCode;
      resText := fRESTRequest.Response.StatusText;
      resContent := fRESTRequest.Response.Content;
    finally
      fRestCS.Leave;
    end;

    if resCode = 200 then
      TThread.Queue(nil, procedure begin aOnDone(resContent); end)
    else
      raise Exception.Create(Format('Bad reply code %d - %s', [resCode, resText]));
  except
    // Threading is one place where you should swallow exceptions
    // Exception belongs to thread and can not be passed on to a main thread easily
    // Thus we have to handle them and pass out the result
    on E: Exception do
    begin
      // Seems like we need to capture a local string copy to safely pass it to outside from the thread
      s := E.Message;
      TThread.Queue(nil, procedure begin aOnFail(s); end);
    end;
  end;
end;


procedure TKMServerAPI.RequestAsync(aMethod: TRESTRequestMethod; const aResource: string; aOnDone, aOnFail: TProc<string>);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.NameThreadForDebugging('TKMKnightsTavern.RequestAsync (' + aResource + ')');
      Request2(aMethod, aResource, aOnDone, aOnFail);
    end
  ).Start;
end;


procedure TKMServerAPI.FileListGet(aOnDone, aOnFail: TProc<string>);
begin
  RequestAsync(rmGET, TKMSettings.SERVER_FILE_LIST_GET, aOnDone, aOnFail);
end;


procedure TKMServerAPI.FileGet(const aUrl: string; aStream: TStream; aOnProgress: TProc);
const
  // Read in chunks of 2mb
  BUFFER_SIZE = 2 * 1024 * 1024;
var
  hSession, hURL: HINTERNET;
  buffer: array of Byte;
  bytesRead: Cardinal;
begin
  // Works good, but has no OnProgress event
  //TDownloadURL.DownloadRawBytes(aUrl, aStream);

  Assert(aStream.Size = 0);

  SetLength(buffer, BUFFER_SIZE);

  hSession := InternetOpen(PChar(fClientName), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  try
    hURL := InternetOpenURL(hSession, PChar(aUrl), nil, 0, 0, 0);
    try
      repeat
        // Signal we've got progress (start from 0 since last loop will have bytesRead = 0 anyway)
        aOnProgress;

        InternetReadFile(hURL, @buffer[0], Length(buffer), bytesRead);
        aStream.Write(buffer[0], bytesRead);
      until bytesRead = 0;
    finally
      InternetCloseHandle(hURL);
    end;
  finally
    InternetCloseHandle(hSession);
  end;
end;


end.
