unit KM_ServerAPI;
interface
uses
  Classes, SysUtils, SyncObjs,
  REST.Client, IPPeerClient, REST.Types, REST.Utils;


type
  TKMServerAPI = class
  public const
    DEFAULT_SERVER_ADDRESS = 'https://release.knightsprovince.com/index.php/apps/kpautomation/api/1.0/';
  private
    fServerAddress: string;
    fClientName: string;

    fRestCS: TCriticalSection; // REST does not cope well with threaded usage
    fRESTClient: TRESTClient;
    fRESTRequestJson: TRESTRequest;

    procedure Request2(aMethod: TRESTRequestMethod; const aResource: string; aOnDone, aOnFail: TProc<string>);
    procedure RequestAsync(aMethod: TRESTRequestMethod; const aResource: string; aOnDone, aOnFail: TProc<string>);
  public
    constructor Create(const aServerAddress: string; const aClientName: string);
    destructor Destroy; override;

    procedure FileListGet(aOnDone, aOnFail: TProc<string>);
    procedure FileGet(const aUrl: string; aStream: TStream);
  end;


implementation


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

  fRESTRequestJson := TRESTRequest.Create(nil);
  fRESTRequestJson.Accept := 'application/json';
  fRESTRequestJson.AcceptCharset := 'UTF-8';
  fRESTRequestJson.Client := fRESTClient;
  fRESTRequestJson.Timeout := 10000;
end;


destructor TKMServerAPI.Destroy;
begin
  fRESTRequestJson.Free;
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
      fRESTRequestJson.Method := aMethod;
      fRESTRequestJson.Resource := aResource;

      fRESTRequestJson.Params.Clear;

      fRESTRequestJson.Execute;

      resCode := fRESTRequestJson.Response.StatusCode;
      resText := fRESTRequestJson.Response.StatusText;
      resContent := fRESTRequestJson.Response.Content;
    finally
      fRestCS.Leave;
    end;

    // Convert errors to EKTException
    if resCode = 200 then
      TThread.Queue(nil, procedure begin aOnDone(resContent); end)
    else
      raise Exception.Create('Bad reply '+ IntToStr(resCode) + ' ' + resText);
  except
    // Threading is one place where you should swallow exceptions
    // Exception belongs to thread and can not be passed on to a main thread easily
    // Thus we have to handle them and pass out the result
    on E: Exception do
    begin
      // Seems like we need to capture a local string copy to safely pass it outside from the thread
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
  RequestAsync(rmGET, 'kp_files', aOnDone, aOnFail);
end;


procedure TKMServerAPI.FileGet(const aUrl: string; aStream: TStream);
begin
  TDownloadURL.DownloadRawBytes(aUrl, aStream);
end;


end.
