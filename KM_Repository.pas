unit KM_Repository;
interface
uses
  Classes, SysUtils, SyncObjs,
  REST.Client, IPPeerClient, REST.Types, REST.Utils,
  KM_RepositoryFileList;


type
  TKMRepository = class
  public const
    DEFAULT_SERVER_ADDRESS = 'https://release.knightsprovince.com/index.php/apps/kpautomation/api/1.0/';
  private
    fServerAddress: string;
    fClientName: string;

    fRestCS: TCriticalSection; // REST does not cope well with threaded usage
    fRESTClient: TRESTClient;
    fRESTRequestJson: TRESTRequest;
    fRESTRequestFile: TRESTRequest;

    procedure Request2(aMethod: TRESTRequestMethod; const aResource: string; aOnDone, aOnFail: TProc<string>);
    procedure RequestAsync(aMethod: TRESTRequestMethod; const aResource: string; aOnDone, aOnFail: TProc<string>);
  public
    FileList: TKMRepositoryFileList;

    constructor Create(const aServerAddress: string; const aClientName: string);
    destructor Destroy; override;

    procedure FileListGet(aOnDone: TProc; aOnFail: TProc<string>);
    procedure FileGet(aUrl: string; aStream: TMemoryStream);
  end;


implementation
uses
  JsonDataObjects, StrUtils;


{ TKMRepository }
constructor TKMRepository.Create(const aServerAddress: string; const aClientName: string);
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

  fRestRequestFile := TRESTRequest.Create(nil);
  fRestRequestFile.Accept := CONTENTTYPE_NONE;
  fRestRequestFile.Client := fRESTClient;
  fRestRequestFile.Timeout := 10000;

  FileList := TKMRepositoryFileList.Create;
end;


destructor TKMRepository.Destroy;
begin
  FreeAndNil(FileList);

  fRestRequestFile.Free;
  fRESTRequestJson.Free;
  fRESTClient.Free;
  fRestCS.Free;

  inherited;
end;


// Queue a request and return when it's done
// aOnDone is a json 'data' object in a string
// All errors are returned through aOnFail
procedure TKMRepository.Request2(aMethod: TRESTRequestMethod; const aResource: string; aOnDone, aOnFail: TProc<string>);
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


// Sometimes we need to block the main thread (e.g. AuthLogin)
procedure TKMRepository.RequestAsync(aMethod: TRESTRequestMethod; const aResource: string; aOnDone, aOnFail: TProc<string>);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.NameThreadForDebugging('TKMKnightsTavern.RequestAsync (' + aResource + ')');
      Request2(aMethod, aResource, aOnDone, aOnFail);
    end
  ).Start;
end;


procedure TKMRepository.FileListGet(aOnDone: TProc; aOnFail: TProc<string>);
begin
  RequestAsync(rmGET, 'kp_files',
    procedure (aData: string)
    begin
      FileList.LoadFromJsonString(aData);

      if Assigned(aOnDone) then aOnDone;
    end,
    procedure (aError: string)
    begin
      // We dont show error message (we could if we wanted though)
      if Assigned(aOnFail) then aOnFail(aError);
    end
  );
end;


procedure TKMRepository.FileGet(aUrl: string; aStream: TMemoryStream);
begin
  TDownloadURL.DownloadRawBytes(aUrl, aStream);
end;


end.
