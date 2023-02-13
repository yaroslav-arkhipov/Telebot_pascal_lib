unit TeleBotAPI;

interface

uses System.Classes, Winapi.Windows, Winapi.Messages, System.SysUtils, System.JSON,
     System.Variants, Generics.Collections, IdBaseComponent, IdComponent, Vcl.Dialogs,
     IdTCPConnection, IdTCPClient, IdHTTP, IdGlobal, IdGlobalProtocols, IdCoderMIME,
     IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdMultipartFormData;

const
  cBaseUrl      = 'https://api.telegram.org/bot';
  cGetMe        = '/GetMe';
  cSendMessage  = '/sendmessage?' + 'chat_id=%s&text=%s';
  cSendLocation = '/sendLocation';
  cSendDocument = '/sendDocument';

type
  TCallbackProc = procedure(AUserID, AUserName, AUserMessage: String) of object; //procedure for notify of new messages

  TTelegramListener = class(TThread)
  strict private
    FResponse: String; // response from server
    FUserID: String; // useid who write message
    FUserName: String; // username who write message
    FUserMessage: String; // message from user
    FTelegramToken: String; //telegram token
  private
    FCallback: TCallbackProc; //procedure for notify of new messages
    procedure Status; // frocedure for processing messages
  protected
    procedure Execute; override;
  public
    constructor Create(Asyspended: Boolean);
    destructor Destroy; override;
    property Callback: TCallbackProc write FCallback;
    property TelegramToken: String write FTelegramToken;
  end;

  TTeleBot = class(TPersistent)
  strict private
    FHTTPConnection: TIdHTTP;
    FSSLSocketHandler: TIdSSLIOHandlerSocketOpenSSL;
    FJSONParser: TJSONObject;
    FMessageListener: TTelegramListener;
    FTelegramToken: String;
    FLastResponse: String;
    FLastResponseCode: Integer;
    FLastError: String;
  protected
    function GetHTTPUserAgent: String;
    function GetHTTPCharSet: String;
    procedure SetHTTPUserAgent(Value: String);
    procedure SetHTTCharSet(Value: String);
  public
    constructor Create(AToken: String);
    destructor Destroy; override;
    function CheckBot: Boolean;
    function SendMessage(const AUserID, AText: String): String;
    function SendLocation(const AUserID, ALatitude, ALongitude: String): String;
    function SendFile(const AUserID, AFileName: String): String;
    procedure StartListenMessages(CallProc: TCallbackProc);
    property LastResponse: String read FLastResponse;
    property LastResponseCode: Integer read FLastResponseCode;
    property HTTPUserAgent: String read GetHTTPUserAgent write SetHTTPUserAgent;
    property HTTPCharSet: String read GetHTTPCharSet write SetHTTCharSet;
    property LastError: String read FLastError;
  end;

implementation

function UrlEncode(const AStr: AnsiString): String;
var
  I: integer;
begin
  Result := '';
  for I := 1 to Length(AStr) do
    case AStr[I] of
      '%', ' ', '&', '=', '@', '.', #13, #10, #128..#255: Result := Result + '%' + IntToHex(Ord(AStr[I]), 2);
    else
      Result := Result + String(AStr[I]);
    end;
end;

{ TTeleBot }

function TTeleBot.CheckBot: Boolean; //check is bot active
begin
  Result := False;
  try
    FHTTPConnection.Request.ContentType := 'application/json';
    FLastResponse := FHTTPConnection.Get(cBaseUrl + FTelegramToken + cGetMe);
    FLastResponseCode := FHTTPConnection.ResponseCode;
    FJSONParser := TJSONObject.ParseJSONValue(FLastResponse, False, True) as TJSONObject;
    Result := FJSONParser.Values['ok'].Value = 'true';
  except
    on E: Exception do
    begin
      ShowMessage('Что то пошло не так, для подроностей обратитесь к свойству LastError!');
      FLastError := E.Message;
    end;
  end;
end;

constructor TTeleBot.Create(AToken: String); //put in constructor your telegram token
begin
  FLastResponseCode := 0;
  FLastError := '';
  FTelegramToken := AToken.Trim;

  FHTTPConnection := TIdHTTP.Create;
  FHTTPConnection.HTTPOptions := FHTTPConnection.HTTPOptions + [hoNoProtocolErrorException];
  FHTTPConnection.Request.BasicAuthentication := False;
  FHTTPConnection.Request.CharSet := 'utf-8';
  FHTTPConnection.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:12.0) Gecko/20100101 Firefox/12.0';

  FSSLSocketHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FSSLSocketHandler.SSLOptions.Method := sslvTLSv1_2;
  FSSLSocketHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];
  FSSLSocketHandler.SSLOptions.Mode := sslmUnassigned;
  FSSLSocketHandler.SSLOptions.VerifyMode := [];
  FSSLSocketHandler.SSLOptions.VerifyDepth := 0;

  FHTTPConnection.IOHandler := FSSLSocketHandler;

  FJSONParser := TJSONObject.Create;
end;

destructor TTeleBot.Destroy;
begin
  if Assigned(FJSONParser) then
    FreeAndNil(FJSONParser);

  if Assigned(FSSLSocketHandler) then
    FreeAndNil(FSSLSocketHandler);

  if Assigned(FHTTPConnection) then
    FreeAndNil(FHTTPConnection);

  if Assigned(FMessageListener) then
  begin
    FMessageListener.Terminate;
    FMessageListener.Free;
  end;
  inherited;
end;

function TTeleBot.GetHTTPCharSet: String;
begin
  Result := FHTTPConnection.Request.CharSet;
end;

function TTeleBot.GetHTTPUserAgent: String;
begin
  Result := FHTTPConnection.Request.UserAgent;
end;

function TTeleBot.SendFile(const AUserID, AFileName: String): String;
var
  LFormData: TIdMultipartFormDataStream;
begin
  FHTTPConnection.Request.ContentType := 'multipart/form-data';
  LFormData := TIdMultipartFormDataStream.Create;
  try
    LFormData.AddFile('document', AFileName);
    LFormData.AddFormField('chat_id', AUserID);

    try
      FLastResponse := FHTTPConnection.Post(cBaseUrl + FTelegramToken + cSendDocument, LFormData);
      FLastResponseCode := FHTTPConnection.ResponseCode;

      if FLastResponse.Trim = '' then
      begin
        Result := 'Этого пользователя нет в списке контактов!';
        Exit;
      end;

      FJSONParser := TJSONObject.ParseJSONValue(FLastResponse, False, True) as TJSONObject;
      if FJSONParser.Values['ok'].Value = 'true' then
        Result := 'Бот ' + FJSONParser.FindValue('result.from.username').Value + ' отправил файл : ' + FJSONParser.FindValue('result.text').Value;
    except
      on E: Exception do
      begin
        ShowMessage('Что то пошло не так, для подроностей обратитесь к свойству LastError!');
        FLastError := E.Message;
      end;
    end;
  finally
    LFormData.Free;
  end;
end;

function TTeleBot.SendLocation(const AUserID, ALatitude, ALongitude: String): String;
var
  LList: TStringList;
begin
  FHTTPConnection.Request.ContentType := 'application/json';
  LList := TStringList.Create;
  try
    LList.AddPair('chat_id', AUserID);
    LList.AddPair('latitude', ALatitude);
    LList.AddPair('longitude', ALongitude);

    try
      FLastResponse := FHTTPConnection.Post(cBaseUrl + FTelegramToken + cSendLocation, LList);
      FLastResponseCode := FHTTPConnection.ResponseCode;

      if FLastResponse.Trim = '' then
      begin
        Result := 'Этого пользователя нет в списке контактов!';
        Exit;
      end;

      FJSONParser := TJSONObject.ParseJSONValue(FLastResponse, False, True) as TJSONObject;
      if FJSONParser.Values['ok'].Value = 'true' then
        Result := 'Бот ' + FJSONParser.FindValue('result.from.username').Value + ' отправил геолокацию: ' + FJSONParser.FindValue('result.text').Value;
    except
      on E: Exception do
      begin
        ShowMessage('Что то пошло не так, для подроностей обратитесь к свойству LastError!');
        FLastError := E.Message;
      end;
    end;
  finally
    LList.Free;
  end;
end;

function TTeleBot.SendMessage(const AUserID, AText: String): String; //AUserID - Unique identifier for the target chat or username of the target channel (in the format @channelusername)
var
  Request: String;
  LText: String;
begin
  LText := UrlEncode(AnsiToUtf8(AText));
  Request := Format(cSendMessage, [AUserID, LText]);
  FHTTPConnection.Request.ContentType := 'application/json';

  try
    FLastResponse := FHTTPConnection.Get(cBaseUrl + FTelegramToken + Request);
    FLastResponseCode := FHTTPConnection.ResponseCode;

    if FLastResponse.Trim = '' then
    begin
      Result := 'Этого пользователя нет в списке контактов!';
      Exit;
    end;

    FJSONParser := TJSONObject.ParseJSONValue(FLastResponse, False, True) as TJSONObject;
    if FJSONParser.Values['ok'].Value = 'true' then
      Result := 'Бот ' + FJSONParser.FindValue('result.from.username').Value + ' отправил сообщение : ' + FJSONParser.FindValue('result.text').Value;
  except
    on E: Exception do
    begin
      ShowMessage('Что то пошло не так, для подроностей обратитесь к свойству LastError!');
      FLastError := E.Message;
    end;
  end;
end;

procedure TTeleBot.SetHTTCharSet(Value: String);
begin
  if Value <> FHTTPConnection.Request.CharSet then
    FHTTPConnection.Request.CharSet := Value;
end;

procedure TTeleBot.SetHTTPUserAgent(Value: String);
begin
  if Value <> FHTTPConnection.Request.UserAgent then
    FHTTPConnection.Request.UserAgent := Value;
end;

procedure TTeleBot.StartListenMessages(CallProc: TCallbackProc);
begin
  if Assigned(FMessageListener) then
  begin
    FMessageListener.Terminate;
    FMessageListener.Free;
  end;
  FMessageListener := TTelegramListener.Create(False);
  FMessageListener.Priority := tpLowest;
  FMessageListener.FreeOnTerminate := True;
  FMessageListener.Callback :=  CallProc;
  FMessageListener.TelegramToken := FTelegramToken;
end;

{ TTelegramListener }

constructor TTelegramListener.Create(Asyspended: Boolean);
begin
  FreeOnTerminate := True;
  inherited Create(Asyspended);
end;

destructor TTelegramListener.Destroy;
begin
  FCallback := nil;
  inherited;
end;

procedure TTelegramListener.Execute;
var
  LidHTTP: TIdHTTP;
  LSSLSocketHandler: TIdSSLIOHandlerSocketOpenSSL;
  Offset, PrevOffset: Integer;
  LJSONParser: TJSONObject;
  LResronseList: TStringList;
  LArrJSON: TJSONArray;
begin
  Offset := 0;
  PrevOffset := 0;
  //create a local indy http component
  LidHTTP := TIdHTTP.Create;
  LidHTTP.HTTPOptions := LidHTTP.HTTPOptions + [hoNoProtocolErrorException];
  LidHTTP.Request.BasicAuthentication := False;
  LidHTTP.Request.CharSet := 'utf-8';
  LidHTTP.Request.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64; rv:12.0) Gecko/20100101 Firefox/12.0';

  LSSLSocketHandler := TIdSSLIOHandlerSocketOpenSSL.Create(LidHTTP);
  LSSLSocketHandler.SSLOptions.Method := sslvTLSv1_2;
  LSSLSocketHandler.SSLOptions.SSLVersions := [sslvTLSv1_2];
  LSSLSocketHandler.SSLOptions.Mode := sslmUnassigned;
  LSSLSocketHandler.SSLOptions.VerifyMode := [];
  LSSLSocketHandler.SSLOptions.VerifyDepth := 0;

  LidHTTP.IOHandler := LSSLSocketHandler;

  LJSONParser := TJSONObject.Create;
  LResronseList := TStringList.Create;
  try
    while not Terminated do
    begin

      if Assigned(LidHTTP) then
      begin
        FResponse := LidHTTP.Get(cBaseUrl + FTelegramToken + '/getUpdates?offset=' + IntToStr(Offset) + '&timeout=30');
        if FResponse.Trim = '' then
          Continue;
        LArrJSON := ((TJSONObject.ParseJSONValue(FResponse) as TJSONObject).GetValue('result') as TJSONArray);

        if lArrJSON.Count <= 0 then Continue;

        LResronseList.Clear;
        for var I := 0 to LArrJSON.Count - 1 do
          LResronseList.Add(LArrJSON.Items[I].ToJSON);

        Offset := LResronseList.Count;
        if Offset > PrevOffset then
        begin
          LJSONParser := TJSONObject.ParseJSONValue(LResronseList[LResronseList.Count - 1], False, True) as TJSONObject;
          if (LJSONParser.FindValue('message.text') <> nil) and (LJSONParser.FindValue('message.text').Value.Trim <> '') then
          begin
            if LJSONParser.FindValue('message.from.id') <> nil then
              FUserID := LJSONParser.FindValue('message.from.id').Value; //Его ИД по которому можем ему написать

            if LJSONParser.FindValue('message.from.first_name') <> nil then
              FUserName := LJSONParser.FindValue('message.from.first_name').Value;

            if (LJSONParser.FindValue('message.from.first_name') <> nil) and (LJSONParser.FindValue('message.from.last_name') <> nil) then
              FUserName := LJSONParser.FindValue('message.from.first_name').Value + ' ' + LJSONParser.FindValue('message.from.last_name').Value; //Это имя написавшего боту

            if LJSONParser.FindValue('message.text') <> nil then
              FUserMessage :=  LJSONParser.FindValue('message.text').Value;  //Текст сообщения
            Synchronize(Status); // Сообщим что есть ответ
          end;
          PrevOffset := LResronseList.Count;
        end;
      end;
    end;
  finally
    FreeAndNil(LidHTTP);
    FreeAndNil(LJSONParser);
    FreeAndNil(LResronseList);
  end;
end;

procedure TTelegramListener.Status;
begin
  if Assigned(FCallback) then
    FCallback(FUserID, FUserName, FUserMessage);
end;

end.
