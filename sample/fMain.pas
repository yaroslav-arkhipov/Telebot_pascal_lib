unit fMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, TeleBotAPI, Vcl.ExtCtrls;

type
  TForm12 = class(TForm)
    Memo1: TMemo;
    btnBotCheck: TButton;
    btnSendMessage: TButton;
    btnGetMessages: TButton;
    edtMsg: TEdit;
    btnSendLocation: TButton;
    btnSendFile: TButton;
    btnSendPhoto: TButton;
    btnSendVideo: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBotCheckClick(Sender: TObject);
    procedure btnSendMessageClick(Sender: TObject);
    procedure btnGetMessagesClick(Sender: TObject);
    procedure btnSendLocationClick(Sender: TObject);
    procedure btnSendFileClick(Sender: TObject);
    procedure btnSendPhotoClick(Sender: TObject);
    procedure btnSendVideoClick(Sender: TObject);
  private
    FBot: TTeleBot;
    procedure CallBack(AUserID, AUserName, AUserMessage: String);
  private const
    cToken = '5817346447:AAF_5gkaMscqRanNZbDMJuVHpXvf0A3Qsis';
  public
    { Public declarations }
  end;

var
  Form12: TForm12;

implementation

{$R *.dfm}

procedure TForm12.btnBotCheckClick(Sender: TObject);
begin
  Memo1.Clear;
  if FBot.CheckBot then
  begin
    Memo1.Text := 'Bot is works!';
    Memo1.Lines.Create.Add(FBot.LastResponse);
  end;
end;

procedure TForm12.btnGetMessagesClick(Sender: TObject);
begin
  FBot.StartListenMessages(CallBack);
end;

procedure TForm12.btnSendFileClick(Sender: TObject);
var
  LResponse: String;
begin
  LResponse := FBot.SendFile('1949936118', 'H:\Local-Rep\Admin_f7\Resources\48\Info.bmp');
  Memo1.Lines.Add(LResponse);
end;

procedure TForm12.btnSendLocationClick(Sender: TObject);
var
  LResponse: String;
begin
  LResponse := FBot.SendLocation('1949936118', '2.4075', '48.8323');
  Memo1.Lines.Add(LResponse);
end;

procedure TForm12.btnSendMessageClick(Sender: TObject);
var
  LResponse: String;
begin
  LResponse := FBot.SendMessage('5008316716', edtMsg.Text);
  Memo1.Lines.Add(LResponse);
end;

procedure TForm12.btnSendPhotoClick(Sender: TObject);
var
  LResponse: String;
begin
  LResponse := FBot.SendPhoto('1949936118', '\\sto-s\folder_redirection\VDI_Development_Delphi\yaarhipov\Downloads\20220429_170910.jpg', 'Hi');
  Memo1.Lines.Add(LResponse);
end;

procedure TForm12.btnSendVideoClick(Sender: TObject);
var
  LResponse: String;
begin
  LResponse := FBot.SendVideo('1949936118', 'H:\Local-Rep\ASC_Common\VideoInstructions\MapRecord от Игоря.mp4', 'Hi');
  Memo1.Lines.Add(LResponse);
end;

procedure TForm12.CallBack(AUserID, AUserName, AUserMessage: String);
begin
  Memo1.Lines.Add(AUserName + ': ' + AUserMessage);
end;

procedure TForm12.FormCreate(Sender: TObject);
begin
  FBot := TTeleBot.Create(cToken);
end;

procedure TForm12.FormDestroy(Sender: TObject);
begin
  FBot.Free;
end;

end.
