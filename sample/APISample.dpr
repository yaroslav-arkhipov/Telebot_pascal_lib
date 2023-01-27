program APISample;

uses
  Vcl.Forms,
  fMain in 'fMain.pas' {Form12},
  TeleBotAPI in '..\source\TeleBotAPI.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm12, Form12);
  Application.Run;
end.
