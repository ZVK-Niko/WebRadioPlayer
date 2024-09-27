program Project1;

uses
  Vcl.Forms,
  WebRadioPlayer in 'WebRadioPlayer.pas' {Form1},
  bass in 'bass.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
