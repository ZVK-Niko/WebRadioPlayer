unit WebRadioPlayer;

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Bass, ShellAPI;
type
  TForm1 = class(TForm)
    ComboBox1: TComboBox;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    StreamHandle: HSTREAM;        // BASS Stream Handle
    MemoryStream: TMemoryStream;  // Speicherstream für den Stream
    procedure PlayStreamFromMemory;
    procedure RunCurlToMemory(const URL: string);
  public
  end;
var
  Form1: TForm1;
implementation
{$R *.dfm}
procedure TForm1.FormCreate(Sender: TObject);
begin
  // BASS initialisieren
  if not BASS_Init(-1, 44100, 0, Handle, nil) then
  begin
    ShowMessage('Fehler beim Initialisieren von BASS.');
    Exit;
  end;
  // Test-Stream-URLs in die ComboBox einfügen
  ComboBox1.Items.Add('http://icecast.omroep.nl/radio1-bb-mp3');
  ComboBox1.Items.Add('https://listen.technobase.fm/tunein-mp3');
  ComboBox1.Items.Add('https://listen.housetime.fm/tunein-mp3');
  ComboBox1.Items.Add('https://listen.hardbase.fm/tunein-mp3');
  ComboBox1.ItemIndex := 0;  // Standardauswahl setzen
  // Speicherstream für den Stream initialisieren
  MemoryStream := TMemoryStream.Create;
end;
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Ressourcen freigeben
  if StreamHandle <> 0 then
  begin
    BASS_ChannelStop(StreamHandle);
    BASS_StreamFree(StreamHandle);
    StreamHandle := 0;
  end;
  BASS_Free; // BASS-Ressourcen freigeben
  MemoryStream.Free; // Speicherstream freigeben
end;
procedure TForm1.Button1Click(Sender: TObject);
var
  URL: string;
begin
  // Stoppe den aktuellen Stream, falls einer läuft
  if StreamHandle <> 0 then
  begin
    BASS_ChannelStop(StreamHandle);
    BASS_StreamFree(StreamHandle);
    StreamHandle := 0;
  end;
  // Hole die URL des Streams aus der ComboBox
  URL := Trim(ComboBox1.Text);
  // Lade den Stream über curl in den Speicher
  RunCurlToMemory(URL);
end;
procedure TForm1.Button2Click(Sender: TObject);
begin
  // Stoppe den Stream
  if StreamHandle <> 0 then
  begin
    BASS_ChannelStop(StreamHandle);
    BASS_StreamFree(StreamHandle);
    StreamHandle := 0;
  end;
end;
procedure TForm1.PlayStreamFromMemory;
begin
  // Erstelle den BASS Stream aus dem Speicherstream (MemoryStream)
  StreamHandle := BASS_StreamCreateFile(True, MemoryStream.Memory, 0, MemoryStream.Size, BASS_STREAM_BLOCK);
  if StreamHandle = 0 then
  begin
    ShowMessage('Fehler beim Abspielen des Streams. Fehlercode: ' + IntToStr(BASS_ErrorGetCode));
  end
  else
  begin
    BASS_ChannelPlay(StreamHandle, False); // Spiele den Stream ab
  end;
end;
procedure TForm1.RunCurlToMemory(const URL: string);
var
  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
  ReadPipe, WritePipe: THandle;
  SecurityAttributes: TSecurityAttributes;
  Buffer: array[0..4095] of Byte;
  BytesRead: DWORD;
begin
  // Sicherheitseinstellungen für die Pipe
  SecurityAttributes.nLength := SizeOf(TSecurityAttributes);
  SecurityAttributes.bInheritHandle := True;
  SecurityAttributes.lpSecurityDescriptor := nil;
  // Erstelle die Pipes
  if not CreatePipe(ReadPipe, WritePipe, @SecurityAttributes, 0) then
  begin
    ShowMessage('Fehler beim Erstellen der Pipe');
    Exit;
  end;
  // Setze die Startinfo für den Prozess
  ZeroMemory(@StartupInfo, SizeOf(StartupInfo));
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.hStdOutput := WritePipe;
  StartupInfo.hStdError := WritePipe;
  StartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_HIDE;
  // Erstelle den curl-Prozess
  if not CreateProcess(nil, PChar('curl.exe ' + URL), nil, nil, True, 0, nil, nil, StartupInfo, ProcessInfo) then
  begin
    ShowMessage('Fehler beim Starten von curl');
    Exit;
  end;
  // Schließe das Schreibende der Pipe
  CloseHandle(WritePipe);
  // Lese die Ausgabe von curl in den Speicherstream (MemoryStream)
  MemoryStream.Clear;
  while ReadFile(ReadPipe, Buffer, SizeOf(Buffer), BytesRead, nil) and (BytesRead > 0) do
  begin
    MemoryStream.Write(Buffer, BytesRead);
  end;
  // Schließe die Handles
  CloseHandle(ReadPipe);
  CloseHandle(ProcessInfo.hProcess);
  CloseHandle(ProcessInfo.hThread);
  // Setze den Stream auf den Anfang
  MemoryStream.Position := 0;
  // Spiele den Stream aus dem Speicher ab
  PlayStreamFromMemory;
end;
end.

