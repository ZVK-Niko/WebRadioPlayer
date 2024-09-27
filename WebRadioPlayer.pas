unit WebRadioPlayer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Bass, Winapi.WinInet;

type
  TDownloadThread = class(TThread)
  private
    FStreamURL: string;
    FMemoryStream: TMemoryStream;
    hFile: HINTERNET;
    hRequest: HINTERNET;
    TaskCancelled: Boolean;
    BufferSize: Int64;
    TotalBytesRead: Int64;
  public
    constructor Create(const StreamURL: string; BufferSize: Int64);
    procedure Execute; override;
    procedure Cancel;
    property MemoryStream: TMemoryStream read FMemoryStream;  // Zugriff auf MemoryStream
  end;

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
    procedure PlayStreamFromMemory(MemoryStream: TMemoryStream);
  public
    DownloadThread: TDownloadThread;
  end;

var
  Form1: TForm1;
  StreamHandle: HSTREAM; // Globale Variable für den Stream-Handle

implementation

{$R *.dfm}

constructor TDownloadThread.Create(const StreamURL: string; BufferSize: Int64);
begin
  inherited Create(True);  // Erstelle Thread, aber starte ihn nicht sofort
  FreeOnTerminate := True; // Stelle sicher, dass der Thread nach Abschluss freigegeben wird
  FStreamURL := StreamURL;
  TaskCancelled := False;
  Self.BufferSize := BufferSize;
  FMemoryStream := TMemoryStream.Create; // Speicher im RAM initialisieren
end;

procedure TDownloadThread.Cancel;
begin
  TaskCancelled := True;
  Terminate;
end;

procedure TDownloadThread.Execute;
var
  Buffer: array[0..8191] of Byte;
  BytesRead: DWORD;
begin
  try
    // HTTP-Request für das Laden des Streams
    hFile := InternetOpen(PChar('Delphi'), INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
    if hFile = nil then
      raise Exception.Create('Fehler beim Öffnen des Internet-Handles');

    hRequest := InternetOpenUrl(hFile, PChar(FStreamURL), nil, 0, INTERNET_FLAG_RELOAD, 0);
    if hRequest = nil then
      raise Exception.Create('Fehler beim Laden der Datei');

    repeat
      if TaskCancelled then
        Exit; // Beende den Task, wenn abgebrochen

      if not InternetReadFile(hRequest, @Buffer, SizeOf(Buffer), BytesRead) then
        raise Exception.Create('Fehler beim Lesen der Daten');

      if BytesRead > 0 then
      begin
        FMemoryStream.Write(Buffer, BytesRead);
        Inc(TotalBytesRead, BytesRead);
      end;

      // Beginne mit dem Abspielen, sobald genügend Daten heruntergeladen wurden (z.B. 512 KB)
      if (TotalBytesRead >= BufferSize) and (StreamHandle = 0) then
      begin
        Synchronize(
          procedure
          begin
            Form1.PlayStreamFromMemory(FMemoryStream);
          end);
      end;

    until BytesRead = 0;

  finally
    // Beende den Internet-Handle
    if hRequest <> nil then
      InternetCloseHandle(hRequest);

    if hFile <> nil then
      InternetCloseHandle(hFile);

    // Speicher freigeben
    if Assigned(FMemoryStream) then
      FMemoryStream.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialisiere BASS mit dem Standard-Soundgerät
  if not BASS_Init(-1, 44100, 0, Handle, nil) then
  begin
    ShowMessage('Fehler beim Initialisieren von BASS!');
    Exit;
  end;

  // Füge Test-Stream-URLs in die ComboBox ein
  ComboBox1.Items.Add('http://icecast.omroep.nl/radio1-bb-mp3'); // Beispiel MP3-Stream
  ComboBox1.Items.Add('https://listen.technobase.fm/tunein-mp3');
  ComboBox1.Items.Add('https://listen.housetime.fm/tunein-mp3');
  ComboBox1.Items.Add('https://listen.hardbase.fm/tunein-mp3');

  // Setze eine Standardauswahl
  ComboBox1.ItemIndex := 0;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  StreamURL: string;
begin
  // Stoppe den aktuellen Stream, falls einer läuft
  if StreamHandle <> 0 then
    BASS_ChannelStop(StreamHandle);

  // Bereite die URL vor
  StreamURL := ComboBox1.Text;

  // Starte den Download im Thread, mit einem Puffer von 512 KB
  DownloadThread := TDownloadThread.Create(StreamURL, 512 * 1024);
  DownloadThread.Start;  // Starte den Thread
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  // Stoppe den Stream
  if StreamHandle <> 0 then
  begin
    BASS_ChannelStop(StreamHandle); // Stream anhalten
    BASS_StreamFree(StreamHandle);  // Ressourcen freigeben
    StreamHandle := 0;              // Handle zurücksetzen
  end;
end;

procedure TForm1.PlayStreamFromMemory(MemoryStream: TMemoryStream);
var
  errorCode: Integer;
begin
  // Prüfe, ob MemoryStream korrekt initialisiert wurde
  if not Assigned(MemoryStream) then
  begin
    ShowMessage('MemoryStream ist nicht initialisiert.');
    Exit;
  end;

  // Setze die Position des Streams zurück und spiele ihn ab
  MemoryStream.Position := 0;
  StreamHandle := BASS_StreamCreateFile(True, MemoryStream.Memory, 0, MemoryStream.Size, BASS_STREAM_BLOCK);

  if StreamHandle = 0 then
  begin
    errorCode := BASS_ErrorGetCode();
    ShowMessage('Fehler beim Abspielen des Streams! Fehlercode: ' + IntToStr(errorCode));
  end
  else
  begin
    BASS_ChannelPlay(StreamHandle, False);
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Beende den Download-Thread, falls er läuft
  if Assigned(DownloadThread) then
  begin
    DownloadThread.Cancel;
    DownloadThread.WaitFor;  // Warte, bis der Thread beendet ist
  end;

  // Stoppe alle aktiven Streams
  if StreamHandle <> 0 then
  begin
    BASS_ChannelStop(StreamHandle);
    BASS_StreamFree(StreamHandle);
    StreamHandle := 0;
  end;

  BASS_Free; // Free BASS resources
end;

end.

