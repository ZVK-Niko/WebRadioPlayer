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
    BytesPlayed: QWORD;  // Keep track of how much has been played
  public
    constructor Create(const StreamURL: string; BufferSize: Int64);
    procedure Execute; override;
    procedure Cancel;
    property MemoryStream: TMemoryStream read FMemoryStream;
  end;

  TForm1 = class(TForm)
    ComboBox1: TComboBox;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure PlayStreamFromMemory(MemoryStream: TMemoryStream; ResumePosition: QWORD);
  public
    DownloadThread: TDownloadThread;
  end;

var
  Form1: TForm1;
  StreamHandle: HSTREAM; // Global variable for stream handle

implementation

{$R *.dfm}

constructor TDownloadThread.Create(const StreamURL: string; BufferSize: Int64);
begin
  inherited Create(True);  // Create thread but do not start immediately
  FreeOnTerminate := True; // Ensure thread is freed after termination
  FStreamURL := StreamURL;
  TaskCancelled := False;
  Self.BufferSize := BufferSize;
  FMemoryStream := TMemoryStream.Create; // Initialize memory stream for storing audio data
  BytesPlayed := 0;  // Initialize the played bytes counter
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
  PlaybackPosition: QWORD;
begin
  try
    // HTTP request for downloading the stream
    hFile := InternetOpen(PChar('Delphi'), INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
    if hFile = nil then
      raise Exception.Create('Error opening internet handle');

    hRequest := InternetOpenUrl(hFile, PChar(FStreamURL), nil, 0, INTERNET_FLAG_RELOAD, 0);
    if hRequest = nil then
      raise Exception.Create('Error opening URL');

    repeat
      if TaskCancelled then Exit; // Exit the task if canceled

      if not InternetReadFile(hRequest, @Buffer, SizeOf(Buffer), BytesRead) then
        raise Exception.Create('Error reading data');

      if BytesRead > 0 then
      begin
        FMemoryStream.Write(Buffer, BytesRead);
        Inc(TotalBytesRead, BytesRead);
        OutputDebugString(PChar('Total bytes read: ' + IntToStr(TotalBytesRead))); // Debug log for bytes read
      end;

      // Start playing the stream once new data is available beyond the already played bytes
      if (TotalBytesRead - BytesPlayed) >= BufferSize then
      begin
        Synchronize(
          procedure
          begin
            // Get the current playback position
            if StreamHandle <> 0 then
              PlaybackPosition := BASS_ChannelGetPosition(StreamHandle, BASS_POS_BYTE)
            else
              PlaybackPosition := 0;

            OutputDebugString(PChar('Playback position: ' + IntToStr(PlaybackPosition))); // Debug log for playback position

            // Re-create the stream and continue playing from the correct position
            Form1.PlayStreamFromMemory(FMemoryStream, BytesPlayed);

            // Update the bytes played with the current total bytes read
            BytesPlayed := TotalBytesRead;
          end);
      end;

    until BytesRead = 0;

  finally
    // Clean up internet handles and memory
    if hRequest <> nil then InternetCloseHandle(hRequest);
    if hFile <> nil then InternetCloseHandle(hFile);
    if Assigned(FMemoryStream) then FMemoryStream.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialize BASS with default sound device
  if not BASS_Init(-1, 44100, 0, Handle, nil) then
  begin
    ShowMessage('Error initializing BASS!');
    Exit;
  end;

  // Add example stream URLs to the ComboBox
  ComboBox1.Items.Add('http://icecast.omroep.nl/radio1-bb-mp3'); // Example MP3 stream
  ComboBox1.Items.Add('https://listen.technobase.fm/tunein-mp3');
  ComboBox1.Items.Add('https://listen.housetime.fm/tunein-mp3');
  ComboBox1.Items.Add('https://listen.hardbase.fm/tunein-mp3');
  ComboBox1.ItemIndex := 0; // Set a default selection
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  StreamURL: string;
begin
  // Stop the current stream if playing
  if StreamHandle <> 0 then
    BASS_ChannelStop(StreamHandle);

  // Get the selected stream URL
  StreamURL := ComboBox1.Text;

  // Start the download in a new thread with a 512 KB buffer
  DownloadThread := TDownloadThread.Create(StreamURL, 512 * 1024);
  DownloadThread.Start;  // Start the download thread
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  // Stop the stream if playing
  if StreamHandle <> 0 then
  begin
    BASS_ChannelStop(StreamHandle); // Stop the stream
    BASS_StreamFree(StreamHandle);  // Free resources
    StreamHandle := 0;              // Reset the handle
  end;
end;

procedure TForm1.PlayStreamFromMemory(MemoryStream: TMemoryStream; ResumePosition: QWORD);
var
  errorCode: Integer;
begin
  // Ensure MemoryStream is valid
  if not Assigned(MemoryStream) then
  begin
    ShowMessage('MemoryStream is not initialized.');
    Exit;
  end;

  // Stop current stream, if any
  if StreamHandle <> 0 then
    BASS_ChannelStop(StreamHandle);

  // Reset MemoryStream position and play the stream
  MemoryStream.Position := 0;
  StreamHandle := BASS_StreamCreateFile(True, MemoryStream.Memory, 0, MemoryStream.Size, BASS_STREAM_BLOCK);

  if StreamHandle = 0 then
  begin
    errorCode := BASS_ErrorGetCode();
    ShowMessage('Error playing stream! Error code: ' + IntToStr(errorCode));
    Exit;
  end;

  // Resume from the last known position (if provided)
  if ResumePosition > 0 then
    BASS_ChannelSetPosition(StreamHandle, ResumePosition, BASS_POS_BYTE);

  BASS_ChannelPlay(StreamHandle, False); // Start playback
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Cancel the download thread if running
  if Assigned(DownloadThread) then
  begin
    try
      DownloadThread.Cancel; // Signal the thread to cancel

      // Wait for the thread to finish (no timeout support in Delphi)
      if not DownloadThread.Finished then
      begin
        DownloadThread.WaitFor;  // Wait for the thread to finish
      end;
    except
      on E: Exception do
      begin
        // Log and handle any exceptions
        OutputDebugString(PChar('Error waiting for thread: ' + E.Message));
      end;
    end;
  end;

  // Stop and free any active streams
  if StreamHandle <> 0 then
  begin
    BASS_ChannelStop(StreamHandle);
    BASS_StreamFree(StreamHandle);
    StreamHandle := 0;
  end;

  BASS_Free; // Free BASS resources
end;




end.
