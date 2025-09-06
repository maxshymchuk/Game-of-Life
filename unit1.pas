unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Forms, Graphics, ExtCtrls, Messages, Classes, SysUtils, Controls, StdCtrls, Dialogs, Strutils, LCLType;

type

  { TLifeForm }

  TLifeForm = class(TForm)
    SPanel_Edit_FieldSizeLabel: TLabel;
    SPanel_Edit_SizeLabel: TLabel;
    SPanel_Edit_AcceptButton: TButton;
    SPanel_Edit_SizeEdit: TEdit;
    SPanel_Edit_BorderToggle: TToggleBox;
    SPanel_Edit_BornEdit: TEdit;
    SPanel_Edit_StayEdit: TEdit;
    SPanel_Text_Label: TLabel;
    SPanel_Edit_BornLabel: TLabel;
    SPanel_Edit_StayLabel: TLabel;
    OpenDialog: TOpenDialog;
    SPanel: TPanel;
    SPanel_Text: TPanel;
    SPanel_Edit: TPanel;
    SaveDialog: TSaveDialog;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SPanel_Edit_ClickActions(Sender: TObject);
    procedure SPanel_Edit_BornEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TimerTimer(Sender: TObject);
  private
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  public
    procedure MouseDraw(X, Y: longint);
    procedure ShowInfo;
    procedure DrawField;
    procedure UpdateField;
  end;

type
  TConfig = record
    B: string;
    S: string;
  end;

var
  LifeForm: TLifeForm;
  DrawBuffer: TBitMap;
  f, f2: array of array of byte;
  fx, fy, size: longint;
  isPlaying, isDrawing, isLeftButton, isBordered: boolean;
  config: TConfig;
  map: text;

implementation
type
  TNeighbor = 0..8;
  TNeighborSet = set of TNeighbor;

{$R *.lfm}
{$R-}

function BitsToInt(const Bits: string): QWord;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(Bits) do
    Result := (Result shl 1) or Ord(Bits[i] = '1');
end;

function IntToBits(Value: QWord; Width: Integer): string;
var
  i: Integer;
begin
  SetLength(Result, Width);
  for i := Width downto 1 do
  begin
    Result[i] := Char(Ord('0') + (Value and 1));
    Value := Value shr 1;
  end;
end;

procedure TLifeForm.ShowInfo;
begin
  LifeForm.Caption := Format(
    'Game of Life :: [B%s/S%s] :: %d ms/step :: %s',
    [config.B, config.S, Timer.Interval, IfThen(isBordered, 'Bordered', 'Looped')]
  );
end;

procedure Operate;
var
  x, y, n: Integer;
  BSet, SSet: TNeighborSet;
  tmp: array of array of Byte;

  function MakeSet(const S: string): TNeighborSet;
  var i: Integer; ch: Char; d: TNeighbor;
  begin
    Result := [];
    for i := 1 to Length(S) do
    begin
      ch := S[i];
      if (ch >= '0') and (ch <= '8') then
      begin
        d := TNeighbor(Ord(ch) - Ord('0'));
        Include(Result, d);
      end;
    end;
  end;

  function CountNeighbors(const yy, xx: Integer): Integer;
  var i, j, ny, nx: Integer;
  begin
    Result := 0;
    for i := yy - 1 to yy + 1 do
      for j := xx - 1 to xx + 1 do
      begin
        if (i = yy) and (j = xx) then Continue;
        if isBordered then
        begin
          if (i >= 0) and (i < fy) and (j >= 0) and (j < fx) then
            Inc(Result, f[i, j]);
        end
        else
        begin
          ny := (i + fy) mod fy;
          nx := (j + fx) mod fx;
          Inc(Result, f[ny, nx]);
        end;
      end;
  end;

begin
  BSet := MakeSet(config.B);
  SSet := MakeSet(config.S);

  for y := 0 to fy - 1 do
    for x := 0 to fx - 1 do
    begin
      n := CountNeighbors(y, x);
      if f[y, x] = 1 then
        f2[y, x] := Ord(TNeighbor(n) in SSet)
      else
        f2[y, x] := Ord(TNeighbor(n) in BSet);
    end;

  tmp := f;
  f := f2;
  f2 := tmp;
end;

procedure TLifeForm.TimerTimer(Sender: TObject);
begin
  Operate;
  DrawField;
  ShowInfo;
end;

procedure TLifeForm.UpdateField;
begin
  DrawBuffer.Width := LifeForm.Width;
  DrawBuffer.Height := LifeForm.Height;
  fx := trunc(LifeForm.Width / size);
  fy := trunc(LifeForm.Height / size);
  SetLength(f, fy, fx);
  SetLength(f2, fy, fx);
  SPanel_Edit_FieldSizeLabel.Caption := Format(
    '[%s, %s]',
    [IntToStr(LifeForm.Width), IntToStr(LifeForm.Height)]
  );
end;

procedure TLifeForm.DrawField;
var
  i, j: word;
begin
  DrawBuffer.Canvas.Brush.Color := clBlack;
  DrawBuffer.Canvas.FillRect(0, 0, DrawBuffer.Width, DrawBuffer.Height);
  DrawBuffer.Canvas.Brush.Color := clWhite;
  for i := 0 to fy - 1 do
    for j := 0 to fx - 1 do
      if f[i, j] = 1 then DrawBuffer.Canvas.FillRect(j * size, i * size, (j + 1) * size, (i + 1) * size);
  Canvas.Draw(0, 0, DrawBuffer);
end;

procedure ClearField;
var
  i, j: word;
begin
  for i := 0 to fy - 1 do
    for j := 0 to fx - 1 do begin
      f[i, j] := 0;
      f2[i, j] := 0;
    end;
end;

procedure RandomFill;
var
  i, j: word;
begin
  for i := 0 to fy - 1 do
    for j := 0 to fx - 1 do begin
      f[i, j] := Random(2);
      f2[i, j] := 0;
    end;
end;

procedure TLifeForm.MouseDraw(X, Y: longint);
var
  i, j: longint;
  function CheckCoords(n, t: longint): longint;
  begin
    if n < 0 then
      Result := 0
    else if n > (t - 1) * size then
      Result := (t - 1) * size
    else Result := n;
  end;
begin
  i := trunc(CheckCoords(Y, fy) / size);
  j := trunc(CheckCoords(X, fx) / size);
  if isLeftButton then begin
    f[i, j] := 1;
    DrawBuffer.Canvas.Brush.Color := clWhite;
  end else begin
    f[i, j] := 0;
    DrawBuffer.Canvas.Brush.Color := clBlack;
  end;
  DrawBuffer.Canvas.FillRect(j * size, i * size, (j + 1) * size, (i + 1) * size);
  Canvas.Draw(0, 0, DrawBuffer);
end;

procedure TLifeForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  isDrawing := true;
  case Button of
    mbLeft: isLeftButton := true; // MLB - DRAW
    mbRight: isLeftButton := false; // MRB - ERASE
    mbMiddle: begin isDrawing := false; Operate; end; // MMB - ONE STEP
  end;
  if isDrawing then MouseDraw(X, Y);
  DrawField;
  ShowInfo;
end;

procedure TLifeForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if isDrawing then MouseDraw(X, Y);
end;

procedure TLifeForm.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  isDrawing := false;
end;

procedure TLifeForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  const DIVIDER = 32;

  procedure SaveMap;
  var
    y, x, k, steps, trunk: Integer;
    chunkBits: string;
    chunkVal: QWord;
  begin
    if not SaveDialog.Execute then Exit;

    System.Assign(map, SaveDialog.FileName);
    System.Rewrite(map);

    steps := fx div DIVIDER;
    trunk := fx mod DIVIDER;

    Write(map, LifeForm.Width, ',', LifeForm.Height, ',', size, ';');

    for y := 0 to fy - 1 do
    begin
      for k := 0 to steps - 1 do
      begin
        chunkBits := '';
        for x := k * DIVIDER to k * DIVIDER + DIVIDER - 1 do
          chunkBits := chunkBits + Char(Ord('0') + f[y, x]);
        chunkVal := BitsToInt(chunkBits);
        Write(map, chunkVal, ',');
      end;

      if trunk > 0 then
      begin
        chunkBits := '';
        for x := steps * DIVIDER to fx - 1 do
          chunkBits := chunkBits + Char(Ord('0') + f[y, x]);
        chunkVal := BitsToInt(chunkBits);
        Write(map, chunkVal);
      end;

      Write(map, ';');
    end;

    System.Close(map);
  end;

  procedure OpenMap;
  var
    y, i, bitIdx, expectedBits, col, trunk: Integer;
    line, header: string;
    lines, params: TStringArray;
    val: QWord;
    bits: string;
    w, h, sz: Integer;
  begin
    if not OpenDialog.Execute then Exit;

    System.Assign(map, OpenDialog.FileName);
    System.Reset(map);
    ReadLn(map, line);
    System.Close(map);

    lines := line.Split(';');
    if Length(lines) = 0 then Exit;

    header := lines[0];
    params := header.Split(',');
    w  := StrToIntDef(params[0], LifeForm.Width);
    h  := StrToIntDef(params[1], LifeForm.Height);
    sz := StrToIntDef(params[2], size);

    LifeForm.Width  := w;
    LifeForm.Height := h;
    size := sz;

    UpdateField;
    ClearField;

    trunk := fx mod DIVIDER;

    for y := 0 to fy - 1 do
    begin
      if (y + 1) >= Length(lines) then Break;
      params := lines[y + 1].Split(',');
      col := 0;

      for i := 0 to High(params) do
      begin
        if params[i] = '' then Continue;

        val := StrToQWordDef(params[i], 0);
        expectedBits := DIVIDER;
        if (i = High(params)) and (trunk <> 0) then
          expectedBits := trunk;

        bits := IntToBits(val, expectedBits);

        for bitIdx := 1 to Length(bits) do
        begin
          if col >= fx then Break;
          f[y, col] := Ord(bits[bitIdx] = '1');
          Inc(col);
        end;
      end;
    end;

    DrawField;
    ShowInfo;
  end;

  procedure Pause;
  begin
    isPlaying := false;
    Timer.Enabled := isPlaying;
  end;

  procedure IncSpeed;
  begin
    if Timer.Interval > 20 then Timer.Interval := Timer.Interval - 20;
  end;

  procedure DecSpeed;
  begin
    if Timer.Interval < 2000 then Timer.Interval := Timer.Interval + 20;
  end;

  procedure PlayPause;
  begin
    isPlaying := not isPlaying;
    Timer.Enabled := isPlaying;
  end;

  procedure ToggleSettings;
  begin
    SPanel.Visible := not SPanel.Visible;
  end;

begin
  // Ctrl pressed
  if ssCtrl in Shift then begin
    case Key of
      VK_S: begin Pause; SaveMap; end;
      VK_O: begin Pause; OpenMap; end;
    end;
  // Shift pressed
  end else if ssShift in Shift then begin
    case Key of
      VK_OEM_PLUS: IncSpeed;
      VK_OEM_MINUS: DecSpeed;
    end;
  end else case Key of
    VK_ADD: IncSpeed;
    VK_SUBTRACT: DecSpeed;
    VK_RETURN: PlayPause;
    VK_C: ClearField;
    VK_R: RandomFill;
    VK_ESCAPE: ToggleSettings;
  end;
  DrawField;
  ShowInfo;
end;

procedure TLifeForm.SPanel_Edit_ClickActions(Sender: TObject);
var
  newSize: Integer;
begin
  if Sender = SPanel_Edit_AcceptButton then begin
    config.B := SPanel_Edit_BornEdit.Text;
    config.S := SPanel_Edit_StayEdit.Text;

    newSize := StrToIntDef(SPanel_Edit_SizeEdit.Text, size);
    if newSize < 1 then newSize := 1;
    if newSize > 100 then newSize := 100;
    size := newSize;
    SPanel_Edit_SizeEdit.Text := IntToStr(size);

    UpdateField;
  end else if Sender = SPanel_Edit_BorderToggle then begin
    isBordered := SPanel_Edit_BorderToggle.Checked;
  end;
  ShowInfo;
end;

procedure TLifeForm.FormResize(Sender: TObject);
begin
  if (DrawBuffer <> nil) and (size <> 0) then begin
    UpdateField;
    DrawField;
  end;
end;

procedure TLifeForm.FormDestroy(Sender: TObject);
begin
  DrawBuffer.Free;
end;

procedure TLifeForm.FormCreate(Sender: TObject);
begin
  KeyPreview := True;
  DoubleBuffered := True;

  isPlaying := false;
  isDrawing := false;
  isBordered := true;

  Timer.Enabled := false;

  DrawBuffer := TBitMap.Create;
  Randomize;
  config.B := '3';
  config.S := '23';
  size := 3;

  SPanel_Edit_BornEdit.Text := config.B;
  SPanel_Edit_StayEdit.Text := config.S;
  SPanel_Edit_SizeEdit.Text := IntToStr(size);

  UpdateField;
  ClearField;
  DrawField;
  ShowInfo;
end;

procedure TLifeForm.FormPaint(Sender: TObject);
begin
  Canvas.Draw(0, 0, DrawBuffer);
end;

procedure TLifeForm.SPanel_Edit_BornEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // BACKSPACE, TAB, SHIFT, END, HOME, LEFT, RIGHT, DELETE, DIGITS
  if not (Key in [8, 9, 16, 35, 36, 37, 39, 46, 48..57]) then Key := 0;
end;

procedure TLifeForm.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

end.

