unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Forms, Graphics, ExtCtrls, Messages, Classes, SysUtils, Controls, StdCtrls, Dialogs, Strutils;

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
    procedure FormResize(Sender: TObject);
    procedure SPanel_Edit_ClickActions(Sender: TObject);
    procedure SPanel_Edit_BornEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  public
    procedure mouseDraw(X, Y: longint);
    procedure showInfo;
    procedure drawField;
    procedure updateField;
  end;

type
  TConfig = record
    B: string;
    S: string;
  end;

var
  LifeForm: TLifeForm;
  DrawBuffer: TBitMap;
  f: array of array of byte;
  fx, fy, step, size: longint;
  isPlaying, isDrawing, isLeftButton, isBordered: boolean;
  config: TConfig;
  map: text;

implementation

{$R *.lfm}
{$R-}

procedure TLifeForm.showInfo;
begin
  LifeForm.Caption := '[B' + config.B + '/S' + config.S + '] Life The Game :: Gen ' + IntToStr(step) + ' :: Speed ' + IntToStr(2020 - Timer.Interval);
  if isBordered then
    LifeForm.Caption := LifeForm.Caption + ' :: Bordered'
  else LifeForm.Caption := LifeForm.Caption + ' :: Looped';
end;

procedure operate;
var
  i, j: word;
var
  temp: array of array of byte;
  function CheckNeighbors(y, x: longint): string;
  var
    i, j, n: longint;
    function CheckCoord(n: longint; t: byte): longint;
    begin
      if t = 0 then begin
        if n < 0 then n := fy - 1;
        if n > fy - 1 then n := 0;
      end else begin
        if n < 0 then n := fx - 1;
        if n > fx - 1 then n := 0;
      end;
      Result := n;
    end;
  begin
    n := 0;
    for i := y - 1 to y + 1 do
      for j := x - 1 to x + 1 do
        if isBordered then begin
          if (i >= 0) and (i < fy) and (j >= 0) and (j < fx) then if f[i, j] = 1 then n := n + 1;
        end else begin
          if f[CheckCoord(i, 0), CheckCoord(j, 1)] = 1 then n := n + 1;
        end;
    if (f[y, x] = 1) and (n > 0) then n := n - 1;
    Result := IntToStr(n);
  end;
begin
  SetLength(temp, fy, fx);
  for i := 0 to fy - 1 do
    for j := 0 to fx - 1 do begin
      if Pos(CheckNeighbors(i, j), config.S) <> 0 then temp[i, j] := f[i, j];
      if Pos(CheckNeighbors(i, j), config.B) <> 0 then temp[i, j] := 1;
    end;
  f := temp;
  step := step + 1;
end;

procedure TLifeForm.TimerTimer(Sender: TObject);
begin
  operate;
  drawField;
  showInfo;
end;

procedure TLifeForm.updateField;
begin
  DrawBuffer.Width := LifeForm.Width;
  DrawBuffer.Height := LifeForm.Height;
  fx := trunc(LifeForm.Width / size);
  fy := trunc(LifeForm.Height / size);
  SetLength(f, fy, fx);
  SPanel_Edit_FieldSizeLabel.Caption :=
    '[' + IntToStr(LifeForm.Width) + ',' + IntToStr(LifeForm.Height) + ']';
end;

procedure TLifeForm.drawField;
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

procedure clearField;
var
  i, j: word;
begin
  step := 0;
  for i := 0 to fy - 1 do
    for j := 0 to fx - 1 do
      f[i, j] := 0;
end;

procedure randomField;
var
  i, j: word;
begin
  step := 0;
  for i := 0 to fy - 1 do
    for j := 0 to fx - 1 do
      f[i, j] := random(2);
end;

procedure TLifeForm.FormShow(Sender: TObject);
begin
  isPlaying := false;
  isDrawing := false;
  isBordered := true;
  Timer.Enabled := false;
  DrawBuffer := TBitMap.Create;
  Randomize;
  config.B := '3';
  config.S := '23';
  size := 3;
  updateField;
  clearField;
  ShowInfo;
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
    mbMiddle: begin isDrawing := false; operate; end; // MMB - ONE STEP
  end;
  if isDrawing then MouseDraw(X, Y);
  drawField;
  showInfo;
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
    i, j, k, steps, trunk: longint;
    currentBinary: string;
  begin
    if SaveDialog.Execute then begin
      System.Assign(map, SaveDialog.FileName);
      System.Rewrite(map);
      steps := length(f) div DIVIDER;
      trunk := length(f) mod DIVIDER;
      write(map, LifeForm.Width, ',', LifeForm.Height, ',', size, ';');
      for i := 0 to (length(f[0]) - 1) do begin
        for j := 1 to steps do begin
          currentBinary := '0';
          for k := (j - 1) * DIVIDER to j * DIVIDER - 1 do begin
            currentBinary := currentBinary + IntToStr(f[i, k]);
          end;
          write(map, Numb2Dec(currentBinary, 2), ',');
        end;
        currentBinary := '0';
        for j := steps * DIVIDER to steps * DIVIDER + trunk - 1 do begin
          currentBinary := currentBinary + IntToStr(f[i, j]);
        end;
        write(map, Numb2Dec(currentBinary, 2), ';');
      end;
      System.Close(map);
    end;
  end;
  procedure OpenMap;
  var
    i, j, k: longint;
    str, value: string;
    mapLines, params: TStringArray;
  begin
    if OpenDialog.Execute then begin
      System.Assign(map, OpenDialog.FileName);
      System.Reset(map);
      readln(map, str);
      System.Close(map);
      mapLines := str.Split(';');
      for i := 0 to length(mapLines) - 1 do begin
        params := mapLines[i].Split(',');
        if i = 0 then begin
           LifeForm.Width := StrToInt(params[0]);
           LifeForm.Height := StrToInt(params[1]);
           size := StrToint(params[2]);
        end else begin
           for j := 0 to length(params) - 1 do begin
             if j = length(params) - 1 then
               value := IntToBin(StrToInt(params[j]), (LifeForm.Width div size) mod DIVIDER)
             else
               value := IntToBin(StrToInt(params[j]), DIVIDER);
             for k := 1 to length(value) do
               f[i - 1, j * DIVIDER + k - 1] := StrToInt(value[k]);
           end;
        end;
      end;
    end;
  end;
  procedure Pause;
  begin
    isPlaying := false;
    Timer.Enabled := isPlaying;
  end;
begin
  if ssCtrl in Shift then begin
    case Key of
      $53: begin Pause; SaveMap; end; // S - SAVE MAP
      $4F: begin Pause; OpenMap; end; // O - OPEN MAP
    end;
  end else case Key of
    $6B: if Timer.Interval > 20 then Timer.Interval := Timer.Interval - 20; // + - INC SPEED
    $6D: if Timer.Interval < 2000 then Timer.Interval := Timer.Interval + 20; // - - DEC SPEED
    $0D: isPlaying := not isPlaying; // ENTER - PLAY/PAUSE
    $43: clearField; // C - CLEAR FIELD
    $52: randomField; // R - RANDOM FIELD
    $1B: SPanel.Visible := not SPanel.Visible; // ESC - SHOW/HIDE SETTINGS
  end;
  Timer.Enabled := isPlaying;
  drawField;
  showInfo;
end;

procedure TLifeForm.SPanel_Edit_ClickActions(Sender: TObject);
begin
  if Sender = SPanel_Edit_AcceptButton then begin
    config.B := SPanel_Edit_BornEdit.Text;
    config.S := SPanel_Edit_StayEdit.Text;
    if SPanel_Edit_SizeEdit.Text < '1' then SPanel_Edit_SizeEdit.Text := '1';
    size := StrToInt(SPanel_Edit_SizeEdit.Text);
    updateField;
  end;
  if Sender = SPanel_Edit_BorderToggle then isBordered := SPanel_Edit_BorderToggle.Checked;
  showInfo;
end;

procedure TLifeForm.FormResize(Sender: TObject);
begin
  if (DrawBuffer <> nil) and (size <> 0) then updateField;
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

