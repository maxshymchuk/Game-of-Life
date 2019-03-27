unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Forms, Graphics, ExtCtrls, Messages, Classes, SysUtils, Controls, StdCtrls, Dialogs;

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
  Timer.Enabled := isPlaying;
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
  procedure SaveMap;
  var
    i, j, currentValue, currentTimes: longint;
  begin
    if SaveDialog.Execute then begin
      System.Assign(map, SaveDialog.FileName);
      System.Rewrite(map);
      writeln(map, LifeForm.Width, ',', LifeForm.Height, ',', size);
      for i := 0 to fy - 1 do begin
        currentValue := f[i, 0];
        currentTimes := 1;
        for j := 1 to fx - 1 do begin
          if f[i, j] <> currentValue then begin
            if currentValue = 1 then
              write(map, '!', IntToStr(currentTimes))
            else
              write(map, IntToStr(currentTimes));
            if j < fx then write(map, ',');
            currentTimes := 1;
            currentValue := f[i, j];
          end else currentTimes := currentTimes + 1;
          if j = fx - 1 then begin
            if currentValue = 1 then
              write(map, '!', IntToStr(currentTimes))
            else
              write(map, IntToStr(currentTimes));
          end;
        end;
        write(map, ';');
      end;
      System.Close(map);
    end;
  end;
  procedure OpenMap;
  var
    i, j, k, position, w, h: longint;
    temp, str: string;
    params: array[0..2] of longint;
  begin
    if OpenDialog.Execute then begin
      System.Assign(map, OpenDialog.FileName);
      System.Reset(map);
      position := 0;
      readln(map, str);
      for i := 1 to length(params) do begin
        str := str[position + 1..length(str)];
        position := pos(',', str);
        if position = 0 then position := length(str) + 1;
        params[i - 1] := StrToInt(str[1..position - 1]);
      end;
      w := params[0];
      h := params[1];
      size := params[2];
      LifeForm.Width := w;
      LifeForm.Height := h;
      updateField;
      position := 0;
      k := 0;
      readln(map, str);
      System.Close(map);
      temp := '';
      for i := 1 to length(str) do begin
        if (str[i] = ',') or (str[i] = ';') then begin
          if temp[1] = '!' then begin
            for j := 0 to StrToInt(temp[2..length(temp)]) - 1 do
              f[k, position + j] := 1;
            position := position + StrToInt(temp[2..length(temp)]);
          end else begin
            for j := 0 to StrToInt(temp) - 1 do
              f[k, position + j] := 0;
            position := position + StrToInt(temp);
          end;
          temp := '';
          if str[i] = ';' then begin
            k := k + 1;
            position := 0;
          end;
          continue;
        end;
        temp := temp + str[i];
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

