
{*******************************************************************************

  meProgressBar Visual Components

  Copyright (c) 2025 Evgeniy Morgunov. All Rights Reserved.
  Copyright (c) 2025 Evgeniy Morgunov. MIT License.

  Source code: https://github.com/EvgeniyMorgan/meProgressBar

*******************************************************************************}

unit meProgressBarEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, LCLType, Types, LResources;

type
  TTextPosition = (tpLeft, tpCenter, tpRight);
  TTextLayout = (tlCenter, tlTop, tlBottom);

  { TmeProgressBarEx }

  TmeProgressBarEx = class(TGraphicControl)
  private
    FBorderColor: TColor;
    FProgress: Integer;
    FMax: Integer;
    FMin: Integer;
    FProgressColor: TColor;
    FShowText: Boolean;
    FTextFormat: string;
    FTextOffset: Integer;
    FTextPosition: TTextPosition;
    FFont: TFont;
    FTextOutline: Boolean;
    FTextOutlineColor: TColor;
    procedure SetBorderColor(AValue: TColor);
    procedure SetMax(AValue: Integer);
    procedure SetMin(AValue: Integer);
    procedure SetProgress(AValue: Integer);
    procedure SetProgressColor(AValue: TColor);
    procedure SetShowText(AValue: Boolean);
    procedure SetTextFormat(AValue: string);
    procedure SetTextOffset(AValue: Integer);
    procedure SetTextPosition(AValue: TTextPosition);
    procedure SetFont(AValue: TFont);
    procedure SetTextOutline(AValue: Boolean);
    procedure SetTextOutlineColor(AValue: TColor);
    procedure FontChanged(Sender: TObject);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property Color default clBtnFace;
    property Min: Integer read FMin write SetMin default 0;
    property Max: Integer read FMax write SetMax default 100;
    property Progress: Integer read FProgress write SetProgress default 0;
    property ProgressColor: TColor read FProgressColor write SetProgressColor default clHighlight;
    property ShowText: Boolean read FShowText write SetShowText default True;
    property TextFormat: string read FTextFormat write SetTextFormat;
    property TextOffset: Integer read FTextOffset write SetTextOffset default 5;
    property TextPosition: TTextPosition read FTextPosition write SetTextPosition default tpCenter;
    property Font: TFont read FFont write SetFont;
    property TextOutline: Boolean read FTextOutline write SetTextOutline default False;
    property TextOutlineColor: TColor read FTextOutlineColor write SetTextOutlineColor default clBlack;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Morgunov', [TmeProgressBarEx]);
end;

{ TmeProgressBarEx }

constructor TmeProgressBarEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 150;
  Height := 20;
  Color := clBtnFace;
  FBorderColor := clGray;
  FProgressColor := clSkyBlue;
  FMin := 0;
  FMax := 100;
  FProgress := 0;
  FShowText := True;
  FTextFormat := 'make %d/%d (%d%%)';
  FTextOffset := 5;
  FTextPosition := tpCenter;
  FTextOutline := True;
  FTextOutlineColor := clWhite;

  FFont := TFont.Create;
  FFont.OnChange := @FontChanged;
  FFont.Color := clBlack;
end;

destructor TmeProgressBarEx.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TmeProgressBarEx.SetBorderColor(AValue: TColor);
begin
  if FBorderColor = AValue then Exit;
  FBorderColor := AValue;
  Invalidate;
end;

procedure TmeProgressBarEx.SetMax(AValue: Integer);
begin
  if FMax = AValue then Exit;
  FMax := AValue;
  if FMax < FMin then FMax := FMin;
  if FProgress > FMax then FProgress := FMax;
  Invalidate;
end;

procedure TmeProgressBarEx.SetMin(AValue: Integer);
begin
  if FMin = AValue then Exit;
  FMin := AValue;
  if FMin > FMax then FMax := FMin;
  if FProgress < FMin then FProgress := FMin;
  Invalidate;
end;

procedure TmeProgressBarEx.SetProgress(AValue: Integer);
begin
  if FProgress = AValue then Exit;
  FProgress := AValue;
  if FProgress < FMin then FProgress := FMin;
  if FProgress > FMax then FProgress := FMax;
  Invalidate;
end;

procedure TmeProgressBarEx.SetProgressColor(AValue: TColor);
begin
  if FProgressColor = AValue then Exit;
  FProgressColor := AValue;
  Invalidate;
end;

procedure TmeProgressBarEx.SetShowText(AValue: Boolean);
begin
  if FShowText = AValue then Exit;
  FShowText := AValue;
  Invalidate;
end;

procedure TmeProgressBarEx.SetTextFormat(AValue: string);
begin
  if FTextFormat = AValue then Exit;
  FTextFormat := AValue;
  Invalidate;
end;

procedure TmeProgressBarEx.SetTextOffset(AValue: Integer);
begin
  if FTextOffset = AValue then Exit;
  FTextOffset := AValue;
  Invalidate;
end;

procedure TmeProgressBarEx.SetTextPosition(AValue: TTextPosition);
begin
  if FTextPosition = AValue then Exit;
  FTextPosition := AValue;
  Invalidate;
end;

procedure TmeProgressBarEx.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TmeProgressBarEx.SetTextOutline(AValue: Boolean);
begin
  if FTextOutline = AValue then Exit;
  FTextOutline := AValue;
  Invalidate;
end;

procedure TmeProgressBarEx.SetTextOutlineColor(AValue: TColor);
begin
  if FTextOutlineColor = AValue then Exit;
  FTextOutlineColor := AValue;
  Invalidate;
end;

procedure TmeProgressBarEx.FontChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TmeProgressBarEx.Paint;
var
  ProgressWidth: Integer;
  ProgressRect, InnerRect: TRect;
  TextStr: string;
  TextSize: TSize;
  Percent: Integer;
  OldFont: TFont;
  TextX, TextY: Integer;
begin
  inherited Paint;

  // Calculate progress percentage
  if FMax = FMin then
    Percent := 100
  else
    Percent := Round((FProgress - FMin) / (FMax - FMin) * 100);

  // Draw border (1px)
  Canvas.Pen.Color := FBorderColor;
  Canvas.Brush.Color := Color;
  Canvas.Rectangle(0, 0, Width, Height);

  // Inner rectangle with 1px offset from border
  InnerRect := Rect(1, 1, Width - 1, Height - 1);

  // Fill background
  Canvas.FillRect(InnerRect);

  // Calculate progress width with proper offset
  if FMax > FMin then
    ProgressWidth := Round((FProgress - FMin) / (FMax - FMin) * (InnerRect.Right - InnerRect.Left - 2))
  else
    ProgressWidth := 0;

  // Draw progress bar with correct offset
  if ProgressWidth > 0 then
  begin
    ProgressRect := Rect(
      InnerRect.Left + 1,    // Left offset
      InnerRect.Top + 1,     // Top offset
      InnerRect.Left + 1 + ProgressWidth,
      InnerRect.Bottom - 1   // Bottom offset
    );
    Canvas.Brush.Color := FProgressColor;
    Canvas.FillRect(ProgressRect);
  end;

  // Draw text if enabled
  if FShowText then
  begin
    // Correct text formatting with proper parameter order:
    // 1. Current progress (FProgress)
    // 2. Max value (FMax)
    // 3. Percentage (Percent)
    TextStr := Format(FTextFormat, [FProgress, FMax, Percent]);

    OldFont := TFont.Create;
    try
      OldFont.Assign(Canvas.Font);
      Canvas.Font := FFont;
      Canvas.Brush.Style := bsClear; // Transparent text background

      TextSize := Canvas.TextExtent(TextStr);

      // Calculate text position
      case FTextPosition of
        tpLeft:
          begin
            TextX := InnerRect.Left + FTextOffset;
            TextY := (Height - TextSize.Height) div 2;
          end;
        tpRight:
          begin
            TextX := InnerRect.Right - TextSize.Width - FTextOffset;
            TextY := (Height - TextSize.Height) div 2;
          end;
        tpCenter:
          begin
            TextX := (Width - TextSize.Width) div 2;
            TextY := (Height - TextSize.Height) div 2;
          end;
      end;

      // Draw text outline if enabled
      if FTextOutline then
      begin
        Canvas.Font.Color := FTextOutlineColor;
        Canvas.TextOut(TextX + 1, TextY + 1, TextStr);
        Canvas.TextOut(TextX - 1, TextY + 1, TextStr);
        Canvas.TextOut(TextX + 1, TextY - 1, TextStr);
        Canvas.TextOut(TextX - 1, TextY - 1, TextStr);
      end;

      // Draw main text
      Canvas.Font.Color := FFont.Color;
      Canvas.TextOut(TextX, TextY, TextStr);

      // Restore original font
      Canvas.Font := OldFont;
    finally
      OldFont.Free;
    end;
  end;
end;

initialization
  {$I progressbar.lrs}

end.
