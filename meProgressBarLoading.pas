{*******************************************************************************

  meProgressBarLoading Visual Components

  Copyright (c) 2025 Evgeniy Morgunov. All Rights Reserved.
  Copyright (c) 2025 Evgeniy Morgunov. MIT License.

  Source code: https://github.com/EvgeniyMorgan/meProgressBar

*******************************************************************************}

unit meProgressBarLoading;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, LCLType, Types, Math, LResources;

type
  TTextLayout = (tlCenter, tlTop, tlBottom);

  { TmeProgressBarLoading }

  TmeProgressBarLoading = class(TGraphicControl)
  private
    FBorderColor: TColor;
    FProgress: Integer;
    FMax: Integer;
    FMin: Integer;
    FProgressColor: TColor;
    FShowText: Boolean;
    FTextFormat: string;
    FTextLeft: string;
    FTextOffset: Integer;
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
    procedure SetTextLeft(AValue: string);
    procedure SetTextOffset(AValue: Integer);
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
    property TextLeft: string read FTextLeft write SetTextLeft;
    property TextOffset: Integer read FTextOffset write SetTextOffset default 5;
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
  RegisterComponents('Morgunov', [TmeProgressBarLoading]);
end;

{ TmeProgressBarLoading }

constructor TmeProgressBarLoading.Create(AOwner: TComponent);
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
  FTextFormat := '%d%%';
  FTextLeft := '';
  FTextOffset := 5;
  FTextOutline := True;
  FTextOutlineColor := clWhite;

  FFont := TFont.Create;
  FFont.OnChange := @FontChanged;
  FFont.Color := clBlack;
end;

destructor TmeProgressBarLoading.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TmeProgressBarLoading.SetBorderColor(AValue: TColor);
begin
  if FBorderColor = AValue then Exit;
  FBorderColor := AValue;
  Invalidate;
end;

procedure TmeProgressBarLoading.SetMax(AValue: Integer);
begin
  if FMax = AValue then Exit;
  FMax := AValue;
  if FMax < FMin then FMax := FMin;
  if FProgress > FMax then FProgress := FMax;
  Invalidate;
end;

procedure TmeProgressBarLoading.SetMin(AValue: Integer);
begin
  if FMin = AValue then Exit;
  FMin := AValue;
  if FMin > FMax then FMax := FMin;
  if FProgress < FMin then FProgress := FMin;
  Invalidate;
end;

procedure TmeProgressBarLoading.SetProgress(AValue: Integer);
begin
  if FProgress = AValue then Exit;
  FProgress := AValue;
  if FProgress < FMin then FProgress := FMin;
  if FProgress > FMax then FProgress := FMax;
  Invalidate;
end;

procedure TmeProgressBarLoading.SetProgressColor(AValue: TColor);
begin
  if FProgressColor = AValue then Exit;
  FProgressColor := AValue;
  Invalidate;
end;

procedure TmeProgressBarLoading.SetShowText(AValue: Boolean);
begin
  if FShowText = AValue then Exit;
  FShowText := AValue;
  Invalidate;
end;

procedure TmeProgressBarLoading.SetTextFormat(AValue: string);
begin
  if FTextFormat = AValue then Exit;
  FTextFormat := AValue;
  Invalidate;
end;

procedure TmeProgressBarLoading.SetTextLeft(AValue: string);
begin
  if FTextLeft = AValue then Exit;
  FTextLeft := AValue;
  Invalidate;
end;

procedure TmeProgressBarLoading.SetTextOffset(AValue: Integer);
begin
  if FTextOffset = AValue then Exit;
  FTextOffset := AValue;
  Invalidate;
end;

procedure TmeProgressBarLoading.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TmeProgressBarLoading.SetTextOutline(AValue: Boolean);
begin
  if FTextOutline = AValue then Exit;
  FTextOutline := AValue;
  Invalidate;
end;

procedure TmeProgressBarLoading.SetTextOutlineColor(AValue: TColor);
begin
  if FTextOutlineColor = AValue then Exit;
  FTextOutlineColor := AValue;
  Invalidate;
end;

procedure TmeProgressBarLoading.FontChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TmeProgressBarLoading.Paint;
var
  ProgressWidth: Integer;
  ProgressRect, InnerRect: TRect;
  LeftText, ProgressText: string;
  LeftTextSize, ProgressTextSize: TSize;
  Percent: Integer;
  OldFont: TFont;
  LeftTextX, ProgressTextX, TextY: Integer;
  TextFitsInProgress: Boolean;
  SpaceWidth: Integer;
  CombinedWidth: Integer;
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
      InnerRect.Left + 1,
      InnerRect.Top + 1,
      InnerRect.Left + 1 + ProgressWidth,
      InnerRect.Bottom - 1
    );
    Canvas.Brush.Color := FProgressColor;
    Canvas.FillRect(ProgressRect);
  end;

  // Draw text if enabled
  if FShowText then
  begin
    OldFont := TFont.Create;
    try
      OldFont.Assign(Canvas.Font);
      Canvas.Font := FFont;
      Canvas.Brush.Style := bsClear;

      // Prepare texts
      LeftText := FTextLeft;
      ProgressText := Format(FTextFormat, [FProgress, FMax, Percent]);

      // Measure texts
      LeftTextSize := Canvas.TextExtent(LeftText);
      ProgressTextSize := Canvas.TextExtent(ProgressText);
      SpaceWidth := Canvas.TextExtent(' ').cx;

      // Calculate combined width
      if LeftText <> '' then
        CombinedWidth := LeftTextSize.Width + SpaceWidth + ProgressTextSize.Width
      else
        CombinedWidth := ProgressTextSize.Width;

      // Check if combined text fits in progress bar (with offsets)
      TextFitsInProgress := (ProgressWidth >= CombinedWidth + FTextOffset * 2);

      // Calculate Y position (centered vertically)
      TextY := (Height - Math.Max(LeftTextSize.Height, ProgressTextSize.Height)) div 2;

      // Position for left text (always left with offset)
      LeftTextX := InnerRect.Left + FTextOffset;

      // Position for progress text
      if TextFitsInProgress then
      begin
        // Fits - place progress text at right edge of progress with offset
        ProgressTextX := ProgressRect.Right - ProgressTextSize.Width - FTextOffset;
      end
      else
      begin
        // Doesn't fit - place progress text right after left text
        if LeftText <> '' then
          ProgressTextX := LeftTextX + LeftTextSize.Width + SpaceWidth
        else
          ProgressTextX := LeftTextX;
      end;

      // Draw left text if not empty
      if LeftText <> '' then
      begin
        // Draw text outline for left text if enabled
        if FTextOutline then
        begin
          Canvas.Font.Color := FTextOutlineColor;
          Canvas.TextOut(LeftTextX + 1, TextY + 1, LeftText);
          Canvas.TextOut(LeftTextX - 1, TextY + 1, LeftText);
          Canvas.TextOut(LeftTextX + 1, TextY - 1, LeftText);
          Canvas.TextOut(LeftTextX - 1, TextY - 1, LeftText);
        end;

        // Draw main left text
        Canvas.Font.Color := FFont.Color;
        Canvas.TextOut(LeftTextX, TextY, LeftText);
      end;

      // Draw progress text
      begin
        // Draw text outline for progress text if enabled
        if FTextOutline then
        begin
          Canvas.Font.Color := FTextOutlineColor;
          Canvas.TextOut(ProgressTextX + 1, TextY + 1, ProgressText);
          Canvas.TextOut(ProgressTextX - 1, TextY + 1, ProgressText);
          Canvas.TextOut(ProgressTextX + 1, TextY - 1, ProgressText);
          Canvas.TextOut(ProgressTextX - 1, TextY - 1, ProgressText);
        end;

        // Draw main progress text
        Canvas.Font.Color := FFont.Color;
        Canvas.TextOut(ProgressTextX, TextY, ProgressText);
      end;

      Canvas.Font := OldFont;
    finally
      OldFont.Free;
    end;
  end;
end;

initialization
  {$I progressbar.lrs}

end.
