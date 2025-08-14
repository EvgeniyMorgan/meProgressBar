# Description of Progress Bar Components

# TmeProgressBarEx Component Description

## Overview
TmeProgressBarEx is an enhanced progress bar component for Lazarus/Delphi that extends standard progress bar functionality with advanced text display options and customizable visual styling.

## Key Features

- **Customizable Appearance**:
  - Adjustable border color, progress color, and background color
  - Configurable text display with font customization
  - Optional text outlining for better visibility

- **Flexible Text Display**:
  - Show/hide progress text
  - Customizable text format with placeholders for current value, max value, and percentage
  - Text positioning (left, center, or right)
  - Text offset control from edges

- **Precise Value Control**:
  - Configurable minimum and maximum values
  - Progress clamping to ensure values stay within valid range
  - Automatic percentage calculation

## Properties

| Property           | Description                                                                 |
|--------------------|-----------------------------------------------------------------------------|
| BorderColor        | Color of the progress bar border (default: clGray)                          |
| Color              | Background color of the progress bar (default: clBtnFace)                   |
| Min                | Minimum value of the progress range (default: 0)                            |
| Max                | Maximum value of the progress range (default: 100)                          |
| Progress           | Current progress value (automatically clamped between Min and Max)          |
| ProgressColor      | Color of the progress indicator (default: clSkyBlue)                        |
| ShowText           | Whether to display progress text (default: True)                            |
| TextFormat         | Format string for progress text (default: 'make %d/%d (%d%%)')              |
| TextOffset         | Distance of text from edge when positioned left/right (default: 5)          |
| TextPosition       | Text alignment (tpLeft, tpCenter, tpRight) (default: tpCenter)              |
| Font               | Font settings for the progress text                                         |
| TextOutline        | Whether to draw an outline around the text (default: False)                 |
| TextOutlineColor   | Color of the text outline (default: clWhite)                                |

## Usage Example

```pascal
// Create and configure a progress bar
ProgressBar := TmeProgressBarEx.Create(Self);
ProgressBar.Parent := Self;
ProgressBar.Left := 10;
ProgressBar.Top := 10;
ProgressBar.Width := 200;
ProgressBar.Height := 25;
ProgressBar.Min := 0;
ProgressBar.Max := 500;
ProgressBar.Progress := 250;
ProgressBar.TextFormat := 'Processing: %d of %d (%d%%)';
ProgressBar.ProgressColor := clGreen;
ProgressBar.ShowText := True;
```

## Installation
1. Add the unit to your project or package
2. Register the component using the provided `Register` procedure
3. The component will appear in the 'Morgunov' palette section of the IDE

## Notes
- The component automatically handles value clamping (ensuring Progress stays between Min and Max)
- Text formatting uses standard Format() syntax with three parameters: current value, max value, and percentage
- The component is lightweight and inherits from TGraphicControl, making it suitable for various container controls
