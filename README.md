# Description of Progress Bar Components TmeProgressBarEx and TmeProgressBarLoading


<p float="left">
  <img src="/image0.png" width="33%" />
  <img src="/image1.png" width="33%" />
  <img src="/image2.png" width="33%" />
</p>

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

# TmeProgressBarLoading Component Description

## Overview
TmeProgressBarLoading is a custom progress bar control for Lazarus/Delphi applications that provides enhanced visualization of progress with flexible text display options. It extends standard progress bar functionality with customizable appearance and text rendering.

## Key Features

- **Customizable Appearance**:
  - Adjustable border and progress colors
  - Configurable minimum and maximum values
  - Smooth progress animation

- **Text Display Options**:
  - Supports left-aligned text (static label) and progress text
  - Customizable text format (supports percentage, current value, and max value)
  - Text outline effect for better visibility
  - Configurable text offset from edges
  - Automatic text positioning (inside progress or right-aligned)

- **Visual Enhancements**:
  - Proper 1px border with inner offset
  - Smooth progress filling
  - Font customization with automatic updates

## Properties

| Property            | Description                                                                 |
|---------------------|-----------------------------------------------------------------------------|
| BorderColor         | Color of the progress bar border (default: clGray)                         |
| ProgressColor       | Color of the progress indicator (default: clSkyBlue)                       |
| Min/Max             | Minimum and maximum values for the progress range (default: 0-100)         |
| Progress            | Current progress value (automatically constrained between Min and Max)     |
| ShowText            | Whether to display progress text (default: True)                           |
| TextFormat          | Format string for progress text (supports %d for value, %% for percentage)|
| TextLeft            | Optional static text to display on the left side of the progress bar       |
| TextOffset          | Pixel offset for text from edges (default: 5)                              |
| Font                | Custom font settings for all text                                          |
| TextOutline         | Whether to display text with outline (default: True)                       |
| TextOutlineColor    | Color of the text outline (default: clWhite)                               |

## Usage Example

```pascal
// Create and configure a progress bar
ProgressBar := TmeProgressBarLoading.Create(Self);
ProgressBar.Parent := Self;
ProgressBar.Align := alTop;
ProgressBar.Height := 30;
ProgressBar.Min := 0;
ProgressBar.Max := 100;
ProgressBar.Progress := 50;
ProgressBar.TextFormat := 'Loading: %d%%';
ProgressBar.TextLeft := 'Status:';
ProgressBar.ProgressColor := clGreen;
ProgressBar.BorderColor := clSilver;
```

## Text Formatting
The TextFormat property supports standard Format() syntax with these placeholders:
- `%d` - current progress value
- `%%` - percentage completed
- Additional numeric placeholders can be used for custom formatting

## Notes
- The component automatically handles text positioning, placing it inside the progress bar when space permits
- All visual changes trigger automatic repaints
- The component is registered in the 'Morgunov' palette in the IDE

This component is ideal for applications that require visually appealing progress indicators with customizable text display and smooth animation.
