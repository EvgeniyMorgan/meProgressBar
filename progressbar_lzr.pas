{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit progressbar_lzr;

{$warn 5023 off : no warning about unused units}
interface

uses
  meProgressBarEx, meProgressBarLoading, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('meProgressBarEx', @meProgressBarEx.Register);
  RegisterUnit('meProgressBarLoading', @meProgressBarLoading.Register);
end;

initialization
  RegisterPackage('progressbar_lzr', @Register);
end.
