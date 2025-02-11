{$mode delphi}
program test;

uses RtlLibImport;

var
  rtl: TRtl;
begin
  rtl := TRTL.Create;
  rtl.Free;
end.
