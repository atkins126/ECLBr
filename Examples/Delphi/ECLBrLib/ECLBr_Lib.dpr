program ECLBr_Lib;

uses
  Vcl.Forms,
  UMain in 'UMain.pas' {Form1},
  eclbr.interfaces in '..\..\..\Source\interfaces\eclbr.interfaces.pas',
  eclbr.lib.dll in '..\..\..\Bin\Source\eclbr.lib.dll.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
