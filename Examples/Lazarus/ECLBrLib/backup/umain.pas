unit UMain;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  rtti,
  eclbr.interfaces,
  eclbr.lib.dll;

type

  { TForm1 }

  { TMyClass }

  TMyClass = class
  public
    constructor Create;
  end;


  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TMyClass }

constructor TMyClass.Create;
begin
  ShowMessage('Create');
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  LECLBr: IECLBr;
  LObject: TObject;
begin
  LECLBr := GetLib;
  LObject := LECLBr.CreateInstance(TMyClass, [], 'Create');
end;

end.

