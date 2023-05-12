unit UMain;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
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
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    function CreateInstance(AClass: TClass): TObject;

  public

  end;

var
  Form1: TForm1;

implementation

uses
  TypInfo, Rtti;

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
  LObject: TValue;
begin
  LECLBr := GetLib;
  LObject := LECLBr.CreateInstance(TMyClass, [], 'Create');
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  L: TMyClass;
begin
  L := TMyClass(CreateInstance(TMyClass));
end;

function TForm1.CreateInstance(AClass: TClass): TObject;
var
  Context: TRttiContext;
  ClassType: TRttiType;
begin
  Context := TRttiContext.Create;
  try
    ClassType := Context.GetType(AClass);
    Result := ClassType.GetMethod('Create').Invoke(ClassType.AsInstance.MetaclassType, []).AsObject;
  finally
    Context.Free;
  end;
end;

end.

