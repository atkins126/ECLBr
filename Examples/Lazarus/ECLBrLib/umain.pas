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

  TMyClass = class(TPersistent)
  private
    FName: string;
    procedure SetName(AValue: string);
  published
    property Name: string read FName write SetName;
    constructor Create;
  end;


  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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

procedure TMyClass.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

constructor TMyClass.Create;
begin
  FName := 'Isaque';
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
  L: TObject;
begin
  L := CreateInstance(TMyClass);
  ShowMessage(TMyClass(L).Name);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  LECLBr: IECLBr;
begin
  LECLBr := GetLib;
  ShowMessage(LECLBr.Mensagem);
end;

function TForm1.CreateInstance(AClass: TClass): TObject;
var
  Context: TRttiContext;
  ClassType: TRttiType;
  ConstructorMethod: TRttiMethod;
begin
  Context := TRttiContext.Create;
  try
    ClassType := Context.GetType(AClass);
    ClassType.AsInstance.ClassType.Create;
//    ConstructorMethod := ClassType.GetMethod('Create');
//    Result := ConstructorMethod.Invoke(ClassType.AsInstance.MetaclassType, []).AsObject;
  finally
    Context.Free;
  end;
end;



initialization
  RegisterClass(TMyClass);

end.

