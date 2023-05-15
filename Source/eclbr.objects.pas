{
             ECL Brasil - Essential Core Library for Delphi

                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Vers�o 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos � permitido copiar e distribuir c�pias deste documento de
       licen�a, mas mud�-lo n�o � permitido.

       Esta vers�o da GNU Lesser General Public License incorpora
       os termos e condi��es da vers�o 3 da GNU General Public License
       Licen�a, complementado pelas permiss�es adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{
  @abstract(ECLBr Library)
  @created(23 Abr 2023)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
}

unit eclbr.objects;

{$I ..\eclbr.inc}

interface

uses
  Rtti,
  eclbr.interfaces;

type
  TObjectFactory = class sealed(TInterfacedObject, IECLBr)
  protected
    function _FactoryInternal(AClass: TClass; AArgs: TArray<TValue>;
      AMethodName: string): TObject;
  public
    function CreateInstance(AClass: TClass; AArgs: TArray<TValue> = nil;
      AMethodName: string = 'Create'): TObject;
    class function New: IECLBr;
  end;

implementation

function TObjectFactory.CreateInstance(AClass: TClass; AArgs: TArray<TValue>;
  AMethodName: string): TObject;
begin
  Result := _FactoryInternal(AClass, AArgs, AMethodName);
end;

class function TObjectFactory.New: IECLBr;
begin
  Result := Self.Create;
end;

function TObjectFactory._FactoryInternal(AClass: TClass; AArgs: TArray<TValue>;
  AMethodName: string): TObject;
var
  LContext: TRttiContext;
  LTypeService: TRttiType;
  LConstructorMethod: TRttiMethod;
  LInstance: TValue;
begin
  Result := nil;
  LContext := TRttiContext.Create;
  try
    LTypeService := LContext.GetType(AClass);
    LConstructorMethod := LTypeService.GetMethod(AMethodName);
    if LConstructorMethod.IsConstructor then
    begin
      LInstance := LConstructorMethod.Invoke
        (LTypeService.AsInstance.MetaClassType, AArgs);
      Result := LInstance.AsObject;
    end;
  finally
    LContext.Free;
  end;
end;

end.
