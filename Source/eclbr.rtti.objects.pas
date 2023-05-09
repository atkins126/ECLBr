{
            ECL Brasil - Essential Core Library for Delphi

                   Copyright (c) 2016, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Versão 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos é permitido copiar e distribuir cópias deste documento de
       licença, mas mudá-lo não é permitido.

       Esta versão da GNU Lesser General Public License incorpora
       os termos e condições da versão 3 da GNU General Public License
       Licença, complementado pelas permissões adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{
  @abstract(ECLBr Library)
  @created(23 Abr 2023)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
}

unit eclbr.rtti.objects;

{$I ..\eclbr.inc}

interface

uses
  Rtti;

type
  IObjectFactory = interface
    ['{E3B4DFC3-25AD-46F5-947C-1509E802C047}']
    function CreateInstance(const AClass: TClass;
      const AArgs: TArray<TValue>;
      const AMethodName: string): TObject;
  end;

  TObjectFactory = class(TInterfacedObject, IObjectFactory)
  protected
    function _FactoryInternal(const AClass: TClass;
      const AArgs: TArray<TValue>;
      const AMethodName: string): TObject;
  public
    function CreateInstance(const AClass: TClass;
      const AArgs: TArray<TValue> = nil;
      const AMethodName: string = 'Create'): TObject;
  end;

implementation

function TObjectFactory.CreateInstance(const AClass: TClass;
  const AArgs: TArray<TValue>; const AMethodName: string): TObject;
begin
  Result := _FactoryInternal(AClass, AArgs, AMethodname);
end;

function TObjectFactory._FactoryInternal(const AClass: TClass;
  const AArgs: TArray<TValue>;
  const AMethodName: string): TObject;
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
      LInstance := LConstructorMethod.Invoke(LTypeService.AsInstance
                                                         .MetaClassType, AArgs);
      Result := LInstance.AsObject;
    end;
  finally
    LContext.Free;
  end;
end;

end.

