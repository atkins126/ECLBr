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
  SysUtils,
  eclbr.interfaces;

type
  TObjectFactory = class sealed(TInterfacedObject, IECLBr)
  protected
    function _FactoryInternal(const AClass: TClass; const AArgs: TArray<TValue>;
      const AMethodName: string): TObject;
  public
    function CreateInstance(const AClass: TClass;
      const AArgs: TArray<TValue> = [];
      const AMethodName: string = 'Create'): TObject;
    class function New: IECLBr;
  end;

implementation

function TObjectFactory.CreateInstance(const AClass: TClass;
  const AArgs: TArray<TValue>;
  const AMethodName: string): TObject;
begin
  Result := _FactoryInternal(AClass, AArgs, AMethodName);
end;

class function TObjectFactory.New: IECLBr;
begin
  Result := Self.Create;
end;

function TObjectFactory._FactoryInternal(const AClass: TClass;
  const AArgs: TArray<TValue>;
  const AMethodName: string): TObject;
var
  LContext: TRttiContext;
  LType: TRttiType;
  LInstance: TValue;
  LConstructor: TRttiMethod;
//  LConstructor: TRttiMethod;
//  LLengthParams: integer;
//  LLengthArgs: integer;
begin
  Result := nil;
  try
    LContext := TRttiContext.Create;
    try
      LType := LContext.GetType(AClass);
      // Verifica se o m�todo construtor � o desejado
      LConstructor := LType.GetMethod(AMethodName);
      if not LConstructor.IsConstructor then
        raise Exception.CreateFmt('Constructor method %s not found in class %s', [AMethodName, AClass.ClassName]);
      LInstance := LConstructor.Invoke(LType.AsInstance.MetaClassType, AArgs);
      Result := LInstance.AsObject;

      // BUG NESSA ROTINA ABAIXO, EXECUTA COMO SE TIVESSE 2 CONSTRUTORES, REVER DEPOIS

      // Verifica se o m�todo construtor possui par�metros
//      LLengthArgs := Length(AArgs);
//      for LConstructor in LType.GetMethods do
//      begin
//        LLengthParams := Length(LConstructor.GetParameters);
//        if (not LConstructor.IsConstructor) or (LLengthParams <> LLengthArgs) then
//          Continue;
//        LInstance := LConstructor.Invoke(LType.AsInstance.MetaClassType, AArgs);
//        Result := LInstance.AsObject;
//      end;
      // Caso nenhum construtor com os par�metros desejados tenha sido encontrado
//      raise Exception.CreateFmt('Constructor with specified parameters not found in class %s', [AClass.ClassName]);
    finally
      LContext.Free;
    end;
  except
    on E: Exception do
      raise Exception.CreateFmt('Error class [%s] : %s', [AClass.ClassName, E.Message]);
  end;
end;

end.
