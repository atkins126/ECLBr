{
      ECL Brasil - Essential Core Library para quem utiliza Delphi

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

unit eclbr.params;

{$I ..\eclbr.inc}

interface

uses
  Rtti;

type
  TArrayParam = array of TValue;

  // Caso de uso
  // TValueParams.Create<T>([TValue.From<TClass>(AClass),
  //                         TValue.From<TGUID>(AGuid),
  //                         TValue.From<IInterface>(nil),
  //                         TValue.From<TInjectionMode>(imSingleton)]);
  TValueParams = record
  public
    function Create<T: class>(const AParams: TArrayParam): TArray<TValue>;
  end;

implementation

{ TValueParam }

function TValueParams.Create<T>(const AParams: TArrayParam): TArray<TValue>;
var
  LFor: Integer;
begin
  SetLength(Result, Length(AParams));
  for LFor := Low(AParams) to High(AParams) do
    Result[LFor] := AParams[LFor]
end;

end.
