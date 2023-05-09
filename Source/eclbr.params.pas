{
      ECL Brasil - Essential Core Library para quem utiliza Delphi

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
