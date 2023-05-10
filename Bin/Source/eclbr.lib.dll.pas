{
    MCI Brasil Framework é um Motor de Cálculo de Impostos for Delphi/Lazarus

                   Copyright (c) 2019, Isaque Pinheiro
                       Todos os direitos reservado.
}

{
  @abstract(Framework)
  @created(25 Mai 2019)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @abstract(Website : https://www.isaquepinheiro.com.br/mcibr)
  @abstract(Termo Uso : https://www.isaquepinheiro.com.br/mcibr)
}

unit eclbr.lib.dll;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  eclbr.interfaces;

function GetLib: IECLBr; stdcall; external 'ECLBrLib.dll' name 'New';

implementation

end.
