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

unit eclbr.sysutils;

interface

uses
  Rtti,
  SysUtils,
  Generics.Collections;

type
  TArrayString = array of string;
  TListString = TList<string>;

  TECLBr = class
  public
    class var FormatSettings: TFormatSettings;
    class function ArrayMerge<T>(const AArray1: TArray<T>;
      const AArray2: TArray<T>): TArray<T>; inline;
    class function ArrayCopy(const ASource: TArrayString; const AIndex: integer;
      const ACount: integer): TArrayString; inline;
    class function IfThen<T>(AValue: boolean; const ATrue: T; const AFalse: T): T; inline;
    class function AsList<T>(const AArray: TArray<T>): TList<T>; static;
    class function JoinStrings(const AStrings: TArrayString;
      const ASeparator: string): string; overload; static;
    class function JoinStrings(const AStrings: TListString;
      const ASeparator: string): string; overload; static;
    class function RemoveTrailingChars(const AStr: string; const AChars: TSysCharSet): string; static;
    class function Iso8601ToDateTime(const AValue: string;
      const AUseISO8601DateFormat: Boolean): TDateTime;
    class function DateTimeToIso8601(const AValue: TDateTime;
      const AUseISO8601DateFormat: Boolean): string;
  end;

implementation

{ TUtils }

class function TECLBr.ArrayCopy(const ASource: TArrayString; const AIndex,
  ACount: integer): TArrayString;
var
  LFor: integer;
begin
  SetLength(Result, ACount);
  for LFor := 0 to ACount - 1 do
    Result[LFor] := ASource[AIndex + LFor];
end;

class function TECLBr.ArrayMerge<T>(const AArray1,
  AArray2: TArray<T>): TArray<T>;
var
  LLength1: integer;
  LLength2: integer;
begin
  LLength1 := Length(AArray1);
  LLength2 := Length(AArray2);
  SetLength(Result, LLength1 + LLength2);
  if LLength1 > 0 then
    Move(AArray1[0], Result[0], LLength1 * SizeOf(T));
  if LLength2 > 0 then
    Move(AArray2[0], Result[LLength1], LLength2 * SizeOf(T));
end;

class function TECLBr.AsList<T>(const AArray: TArray<T>): TList<T>;
var
  LFor: integer;
begin
  Result := TList<T>.Create;
  for LFor := 0 to High(AArray) do
    Result.Add(AArray[LFor]);
end;

class function TECLBr.DateTimeToIso8601(const AValue: TDateTime;
  const AUseISO8601DateFormat: Boolean): string;
var
  LDatePart, LTimePart: string;
begin
  Result := '';
  if AValue = 0 then
    exit;
  if AUseISO8601DateFormat then
    LDatePart := FormatDateTime('yyyy-mm-dd', AValue)
  else
    LDatePart := DateToStr(AValue, FormatSettings);
  if Frac(AValue) = 0 then
    Result := ifThen(AUseISO8601DateFormat, LDatePart, TimeToStr(AValue, FormatSettings))
  else
  begin
    LTimePart := FormatDateTime('hh:nn:ss', AValue);
    Result := ifThen(AUseISO8601DateFormat, LDatePart + 'T' + LTimePart, LDatePart + ' ' + LTimePart);
  end;
end;

class function TECLBr.IfThen<T>(AValue: boolean; const ATrue, AFalse: T): T;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

class function TECLBr.Iso8601ToDateTime(const AValue: string;
  const AUseISO8601DateFormat: Boolean): TDateTime;
var
  LYYYY, LMM, LDD, LHH, LMI, LSS: Cardinal;
begin
  if AUseISO8601DateFormat then
    Result := StrToDateTimeDef(AValue, 0)
  else
    Result := StrToDateTimeDef(AValue, 0, FormatSettings);

  if Length(AValue) = 19 then
  begin
    LYYYY := StrToIntDef(Copy(AValue, 1, 4), 0);
    LMM := StrToIntDef(Copy(AValue, 6, 2), 0);
    LDD := StrToIntDef(Copy(AValue, 9, 2), 0);
    LHH := StrToIntDef(Copy(AValue, 12, 2), 0);
    LMI := StrToIntDef(Copy(AValue, 15, 2), 0);
    LSS := StrToIntDef(Copy(AValue, 18, 2), 0);
    if (LYYYY <= 9999) and (LMM <= 12) and (LDD <= 31) and
       (LHH < 24) and (LMI < 60) and (LSS < 60) then
      Result := EncodeDate(LYYYY, LMM, LDD) + EncodeTime(LHH, LMI, LSS, 0);
  end;
end;

class function TECLBr.JoinStrings(const AStrings: TListString;
  const ASeparator: string): string;
var
  LFor: integer;
begin
  Result := '';
  for LFor := 0 to AStrings.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ASeparator;
    Result := Result + AStrings[LFor];
  end;
end;

class function TECLBr.RemoveTrailingChars(const AStr: string;
  const AChars: TSysCharSet): string;
var
  LLastCharIndex: integer;
begin
  LLastCharIndex := Length(AStr);
  while (LLastCharIndex > 0) and not CharInSet(AStr[LLastCharIndex], AChars) do
    Dec(LLastCharIndex);
  Result := Copy(AStr, 1, LLastCharIndex);
end;

class function TECLBr.JoinStrings(const AStrings: TArrayString;
  const ASeparator: string): string;
var
  LFor: integer;
begin
  Result := '';
  for LFor := Low(AStrings) to High(AStrings) do
  begin
    if LFor > Low(AStrings) then
      Result := Result + ASeparator;
    Result := Result + AStrings[LFor];
  end;
end;

initialization
  FormatSettings := TFormatSettings.Create('en_US');

end.
