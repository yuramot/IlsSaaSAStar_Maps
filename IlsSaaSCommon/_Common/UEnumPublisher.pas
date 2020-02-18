unit UEnumPublisher;

interface

uses
  TypInfo, SysUtils, SysConst, Generics.Collections;


type

//  TTestEnum = (en1, en2, en3);

  TEnumTextInfo = record
    Name: string;
    Comment: String;
  end;

//const

//  CTestEnumInfo: array[TTestEnum] of TEnumTextInfo = (
//    (Name: '1'; Comment: '11'),
//    (Name: '2'; Comment: '22'),
//    (Name: '3'; Comment: '33')
//   );


type

  TRttiEnumIterHelp<TEnum> = class
  private
    class function EnumValue(const aValue: Integer): TEnum; static;
  protected
    class procedure DoIter(AIdx: Integer; AInfo: TEnumTextInfo); virtual; abstract;
    class procedure EnumAll(AInfoArray: array of TEnumTextInfo); static;
  public
  end;



// пример: TRttiEnumIterHelp<TTestEnum>.EnumAll(CTestEnumInfo);

  TestEnumPublishMySql<TEnum> = class(TRttiEnumIterHelp<TEnum>)
  protected
    class procedure DoIter(AIdx: Integer; AInfo: TEnumTextInfo);

  public
    class procedure Publish(AInfoArray: array of TEnumTextInfo; ATableName: string); overload; static;
  end;
//  end;
//  TEnumPublish<TEnum, CConstArray> = class
//    class procedure EnumIter<TEnum {:enum}>; static;
//    procedure
//  end;


implementation


class procedure TRttiEnumIterHelp<TEnum>.EnumAll(AInfoArray: array of TEnumTextInfo);
var
  typeInf: PTypeInfo;
  typeData: PTypeData;
  iterValue: Integer;
begin
  typeInf := PTypeInfo(TypeInfo(TEnum));
  if typeInf^.Kind <> tkEnumeration then
    raise EInvalidCast.CreateRes(@SInvalidCast);

  typeData := GetTypeData(typeInf);
  for iterValue := typeData.MinValue to typeData.MaxValue do
    DoIter(iterValue, AInfoArray[iterValue]);
end;


class function TRttiEnumIterHelp<TEnum>.EnumValue(const aValue: Integer): TEnum;
var
  typeInf: PTypeInfo;
begin
  typeInf := PTypeInfo(TypeInfo(TEnum));
  if typeInf^.Kind <> tkEnumeration then
    raise EInvalidCast.CreateRes(@SInvalidCast);

  case GetTypeData(typeInf)^.OrdType of
    otUByte, otSByte:
      PByte(@Result)^ := aValue;
    otUWord, otSWord:
      PWord(@Result)^ := aValue;
    otULong, otSLong:
      PInteger(@Result)^ := aValue;
  else
    raise EInvalidCast.CreateRes(@SInvalidCast);
  end;
end;

{ TestEnumMySqlPublish<TEnum> }

class procedure TestEnumPublishMySql<TEnum>.DoIter(AIdx: Integer; AInfo: TEnumTextInfo);
begin

end;

class procedure TestEnumPublishMySql<TEnum>.Publish(AInfoArray: array of TEnumTextInfo; ATableName: string);
begin

end;

end.
