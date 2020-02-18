unit E_Distorter;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

uses
  SysUtils,
  T_Common;

//------------------------------------------------------------------------------
//! внести искажения
//------------------------------------------------------------------------------
procedure Distort(
  var ARData: TMyByteList
);

//------------------------------------------------------------------------------
implementation

procedure Distort(
  var ARData: TMyByteList
);
var
  //! общая длина данных
  LDataLen: Integer;
  //! длина данных коррекции
  LCorLen: Integer;
  //! позиция коррекции
  LCorPos: Integer;
begin
  LDataLen := ARData.Count;
  LCorLen  := Random( LDataLen shr 1 ) + 1;
  case ( Random( 6 ) + 1 ) * ( Random( 6 ) + 1 ) of
    2:begin // убрать
      LCorPos := Random( LDataLen - LCorLen + 1 );
      ARData.DeleteRange( LCorPos, LCorLen );
    end;
    12:begin // добавить
      LCorPos := Random( LDataLen - 1 ) + 1; // кроме самого конца и самого начала
      while ( LCorLen > 0 )
      do begin
        ARData.Insert( LCorPos, Byte( Random( 256 ) ) );
        Dec( LCorLen );
      end;
    end;
    else begin // изменить
      LCorPos := Random( LDataLen - LCorLen + 1 );
      while ( LCorLen > 0 )
      do begin
        ARData[LCorPos] := Byte( Random( 256 ) );
        Inc( LCorPos );
        Dec( LCorLen );
      end;
    end;
  end;
end;

end.

