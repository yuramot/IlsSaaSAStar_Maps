﻿unit AStar64.Areas;
//------------------------------------------------------------------------------
// модуль манипуляции областями AStar
//  содержит операции сдвигов областей
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
interface

//------------------------------------------------------------------------------
const

//------------------------------------------------------------------------------
//! маскировка битов области
//------------------------------------------------------------------------------
  CAreaHashMask = Int64($0FFFFFE000000000);

//------------------------------------------------------------------------------
//! маски преобразований
//------------------------------------------------------------------------------
  CAreaShiftLongi = Int64($0000002000000000);
  CAreaShiftLati  = Int64($0000004000000000);
  CMask64Longi    = Int64($0AAAAAAAAAAAAAAA);
  CMask64Lati     = Int64($0555555555555555);
  CMask64Sig      = Int64($0FFFFFFFFFFFFFFF);

//------------------------------------------------------------------------------
//! операции сдвигов областей
//------------------------------------------------------------------------------
function AddLongi(
  const AArg: Int64
): Int64;

function AddLati(
  const AArg: Int64
): Int64;

function SubLongi(
  const AArg: Int64
): Int64;

function SubLati(
  const AArg: Int64
): Int64;

//------------------------------------------------------------------------------
implementation

function AddLongi(
  const AArg: Int64
): Int64;
begin
  Result := (AArg and CMask64Longi) * 3 + CAreaShiftLongi;
  Result := (Result and CMask64Longi) or (AArg and CMask64Lati);
end;

function AddLati(
  const AArg: Int64
): Int64;
begin
  Result := (AArg and CMask64Lati) * 3 + CAreaShiftLati;
  Result := (Result and CMask64Lati) or (AArg and CMask64Longi);
end;

function SubLongi(
  const AArg: Int64
): Int64;
begin
  Result := (AArg and CMask64Longi) * 3 - CAreaShiftLongi;
  Result := (Result and CMask64Longi) or (AArg and CMask64Lati);
end;

function SubLati(
  const AArg: Int64
): Int64;
begin
  Result := (AArg and CMask64Lati) * 3 - CAreaShiftLati;
  Result := (Result and CMask64Lati) or (AArg and CMask64Longi);
end;

end.

