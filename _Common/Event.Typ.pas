unit Event.Typ;

interface

type

  TEventTriggerData = class
  private
    FDT: string;
    FVehicleID: Integer;
    FIMEI: Int64;
    FTripID: Integer;
    FCurrentWayPointId: Integer;
    FNextWayPointId: Integer;
    FRepeatAtTime: string;
  public
    property DT: string read FDT write FDT;
    property VehicleID: Integer read FVehicleID write FVehicleID;
    property IMEI: Int64 read FIMEI write FIMEI;
    property TripID: Integer read FTripID write FTripID;
    property CurrentWayPointId: Integer read FCurrentWayPointId write FCurrentWayPointId;
    property NextWayPointId: Integer read FNextWayPointId write FNextWayPointId;
    property RepeatAtTime: string read FRepeatAtTime write FRepeatAtTime;
  end;

implementation

end.
