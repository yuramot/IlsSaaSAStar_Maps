unit Ils.Stat.Sender;

interface

uses
  blcksock, SysUtils ;

type
  TIlsStatSender = class
  protected
    FSock: TUDPBlockSocket;
  public
//    procedure Send(AHost: string; AMessage: string); overload;
    procedure Send(AMessage: string); //overload;

    constructor Create(AHost: string);
    destructor Destroy; override;
  end;


const
  CIlsStatPort = 11111;

  CSOTest             = 'Test';
  CSONovacomRecv      = 'NovacomRecv';
  CSOWialonRecv       = 'WialonRecv';
  CSOMintransRecv     = 'MintransRecv';
  CSONavtelecomRecv   = 'NavtelecomRecv';
  CSORusAgroTaxiRecv  = 'RusAgroTaxiRecv';
  CSORuptelaRecv      = 'RuptelaRecv';
  CSOWialonIPS2Repl   = 'WialonIPS2Repl';
  CSOKafkaSplit       = 'KafkaSplit';
  CSOKafkaWebPrxy     = 'KafkaWebPrxy';
  CSOTranslationEdit  = 'TranslationEdit';


implementation

{ TIlsStatSender }

constructor TIlsStatSender.Create(AHost: string);
begin
  FSock := TUDPBlockSocket.Create;
  FSock.Connect(AHost, IntToStr(CIlsStatPort));
end;

destructor TIlsStatSender.Destroy;
begin
  FSock.Free;
end;

//procedure TIlsStatSender.Send(AHost: string; AMessage: string);
//begin
//  FSock.SendString(AnsiString(AMessage));
//end;

procedure TIlsStatSender.Send(AMessage: string);
begin
  FSock.SendString(AnsiString(AMessage));
end;

end.
