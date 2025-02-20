﻿unit Ils.Kafka;

interface

uses
  ULibKafka, Classes, Windows, SysUtils, DateUtils, StrUtils,
  System.Generics.Collections, SyncObjs, IniFiles, Ils.Kafka.Conf;

const
  KC_ERROR_KAFKA              = -100;
  KC_ERROR_BROKER             = -101;
  KC_ERROR_PARTITION          = -102;
  KC_ERROR_BOOTSTRAP_SERVERS  = -103;
  KC_ERROR_GROUP_ID           = -104;
  KC_ERROR_AUTO_COMMIT        = -105;

  KP_ERROR_KAFKA              = -200;
  KP_ERROR_TOPIC              = -201;

type

  TLogFunc = function(const AMessage: string; const ASkipTimeStamp: Boolean = False): string;

  TOnTimerFunc = procedure of object;

  TOnMessageFunc = function(const AMessage: AnsiString; const AOffset: Int64; const ATopic: AnsiString): Boolean of object;

  TKafkaConnection = class
  private
    FTopic: AnsiString;
    FGroup: AnsiString;
    FBootstrap: AnsiString;
    FOnLog: TLogFunc;

    function Log(AMessage: string): string;
    function Connect: Integer; virtual; abstract;
    function Disconnect: Integer; virtual; abstract;
    function CheckConnect: Integer; virtual; abstract;
  public
    property OnLog: TLogFunc read FOnLog write FOnLog;
    constructor Create(const ABootstrap, AGroup, ATopic: string);
  end;

  TKafka = class(TKafkaConnection)
  private
    FLastPollTimeStamp: TDateTime;
    FLastError: Integer;
    FLasterrorString: AnsiString;
    FConnected: Boolean;
    FKafka: prd_kafka_t;
    FErrorBuffer: array [1..1024] of AnsiChar;
//    FTermination: array[1..16] of AnsiChar;
    FKafkaConf: prd_kafka_conf_t;

    procedure KafkaCloseSequence; virtual; abstract;
    procedure WaitTillKafkaDestroyed;

    function Connect: Integer; override;
    function Disconnect: Integer; override;
    function CheckConnect: Integer; override;
    function CheckPoll(const APollTimeout: Integer; const AForce: Boolean = False; const APollInterval: Integer = 1000): Integer;
  public
    constructor Create(const ABootstrap, AGroup, ATopic: string);
  end;

  TKafkaMessageState = (msLoaded, msQueued, msBacked);

  TKafkaMessage = class
    pl: AnsiString;
    d: TDateTime;
    s: TKafkaMessageState;
    constructor Create(AStr: AnsiString);
  end;

  PKafkaMessage = ^TKafkaMessage;

  TKafkaMessageHash = class(TDictionary<PKafkaMessage,TKafkaMessage>)
    destructor Destroy; override;
  end;

  TKafkaTimerThread = class(TThread)
  protected
    procedure Execute; override;
  private
    FInterval: Integer;
    FOnTimer: TOnTimerFunc;
  public
    constructor Create(const AOnTimer: TOnTimerFunc; const AInterval: Integer);
  end;

  TKafkaProducer = class(TKafka)
  private
    FMessageCache: TKafkaMessageHash;
    FTimer: TKafkaTimerThread;
    FCS: TCriticalSection;
    FBackupCount: Integer;
    FBackupFileName: string;
    FBackupFile: TFileStream;
    FKafkaTopicConf: prd_kafka_topic_conf_t;
    FKafkaTopic: prd_kafka_topic_t;
    FBackupTimeout: Integer;

    function Connect: Integer; override;

//    procedure CacheFlush(const ADelay: Integer);
//    procedure CacheResend;
//    function CheckBackupFile: Boolean;
//    function BackupMessage(AMsg: TKafkaMessage): Boolean;
//    procedure CloseBackupFile(const ADelete: Boolean);

    procedure Housekeeping;
    function SetDelivered(const AMessage: PKafkaMessage): Boolean;
    procedure KafkaCloseSequence; override;

    function Produce(const AMessage: TKafkaMessage): Integer; overload;
  public
    function Produce(APayload: AnsiString): Integer; overload;

    constructor Create(const ABootstrap, ATopic: string; const ABackupPath: string = ''; const ABackupTimeout: Integer = 120000; const AOnLog: TLogFunc = nil); reintroduce; overload;
    constructor Create(const AParams: TKafkaProducerParams; const AOnLog: TLogFunc = nil); reintroduce; overload;
    destructor Destroy; override;
  end;

  TKafkaConsumer = class(TKafka)
  private
    FKafkaTopicPartitionList: prd_kafka_topic_partition_list_t;
    FKafkaTopicPartition: prd_kafka_topic_partition_t;

    FOnMessage: TOnMessageFunc;
    FKafkaTimerThread: TKafkaTimerThread;

    FOffset: Int64;

    procedure StopSelf;

    procedure KafkaCloseSequence; override;

    function Connect: Integer; overload; override;
    function Disconnect: Integer; override;

    procedure Consume; overload;
    function Consume(const rkmessage: prd_kafka_message_t): Boolean; overload;

  public
    property NextOffset: Int64 read FOffset;

    function Start(const AOffset: Int64 = RD_KAFKA_OFFSET_INVALID; const ACreateThread: Boolean = True): Boolean;
    function Stop: Boolean;

    function CheckTryRestart(var ACheckedOffset: Int64): Boolean;

    function GetMessage(out RMessage: AnsiString): Boolean; //out RLastOffset: Int64): Boolean;

    constructor Create(const AKafkaParams: TKafkaConsumerParams; const AOnMessage: TOnMessageFunc; const AOnLog: TLogFunc); overload;
    constructor Create(const ABootstrap, AGroup, ATopic: string; const AOnMessage: TOnMessageFunc; const AOnLog: TLogFunc); reintroduce; overload;
//    constructor Create(const AKafkaParams: TKafkaConsumerParams; const AOnMessage: TOnMessageFunc; const AOnLog: TLogFunc); overload;
    destructor Destroy; override;
  end;

implementation

//function NextError(var aErrorCode: Integer): Integer;
//begin
//  Dec(aErrorCode);
//  Result := aErrorCode;
//end;


procedure error_cb(rk: prd_kafka_t; err: integer; reason: PAnsiChar; opaque: Pointer); cdecl;
begin
  with TKafka(opaque) do
  begin
    FLastError := err;
    FLasterrorString := reason;
    FConnected := False;
    Log('error - ' + reason);
  end;
end;

procedure dr_cb(rk: prd_kafka_t; payload: Pointer; len: size_t; err: rd_kafka_resp_err_t; opaque, msg_opaque: Pointer); cdecl;
begin
  if err in [RD_KAFKA_RESP_ERR_NO_ERROR] then
    TKafkaProducer(opaque).SetDelivered(msg_opaque);
end;

procedure dr_msg_cb(rk: prd_kafka_t; rkmessage: prd_kafka_message_t; opaque: Pointer); cdecl;
begin

end;

procedure consume_cb(rkmessage: prd_kafka_message_t; opaque: Pointer); cdecl;
begin
  TKafkaConsumer(opaque).Consume(rkmessage);
end;

procedure rebalance_cb(rk: prd_kafka_t; err: rd_kafka_resp_err_t; partitions: prd_kafka_topic_partition_list_t; opaque: Pointer); cdecl;
begin
//void my_rebalance_cb (rd_kafka_t *rk, rd_kafka_resp_err_t err,
//                      rd_kafka_topic_partition_list_t *partitions, void *opaque) {
//   if (err == RD_KAFKA_RESP_ERR__ASSIGN_PARTITIONS) {
//       rd_kafka_topic_partition_t *part;
//       if ((part = rd_kafka_topic_partition_list_find(partitions, "mytopic", 3)))
//           part->offset = 1234;
//       rd_kafka_assign(rk, partitions);
//   }  else {
//       rd_kafka_assign(rk, NULL);
//   }
//}
end;

procedure offset_commit_cb(rk: prd_kafka_t; err: rd_kafka_resp_err_t; offsets: prd_kafka_topic_partition_list_t; opaque: Pointer); cdecl;
begin

end;

{ TKafkaConnection }

constructor TKafkaConnection.Create(const ABootstrap, AGroup, ATopic: string);
begin
  FBootstrap := AnsiString(ABootstrap);
  FTopic := AnsiString(ATopic);
  FGroup := AnsiString(AGroup);
  if (FBootstrap = '') or (FTopic = '') {or (FGroup = '')} then
    raise Exception.Create('необходимо указать все параметры подсоединения к Kafka!');
end;

function TKafkaConnection.Log(AMessage: string): string;
begin
  if Assigned(FOnLog) then
    FOnLog('Kafka - ' + string(FBootstrap) + ';' + string(FTopic) + ';' + string(FGroup) + ': ' + AMessage);
end;


{ TKafka }

function TKafka.CheckPoll(const APollTimeout: Integer; const AForce: Boolean; const APollInterval: Integer): Integer;
begin
  Result := 0;
  if (AForce or ((Now - FLastPollTimeStamp) > (APollInterval * OneMillisecond))) and
     Assigned(FKafka) then
  begin
    Result := rd_kafka_poll(FKafka, APollTimeout);//block for max APollTimeout in ms
    FLastPollTimeStamp := Now;
  end;
end;

function TKafka.Connect: Integer;
begin
  Result := 0;

  FKafkaConf := rd_kafka_conf_new();
  rd_kafka_conf_set_opaque(FKafkaConf, Self);

//	rd_kafka_conf_set(FKafkaConf, 'internal.termination.signal', @(FTermination[1]), @(FErrorBuffer[1]), SizeOf(FErrorBuffer));

  rd_kafka_conf_set_dr_cb(FKafkaConf, dr_cb);
//  rd_kafka_conf_set_dr_msg_cb(FKafkaConf, dr_msg_cb);
//  rd_kafka_conf_set_consume_cb(FKafkaConf, consume_cb); //эта штука потребляет сообщение через callback, тогда Kafka через poll не отдаёт

//  rd_kafka_conf_set_rebalance_cb(FKafkaConf, rebalance_cb);
//  rd_kafka_conf_set_offset_commit_cb(FKafkaConf, offset_commit_cb);
  rd_kafka_conf_set_error_cb(FKafkaConf, error_cb);

  // Set bootstrap broker(s) as a comma-separated list of
  // host or host:port (default port 9092).
  // librdkafka will use the bootstrap brokers to acquire the full
  // set of brokers from the cluster.

  if rd_kafka_conf_set(
        FKafkaConf, 'bootstrap.servers', PAnsiChar(FBootstrap),
        @(FErrorBuffer[1]), sizeof(FErrorBuffer)) <> RD_KAFKA_CONF_OK then
  begin
    Log(string(FErrorBuffer));
    Exit(KC_ERROR_BOOTSTRAP_SERVERS);
  end;

  if rd_kafka_conf_set(
        FKafkaConf, 'group.id', PAnsiChar(FGroup),
        @(FErrorBuffer[1]), sizeof(FErrorBuffer)) <> RD_KAFKA_CONF_OK then
  begin
    Log(string(FErrorBuffer));
    Exit(KC_ERROR_GROUP_ID);
  end;

  if rd_kafka_conf_set(
        FKafkaConf, 'enable.auto.commit', PAnsiChar('false'),
        @(FErrorBuffer[1]), sizeof(FErrorBuffer)) <> RD_KAFKA_CONF_OK then
  begin
    Log(string(FErrorBuffer));
    Exit(KC_ERROR_AUTO_COMMIT);
  end;

end;

constructor TKafka.Create(const ABootstrap, AGroup, ATopic: string);
begin
  inherited Create(ABootstrap, AGroup, ATopic);
  FLastPollTimeStamp := 0;
end;

function TKafka.CheckConnect: Integer;
begin
  if FConnected then
    Exit(0);

  FLastError := 0;
  FLasterrorString := '';
  CheckPoll(5000, True);

  Result := FLastError;
  if Result = 0 then
  begin
    Log('Connected');
    FConnected := True;
  end;
end;

function TKafka.Disconnect: Integer;
begin
  Result := 0;

  if not Assigned(FKafka) then
    Exit;

  KafkaCloseSequence;
end;

procedure TKafka.WaitTillKafkaDestroyed;
var
  run: Integer;
  r: Integer;
begin
  run := 5;
  repeat
    r := rd_kafka_wait_destroyed(100);
    if r = -1 then
      Log('Waiting for librdkafka to decommission');
    Dec(run)
  until (run <= 0) or (r = 0);

  if (run <= 0) then
    Log('Kill unsuccess');
end;

{ TKafkaProducer }

constructor TKafkaProducer.Create(const ABootstrap, ATopic: string; const ABackupPath: string; const ABackupTimeout: Integer; const AOnLog: TLogFunc);
begin
  inherited Create(ABootstrap, '', ATopic);
  FOnLog := AOnLog;
  FBackupCount := 0;
  FBackupTimeout := ABackupTimeout;
  FBackupFileName :=
    IfThen(
      ABackupPath <> '',
      IncludeTrailingPathDelimiter(ABackupPath),
      ExtractFilePath(GetModuleName(HInstance)) + 'backup\'
    ) + ATopic + '.bak';

  FCS := TCriticalSection.Create;
  FMessageCache := TKafkaMessageHash.Create;
  Connect;
  FTimer := TKafkaTimerThread.Create(Housekeeping, 1000);
end;

constructor TKafkaProducer.Create(const AParams: TKafkaProducerParams; const AOnLog: TLogFunc = nil);
begin
  with AParams do
    Self.Create(BootStrap, Topic, BackupPath, BackupTimeout, AOnLog);
end;

destructor TKafkaProducer.Destroy;
begin
  FTimer.Terminate;

//  CacheFlush(0);

  FMessageCache.Free;
  FCS.Free;
  FBackupFile.Free;

  inherited;
end;

function TKafkaProducer.Connect: Integer;
begin
  Result := inherited Connect;

  FKafka := rd_kafka_new(RD_KAFKA_PRODUCER, FKafkaConf, @(FErrorBuffer[1]), sizeof(FErrorBuffer));
  if not Assigned(FKafka) then
  begin
    Log(string(FErrorBuffer));
    Exit(KP_ERROR_KAFKA);
  end;

//  rd_kafka_conf_set_default_topic_conf(FKafkaConf, FKafkaTopicConf);
  FKafkaTopicConf := rd_kafka_topic_conf_new;
  FKafkaTopic :=
    rd_kafka_topic_new(FKafka, PAnsiChar(FTopic), FKafkaTopicConf);
  if not Assigned(FKafkaTopic) then
  begin
    //'Failed to create topic object: '
    Log(string(rd_kafka_err2str(rd_kafka_last_error())));
    rd_kafka_destroy(FKafka);
    Exit(KP_ERROR_TOPIC);
  end;
end;

//function TKafkaProducer.CheckBackupFile: Boolean;
//var
//  FileMode: Integer;
//begin
//  if Assigned(FBackupFile) then
//    Exit(True)
//  else
//  begin
//    if FileExists(FBackupFileName) then
//      FileMode := fmOpenReadWrite + fmShareDenyWrite
//    else
//      FileMode := fmOpenReadWrite + fmShareDenyWrite + fmCreate;
//
//    ForceDirectories(ExtractFilePath(FBackupFileName));
//    try
//      FBackupFile := TFileStream.Create(FBackupFileName, FileMode);
//    except
//    end;
//
//    Result := Assigned(FBackupFile);
//  end;
//end;

//function TKafkaProducer.BackupMessage(AMsg: TKafkaMessage): Boolean;
//var
//  PLLen: UInt32;
//begin
//  Result := False;
//  PLLen := Length(AMsg.pl);
//
//  if PLLen = 0 then
//    Exit(True);
//
//  if not CheckBackupFile then
//    Exit;
//
//  FBackupFile.Write(PLLen, SizeOf(PLLen));
//  FBackupFile.Write(AMsg.pl[1], PLLen);
//  AMsg.s := msBacked;
//  Result := True;
//  Inc(FBackupCount);
//end;

//procedure TKafkaProducer.CloseBackupFile(const ADelete: Boolean);
//begin
//  FBackupFile.Free;
//  FBackupFile := nil;
//  if ADelete then
//    DeleteFile(FBackupFileName);
//end;

//procedure TKafkaProducer.CacheFlush(const ADelay: Integer);
//var
//  k: PKafkaMessage;
//begin
//  try
//    for k in FMessageCache.Keys do
//      if (FMessageCache.Items[k].s in [msQueued]) and
//         (FMessageCache.Items[k].d < (Now - ADelay * OneMillisecond)) then
//        if BackupMessage(FMessageCache.Items[k]) then
//          FMessageCache.ExtractPair(k).Value.Free;
//  finally
//    CloseBackupFile(False)
//  end;
//end;

//procedure TKafkaProducer.CacheResend;
//var
//  PLLen: UInt32;
//  pl: AnsiString;
//begin
//  try
//    if not CheckBackupFile then
//      Exit;
//
//    while FBackupFile.Position < FBackupFile.Size do
//    begin
//      try
//        FBackupFile.Read(PLLen, SizeOf(PLLen));
//        if PLLen > 0 then
//        begin
//          SetLength(pl, PLLen);
//          FBackupFile.Read(pl[1], PLLen);
//          Produce(pl);
//        end;
//      except
//      end;
//    end;
//  finally
//    CloseBackupFile(True);
//  end;
//  Inc(FBackupCount);
//end;

procedure TKafkaProducer.Housekeeping;
begin
  CheckPoll(100);

//  if FCS.TryEnter then
//    try
//      CacheFlush(FBackupTimeout);
//
//      if FConnected then
//        CacheResend;
//
//    finally
//      FCS.Leave;
//    end;
end;

procedure TKafkaProducer.KafkaCloseSequence;
begin
  while (rd_kafka_outq_len(FKafka) > 0) do
    rd_kafka_poll(FKafka, 50);

  rd_kafka_topic_destroy(FKafkaTopic);
  //Destroy handle
  rd_kafka_destroy(FKafka);
  FKafka := nil;

  WaitTillKafkaDestroyed;
end;

//procedure TKafkaProducer.Flush;
//begin
//  while( rd_kafka_outq_len( FKafka ) > 0 ) do
//    rd_kafka_poll( FKafka, 1 );
//
//  rd_kafka_flush(FKafka, 10 ); //wait for max 10 seconds
//end;

function TKafkaProducer.Produce(APayload: AnsiString): Integer;
var
  msg: TKafkaMessage;
begin
  msg := TKafkaMessage.Create(APayload);
  FMessageCache.Add(PKafkaMessage(msg), msg);
  Result := Produce(msg);
end;

function TKafkaProducer.Produce(const AMessage: TKafkaMessage): Integer;
const
  CTryCountDef = 10;
var
  TryCount: Integer;
begin
  TryCount := CTryCountDef;
  if CheckConnect <> 0 then
    Exit(FLastError);

  repeat
    Result :=
      rd_kafka_produce(
        FKafkaTopic,              //Topic object
        integer(RD_KAFKA_PARTITION_UA),    //Use builtin partitioner to select partition
        RD_KAFKA_MSG_F_COPY,      //Make a copy of the payload.
        Pointer(AMessage.pl), //Message payload (value)
        Length(AMessage.pl),      //Message payload (length)
        nil, 0,                   //Optional key and its length
        AMessage                  //Message opaque, provided in delivery report callback as msg_opaque.
      );

    AMessage.s := msQueued;

    case Result of
      0: begin
        if FMessageCache.Count > 100000 then
//        rd_kafka_poll(FKafka, 1000);//block for max 1000ms
          Result := CheckPoll(1000, True);//block for max 1000ms
      end;
      -1: begin
        //Failed to *enqueue* message for producing.
        Log('Failed to produce to topic: ' + string(FTopic) + string(rd_kafka_err2str(rd_kafka_last_error())));
        //Poll to handle delivery reports
        if rd_kafka_last_error() <> RD_KAFKA_RESP_ERR__QUEUE_FULL then
          Break
        else
        begin
          //If the internal queue is full, wait for
          //messages to be delivered and then retry.
          //The internal queue represents both
          //messages to be sent and messages that have
          //been sent or failed, awaiting their
          //delivery report callback to be called.
          //
          //The internal queue is limited by the
          //configuration property
          //queue.buffering.max.messages
//          rd_kafka_poll(FKafka, 1000);//block for max 1000ms
          CheckPoll(1000, True);//block for max 1000ms
        end;
      end;
    end;
    Dec(TryCount);
  until (Result >= 0) or (TryCount <= 0);
end;

function TKafkaProducer.SetDelivered(const AMessage: PKafkaMessage): Boolean;
begin
  Result := True;
  if FCS.TryEnter then
    try
      if FMessageCache.ContainsKey(AMessage) then
        FMessageCache.ExtractPair(AMessage).Value.Free;
    finally
      FCS.Leave;
    end;
end;

{ TKafkaConsumer }

//constructor TKafkaConsumer.Create(const AKafkaParams: TKafkaConsumerParams; const AOnMessage: TOnMessageFunc; const AOnLog: TLogFunc);
//begin
//  Create(AKafkaParams.Bootstrap, AKafkaParams.Group, AKafkaParams.Topic, AOnMessage, AOnLog);
//end;

constructor TKafkaConsumer.Create(const ABootstrap, AGroup, ATopic: string; const AOnMessage: TOnMessageFunc; const AOnLog: TLogFunc);
begin
  FOnMessage := AOnMessage;
  FOnLog := AOnLog;
  FOffset := RD_KAFKA_OFFSET_INVALID;
//  FNextOffset := RD_KAFKA_OFFSET_INVALID;
  FKafkaTimerThread := nil;
  inherited Create(ABootstrap, AGroup, ATopic);
end;

destructor TKafkaConsumer.Destroy;
begin
  Stop;
  if Assigned(FKafka) then
    Disconnect;
  inherited;
end;

function TKafkaConsumer.Connect: Integer;
var
  err: rd_kafka_resp_err_t;
begin
  if Assigned (FKafka) then
    Exit(-1000);

  Result := inherited Connect;
  if Result <> 0 then
    Exit;

  FKafka := rd_kafka_new(RD_KAFKA_CONSUMER, FKafkaConf, @(FErrorBuffer[1]), sizeof(FErrorBuffer));
  if not Assigned(FKafka) then
  begin
    Log('Failed to create new consumer: ' + FErrorBuffer);
    Exit(KC_ERROR_KAFKA);
  end;

  //Add brokers
  if (rd_kafka_brokers_add(FKafka, PAnsiChar(FBootstrap)) = 0) then begin
    rd_kafka_destroy(FKafka);
    Log('No valid brokers specified');
    Exit(KC_ERROR_BROKER);
  end;

  {
  FTopicConf := rd_kafka_topic_conf_new();

  FKafkaTopic := rd_kafka_topic_new(FKafka, PansiChar(FTopic), FTopicConf);
  rd_kafka_consume_start(FKafkaTopic, 0, -2);
  }

//Redirect rd_kafka_poll() to consumer_poll()
  rd_kafka_poll_set_consumer(FKafka);

  FKafkaTopicPartitionList := rd_kafka_topic_partition_list_new(1);
  FKafkaTopicPartition :=
    rd_kafka_topic_partition_list_add(FKafkaTopicPartitionList, PAnsiChar(FTopic), 0);

  FKafkaTopicPartition.offset := FOffset;

//        }
//
//==================================================================================
//        if (mode == 'O') {
//                /* Offset query */
//
//                err = rd_kafka_committed(rk, topics, 5000);
//                if (err) {
//                        fprintf(stderr, "%% Failed to fetch offsets: %s\n",
//                                rd_kafka_err2str(err));
//                        exit(1);
//                }
//
//                for (i = 0 ; i < topics->cnt ; i++) {
//                        rd_kafka_topic_partition_t *p = &topics->elems[i];
//                        printf("Topic \"%s\" partition %"PRId32,
//                               p->topic, p->partition);
//                        if (p->err)
//                                printf(" error %s",
//                                       rd_kafka_err2str(p->err));
//                        else {
//                                printf(" offset %"PRId64"",
//                                       p->offset);
//
//                                if (p->metadata_size)
//                                        printf(" (%d bytes of metadata)",
//                                               (int)p->metadata_size);
//                        }
//                        printf("\n");
//                }
//
//                goto done;
//        }
//==================================================================================
//
//        if (is_subscription) {
//                fprintf(stderr, "%% Subscribing to %d topics\n", topics->cnt);
//
//                if ((err = rd_kafka_subscribe(rk, topics))) {
//                        fprintf(stderr,
//                                "%% Failed to start fconsuming topics: %s\n",
//                                rd_kafka_err2str(err));
//                        exit(1);
//                }
//        } else {
//                fprintf(stderr, "%% Assigning %d partitions\n", topics->cnt);

  err := rd_kafka_assign(FKafka, FKafkaTopicPartitionList);
  if err <> RD_KAFKA_RESP_ERR_NO_ERROR then
  begin
    rd_kafka_topic_partition_list_del(FKafkaTopicPartitionList, PAnsiChar(FTopic), 0);
    rd_kafka_topic_partition_list_destroy(FKafkaTopicPartitionList);
    rd_kafka_destroy(FKafka);
    FKafka := nil;
    Log('Failed to assign partitions: ' + rd_kafka_err2name(err));
    Exit(KC_ERROR_PARTITION);
  end;

//   rd_kafka_commit(FKafka, FKafkaTopicPartitionList, 0);
end;

function TKafkaConsumer.Disconnect: Integer;
var
  err: rd_kafka_resp_err_t;
begin
  if not Assigned(FKafka) then
    Exit(0);

  if FTopic <> '' then
  begin
    rd_kafka_topic_partition_list_del(FKafkaTopicPartitionList, PAnsiChar(FTopic), 0);
    rd_kafka_topic_partition_list_destroy(FKafkaTopicPartitionList);
//  rd_kafka_topic_partition_destroy(FKafkaTopicPartition);
  end;
  err := rd_kafka_consumer_close(FKafka);
  if (err <> RD_KAFKA_RESP_ERR_NO_ERROR)then
    Log('Failed to close consumer: ' + rd_kafka_err2str(err))
  else
    Log('Consumer closed');


  Result := inherited Disconnect;
end;

procedure TKafkaConsumer.StopSelf;
begin
  if Assigned(FKafkaTimerThread) then
    FKafkaTimerThread.Terminate;
  if Assigned(FKafka) then
    Disconnect;
  FKafkaTimerThread := nil;
end;

procedure TKafkaConsumer.Consume;
var
  rkmessage: prd_kafka_message_t;
  Success: Boolean;
begin
  rkmessage := rd_kafka_consumer_poll(FKafka, 1000);
  if Assigned(rkmessage) then
  begin
    Success := Consume(rkmessage);
    rd_kafka_message_destroy(rkmessage);

    if not Success then
      StopSelf;
  end;
end;

function TKafkaConsumer.Consume(const rkmessage: prd_kafka_message_t): Boolean;
var
  ss: AnsiString;
  b: Boolean;
begin
//msg_consume(RKMessage);
  Result := True;
  if rkmessage.err <> RD_KAFKA_RESP_ERR_NO_ERROR then
  begin
//        if Assigned(FOnError) then
//          FOnError('massage error: ' + rd_kafka_err2name(rkmessage.err))
  end
  else begin
    SetLength(ss, rkmessage.len);
    MoveMemory(Pointer(ss), rkmessage.payload, rkmessage.len);
//        for i := 1 to rkmessage.len do
//          ss[i] := AnsiString(PAnsiString(rkmessage.payload))[i];
    if Assigned(FOnMessage) then
    begin
      TThread.Synchronize(FKafkaTimerThread,
        procedure
        begin
          b := False;
          try
            b := FOnMessage(ss, rkmessage.offset, FTopic);
          except
          end;
        end);
        Result := b;
      //CB(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now));
//          Inc(FConsumed);
    end;
  end;
end;

constructor TKafkaConsumer.Create(const AKafkaParams: TKafkaConsumerParams;
  const AOnMessage: TOnMessageFunc; const AOnLog: TLogFunc);
begin
  Create(AKafkaParams.BootStrap, AKafkaParams.Group, AKafkaParams.Topic, AOnMessage, AOnLog);
end;

function TKafkaConsumer.GetMessage(out RMessage: AnsiString): Boolean; //out RLastOffset: Int64): Boolean;
var
  rkmessage: prd_kafka_message_t;
  ss: AnsiString;
begin
  RMessage := '';
  Result := False;

  rkmessage := rd_kafka_consumer_poll(FKafka, 1000);
  if Assigned(rkmessage) then
  begin
    if (rkmessage.err <> RD_KAFKA_RESP_ERR_NO_ERROR) or
       ((rkmessage.offset <> FOffset) and (FOffset >= 0))  then
    begin
  //        if Assigned(FOnError) then
  //          FOnError('massage error: ' + rd_kafka_err2name(rkmessage.err))
    end
    else begin
      FOffset := rkmessage.offset + 1;
      SetLength(ss, rkmessage.len);
      MoveMemory(Pointer(ss), rkmessage.payload, rkmessage.len);
      RMessage := ss;
      Result := True;
    end;
    rd_kafka_message_destroy(rkmessage);
  end;
end;

procedure TKafkaConsumer.KafkaCloseSequence;
begin
  rd_kafka_consumer_close(FKafka);

  rd_kafka_destroy(FKafka);

  FKafka := nil;

  WaitTillKafkaDestroyed;
end;

function TKafkaConsumer.Start(const AOffset: Int64; const ACreateThread: Boolean): Boolean;
begin
  if Assigned(FKafka) or Assigned(FKafkaTimerThread) then
    Exit(False);

  FOffset := AOffset;
//  FNextOffset := AOffset;

  if Connect <> 0 then
    Exit(False);

  if ACreateThread then
    FKafkaTimerThread := TKafkaTimerThread.Create(Consume, 0);

  Result := True;
end;

function TKafkaConsumer.Stop: Boolean;
begin
  if Assigned(FKafkaTimerThread) then
  begin
    FKafkaTimerThread.FreeOnTerminate := False;
    FKafkaTimerThread.Free;
    FKafkaTimerThread := nil;
  end;
  Disconnect;
  Result := True;
end;

function TKafkaConsumer.CheckTryRestart(var ACheckedOffset: Int64): Boolean;
begin
  Result := ACheckedOffset = RD_KAFKA_OFFSET_END;
  if not Result then
  begin
    Log('попытка перезапуска');
    try
      Start(ACheckedOffset);
      ACheckedOffset := RD_KAFKA_OFFSET_END;
      Log('перезапуск успешен');
      Result := True;
    except
      on Ex: Exception do
        Log('ошибка перезапуска "' + Ex.ClassName + '":' + Ex.Message);
    end;
  end;
end;

{ TKafkaTimerThread }

constructor TKafkaTimerThread.Create(const AOnTimer: TOnTimerFunc; const AInterval: Integer);
begin
  FreeOnTerminate := True;
  FInterval := AInterval;
  FOnTimer := AOnTimer;
  inherited Create;
end;


procedure TKafkaTimerThread.Execute;
begin
  while not Terminated do
  begin
    if Assigned(FOnTimer) then
      FOnTimer;

    Sleep(FInterval);
  end;
end;

{ TKafkaMessage }

constructor TKafkaMessage.Create(AStr: AnsiString);
begin
  pl := AStr;
  d := Now;
  s := msLoaded;
end;

{ TKafkaMessageHash }

destructor TKafkaMessageHash.Destroy;
var
  k: PKafkaMessage;
begin
  for k in Keys do
    Items[k].Free;
  Clear;

  inherited;
end;

end.



