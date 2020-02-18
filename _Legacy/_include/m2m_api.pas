// ************************************************************************ //
// The types declared in this file were generated from data read from the
// WSDL File described below:
// WSDL     : http://www.mcommunicator.ru/m2m/m2m_api.asmx?WSDL
//  >Import : http://www.mcommunicator.ru/m2m/m2m_api.asmx?WSDL>0
//  >Import : http://www.mcommunicator.ru/m2m/m2m_api.asmx?WSDL>1
// Encoding : utf-8
// Version  : 1.0
// (16.07.2015 17:45:19 - - $Rev: 45757 $)
// ************************************************************************ //

unit m2m_api;

interface

uses InvokeRegistry, SOAPHTTPClient, Types, XSBuiltIns;

const
  IS_OPTN = $0001;
  IS_UNBD = $0002;
  IS_NLBL = $0004;
  IS_UNQL = $0008;
  IS_REF  = $0080;


type

  // ************************************************************************ //
  // The following types, referred to in the WSDL document are not being represented
  // in this file. They are either aliases[@] of other types represented or were referred
  // to but never[!] declared in the document. The types from the latter category
  // typically map to predefined/known XML or Embarcadero types; however, they could also 
  // indicate incorrect WSDL documents that failed to declare or import a schema type.
  // ************************************************************************ //
  // !:long            - "http://www.w3.org/2001/XMLSchema"[Gbl]
  // !:string          - "http://www.w3.org/2001/XMLSchema"[Gbl]
  // !:int             - "http://www.w3.org/2001/XMLSchema"[Gbl]
  // !:dateTime        - "http://www.w3.org/2001/XMLSchema"[Gbl]
  // !:boolean         - "http://www.w3.org/2001/XMLSchema"[Gbl]
  // !:unsignedByte    - "http://www.w3.org/2001/XMLSchema"[Gbl]

  SendMessageIDs       = class;                 { "http://mcommunicator.ru/M2M"[GblCplx] }
  DeliveryInfo         = class;                 { "http://mcommunicator.ru/M2M"[GblCplx] }
  ResultMailingList    = class;                 { "http://mcommunicator.ru/M2M"[GblCplx] }
  MailingListAttribute = class;                 { "http://mcommunicator.ru/M2M"[GblCplx] }
  SubscriberGroupInfo  = class;                 { "http://mcommunicator.ru/M2M"[GblCplx] }
  UserInfo             = class;                 { "http://mcommunicator.ru/M2M"[GblCplx] }
  MessageStatusWithID  = class;                 { "http://mcommunicator.ru/M2M"[GblCplx] }
  MessageInfo          = class;                 { "http://mcommunicator.ru/M2M"[GblCplx] }
  MailingListContact   = class;                 { "http://mcommunicator.ru/M2M"[GblCplx] }
  DeliveryInfoExt      = class;                 { "http://mcommunicator.ru/M2M"[GblCplx] }
  StatisticsInfo2      = class;                 { "http://mcommunicator.ru/M2M"[GblCplx] }
  StatisticsInfo       = class;                 { "http://mcommunicator.ru/M2M"[GblElm] }

  {$SCOPEDENUMS ON}
  { "http://mcommunicator.ru/M2M"[GblSmpl] }
  DeliveryStatus = (Pending, Sending, Sent, NotSent, Delivered, NotDelivered, TimedOut, Error);

  { "http://mcommunicator.ru/M2M"[GblSmpl] }
  AccessLevel = (Administrator, Operator, BaseUser);

  { "http://mcommunicator.ru/M2M"[GblSmpl] }
  RequestMessageType = (All, MO, MT);

  { "http://mcommunicator.ru/M2M"[GblSmpl] }
  MessageType = (MO, MT);

  {$SCOPEDENUMS OFF}

  ArrayOfSendMessageIDs2 = array of SendMessageIDs;   { "http://mcommunicator.ru/M2M"[GblCplx] }
  ArrayOfString = array of string;              { "http://mcommunicator.ru/M2M"[GblCplx] }


  // ************************************************************************ //
  // XML       : SendMessageIDs, global, <complexType>
  // Namespace : http://mcommunicator.ru/M2M
  // ************************************************************************ //
  SendMessageIDs = class(TRemotable)
  private
    FMsid: string;
    FMsid_Specified: boolean;
    FMessageID: Int64;
    procedure SetMsid(Index: Integer; const Astring: string);
    function  Msid_Specified(Index: Integer): boolean;
  published
    property Msid:      string  Index (IS_OPTN) read FMsid write SetMsid stored Msid_Specified;
    property MessageID: Int64   read FMessageID write FMessageID;
  end;

  ArrayOfDeliveryInfo2 = array of DeliveryInfo;   { "http://mcommunicator.ru/M2M"[GblCplx] }
  ArrayOfLong = array of Int64;                 { "http://mcommunicator.ru/M2M"[GblCplx] }


  // ************************************************************************ //
  // XML       : DeliveryInfo, global, <complexType>
  // Namespace : http://mcommunicator.ru/M2M
  // ************************************************************************ //
  DeliveryInfo = class(TRemotable)
  private
    FMsid: string;
    FMsid_Specified: boolean;
    FDeliveryStatus: DeliveryStatus;
    FDeliveryDate: TXSDateTime;
    FUserDeliveryDate: TXSDateTime;
    FPartCount: Integer;
    procedure SetMsid(Index: Integer; const Astring: string);
    function  Msid_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property Msid:             string          Index (IS_OPTN) read FMsid write SetMsid stored Msid_Specified;
    property DeliveryStatus:   DeliveryStatus  read FDeliveryStatus write FDeliveryStatus;
    property DeliveryDate:     TXSDateTime     read FDeliveryDate write FDeliveryDate;
    property UserDeliveryDate: TXSDateTime     Index (IS_NLBL) read FUserDeliveryDate write FUserDeliveryDate;
    property PartCount:        Integer         read FPartCount write FPartCount;
  end;

  StringArray = array of string;                { "http://mcommunicator.ru/M2M/AbstractTypes"[GblCplx] }
  ArrayOfSendMessageIDs =  type ArrayOfSendMessageIDs2;      { "http://mcommunicator.ru/M2M"[GblElm] }
  long            =  type Int64;      { "http://mcommunicator.ru/M2M"[GblElm] }
  ArrayOfDeliveryInfo =  type ArrayOfDeliveryInfo2;      { "http://mcommunicator.ru/M2M"[GblElm] }


  // ************************************************************************ //
  // XML       : ResultMailingList, global, <complexType>
  // Namespace : http://mcommunicator.ru/M2M
  // ************************************************************************ //
  ResultMailingList = class(TRemotable)
  private
    FID: Int64;
    FName_: string;
    FName__Specified: boolean;
    procedure SetName_(Index: Integer; const Astring: string);
    function  Name__Specified(Index: Integer): boolean;
  published
    property ID:    Int64   read FID write FID;
    property Name_: string  Index (IS_OPTN) read FName_ write SetName_ stored Name__Specified;
  end;

  ArrayOfSubscriberGroupInfo2 = array of SubscriberGroupInfo;   { "http://mcommunicator.ru/M2M"[GblCplx] }
  ArrayOfSubscriberGroupInfo =  type ArrayOfSubscriberGroupInfo2;      { "http://mcommunicator.ru/M2M"[GblElm] }
  ArrayOfResultMailingList2 = array of ResultMailingList;   { "http://mcommunicator.ru/M2M"[GblCplx] }
  ArrayOfResultMailingList =  type ArrayOfResultMailingList2;      { "http://mcommunicator.ru/M2M"[GblElm] }
  ArrayOfMailingListAttribute = array of MailingListAttribute;   { "http://mcommunicator.ru/M2M"[GblCplx] }


  // ************************************************************************ //
  // XML       : MailingListAttribute, global, <complexType>
  // Namespace : http://mcommunicator.ru/M2M
  // ************************************************************************ //
  MailingListAttribute = class(TRemotable)
  private
    FName_: string;
    FName__Specified: boolean;
    FValue: string;
    FValue_Specified: boolean;
    procedure SetName_(Index: Integer; const Astring: string);
    function  Name__Specified(Index: Integer): boolean;
    procedure SetValue(Index: Integer; const Astring: string);
    function  Value_Specified(Index: Integer): boolean;
  published
    property Name_: string  Index (IS_OPTN) read FName_ write SetName_ stored Name__Specified;
    property Value: string  Index (IS_OPTN) read FValue write SetValue stored Value_Specified;
  end;



  // ************************************************************************ //
  // XML       : SubscriberGroupInfo, global, <complexType>
  // Namespace : http://mcommunicator.ru/M2M
  // ************************************************************************ //
  SubscriberGroupInfo = class(TRemotable)
  private
    FID: Int64;
    FName_: string;
    FName__Specified: boolean;
    FColor: Integer;
    procedure SetName_(Index: Integer; const Astring: string);
    function  Name__Specified(Index: Integer): boolean;
  published
    property ID:    Int64    read FID write FID;
    property Name_: string   Index (IS_OPTN) read FName_ write SetName_ stored Name__Specified;
    property Color: Integer  read FColor write FColor;
  end;

  ArrayOfUserInfo2 = array of UserInfo;         { "http://mcommunicator.ru/M2M"[GblCplx] }
  ArrayOfUserInfo =  type ArrayOfUserInfo2;      { "http://mcommunicator.ru/M2M"[GblElm] }


  // ************************************************************************ //
  // XML       : UserInfo, global, <complexType>
  // Namespace : http://mcommunicator.ru/M2M
  // ************************************************************************ //
  UserInfo = class(TRemotable)
  private
    FName_: string;
    FName__Specified: boolean;
    FMsid: string;
    FMsid_Specified: boolean;
    FEmail: string;
    FEmail_Specified: boolean;
    FGroupID: Int64;
    FWebAccessEnabled: Boolean;
    FAccessLevel: AccessLevel;
    procedure SetName_(Index: Integer; const Astring: string);
    function  Name__Specified(Index: Integer): boolean;
    procedure SetMsid(Index: Integer; const Astring: string);
    function  Msid_Specified(Index: Integer): boolean;
    procedure SetEmail(Index: Integer; const Astring: string);
    function  Email_Specified(Index: Integer): boolean;
  published
    property Name_:            string       Index (IS_OPTN) read FName_ write SetName_ stored Name__Specified;
    property Msid:             string       Index (IS_OPTN) read FMsid write SetMsid stored Msid_Specified;
    property Email:            string       Index (IS_OPTN) read FEmail write SetEmail stored Email_Specified;
    property GroupID:          Int64        read FGroupID write FGroupID;
    property WebAccessEnabled: Boolean      Index (IS_NLBL) read FWebAccessEnabled write FWebAccessEnabled;
    property AccessLevel:      AccessLevel  Index (IS_NLBL) read FAccessLevel write FAccessLevel;
  end;

  ArrayOfDeliveryInfoExt = array of DeliveryInfoExt;   { "http://mcommunicator.ru/M2M"[GblCplx] }
  ArrayOfMessageInfo2 = array of MessageInfo;   { "http://mcommunicator.ru/M2M"[GblCplx] }
  ArrayOfMessageInfo =  type ArrayOfMessageInfo2;      { "http://mcommunicator.ru/M2M"[GblElm] }
  ArrayOfMessageStatusWithID2 = array of MessageStatusWithID;   { "http://mcommunicator.ru/M2M"[GblCplx] }
  ArrayOfMessageStatusWithID =  type ArrayOfMessageStatusWithID2;      { "http://mcommunicator.ru/M2M"[GblElm] }


  // ************************************************************************ //
  // XML       : MessageStatusWithID, global, <complexType>
  // Namespace : http://mcommunicator.ru/M2M
  // ************************************************************************ //
  MessageStatusWithID = class(TRemotable)
  private
    FMessageID: Int64;
    FDelivery: ArrayOfDeliveryInfo2;
    FDelivery_Specified: boolean;
    procedure SetDelivery(Index: Integer; const AArrayOfDeliveryInfo2: ArrayOfDeliveryInfo2);
    function  Delivery_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property MessageID: Int64                 read FMessageID write FMessageID;
    property Delivery:  ArrayOfDeliveryInfo2  Index (IS_OPTN) read FDelivery write SetDelivery stored Delivery_Specified;
  end;



  // ************************************************************************ //
  // XML       : MessageInfo, global, <complexType>
  // Namespace : http://mcommunicator.ru/M2M
  // ************************************************************************ //
  MessageInfo = class(TRemotable)
  private
    FMessageID: Int64;
    FCreationDate: TXSDateTime;
    FSenderMsid: string;
    FSenderMsid_Specified: boolean;
    FSenderName: string;
    FSenderName_Specified: boolean;
    FDeliveryInfo: ArrayOfDeliveryInfoExt;
    FDeliveryInfo_Specified: boolean;
    FMessageType: MessageType;
    FMessageText: string;
    FMessageText_Specified: boolean;
    procedure SetSenderMsid(Index: Integer; const Astring: string);
    function  SenderMsid_Specified(Index: Integer): boolean;
    procedure SetSenderName(Index: Integer; const Astring: string);
    function  SenderName_Specified(Index: Integer): boolean;
    procedure SetDeliveryInfo(Index: Integer; const AArrayOfDeliveryInfoExt: ArrayOfDeliveryInfoExt);
    function  DeliveryInfo_Specified(Index: Integer): boolean;
    procedure SetMessageText(Index: Integer; const Astring: string);
    function  MessageText_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property MessageID:    Int64                   read FMessageID write FMessageID;
    property CreationDate: TXSDateTime             read FCreationDate write FCreationDate;
    property SenderMsid:   string                  Index (IS_OPTN) read FSenderMsid write SetSenderMsid stored SenderMsid_Specified;
    property SenderName:   string                  Index (IS_OPTN) read FSenderName write SetSenderName stored SenderName_Specified;
    property DeliveryInfo: ArrayOfDeliveryInfoExt  Index (IS_OPTN) read FDeliveryInfo write SetDeliveryInfo stored DeliveryInfo_Specified;
    property MessageType:  MessageType             read FMessageType write FMessageType;
    property MessageText:  string                  Index (IS_OPTN) read FMessageText write SetMessageText stored MessageText_Specified;
  end;

  ArrayOfMailingListContact2 = array of MailingListContact;   { "http://mcommunicator.ru/M2M"[GblCplx] }
  ArrayOfMailingListContact =  type ArrayOfMailingListContact2;      { "http://mcommunicator.ru/M2M"[GblElm] }


  // ************************************************************************ //
  // XML       : MailingListContact, global, <complexType>
  // Namespace : http://mcommunicator.ru/M2M
  // ************************************************************************ //
  MailingListContact = class(TRemotable)
  private
    FMsid: string;
    FMsid_Specified: boolean;
    FName_: string;
    FName__Specified: boolean;
    FAttributes: ArrayOfMailingListAttribute;
    FAttributes_Specified: boolean;
    procedure SetMsid(Index: Integer; const Astring: string);
    function  Msid_Specified(Index: Integer): boolean;
    procedure SetName_(Index: Integer; const Astring: string);
    function  Name__Specified(Index: Integer): boolean;
    procedure SetAttributes(Index: Integer; const AArrayOfMailingListAttribute: ArrayOfMailingListAttribute);
    function  Attributes_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property Msid:       string                       Index (IS_OPTN) read FMsid write SetMsid stored Msid_Specified;
    property Name_:      string                       Index (IS_OPTN) read FName_ write SetName_ stored Name__Specified;
    property Attributes: ArrayOfMailingListAttribute  Index (IS_OPTN) read FAttributes write SetAttributes stored Attributes_Specified;
  end;



  // ************************************************************************ //
  // XML       : DeliveryInfoExt, global, <complexType>
  // Namespace : http://mcommunicator.ru/M2M
  // ************************************************************************ //
  DeliveryInfoExt = class(TRemotable)
  private
    FTargetMsid: string;
    FTargetMsid_Specified: boolean;
    FDeliveryStatus: DeliveryStatus;
    FDeliveryDate: TXSDateTime;
    FTargetName: string;
    FTargetName_Specified: boolean;
    FUserDeliveryDate: TXSDateTime;
    procedure SetTargetMsid(Index: Integer; const Astring: string);
    function  TargetMsid_Specified(Index: Integer): boolean;
    procedure SetTargetName(Index: Integer; const Astring: string);
    function  TargetName_Specified(Index: Integer): boolean;
  public
    destructor Destroy; override;
  published
    property TargetMsid:       string          Index (IS_OPTN) read FTargetMsid write SetTargetMsid stored TargetMsid_Specified;
    property DeliveryStatus:   DeliveryStatus  read FDeliveryStatus write FDeliveryStatus;
    property DeliveryDate:     TXSDateTime     read FDeliveryDate write FDeliveryDate;
    property TargetName:       string          Index (IS_OPTN) read FTargetName write SetTargetName stored TargetName_Specified;
    property UserDeliveryDate: TXSDateTime     Index (IS_NLBL) read FUserDeliveryDate write FUserDeliveryDate;
  end;



  // ************************************************************************ //
  // XML       : StatisticsInfo, global, <complexType>
  // Namespace : http://mcommunicator.ru/M2M
  // ************************************************************************ //
  StatisticsInfo2 = class(TRemotable)
  private
    FYear: Integer;
    FMonth: Byte;
    FPacketSize: Integer;
    FIncludedSMS: Integer;
    FExtraSMS: Integer;
    FRemainder: Integer;
    FExternalPacketSize: Integer;
    FExternalIncludedSms: Integer;
    FExternalExtraSms: Integer;
    FExternalReminder: Integer;
    FGeneralPacketSize: Integer;
    FGeneralIncludedSms: Integer;
    FGeneralExtraSms: Integer;
    FGeneralReminder: Integer;
    FMegafonRemainder: Integer;
    FBeelineRemainder: Integer;
    FMegafonTransactional: Integer;
    FBeelineTransactional: Integer;
    FTotalBilledSms: Integer;
  published
    property Year:                 Integer  read FYear write FYear;
    property Month:                Byte     read FMonth write FMonth;
    property PacketSize:           Integer  read FPacketSize write FPacketSize;
    property IncludedSMS:          Integer  read FIncludedSMS write FIncludedSMS;
    property ExtraSMS:             Integer  read FExtraSMS write FExtraSMS;
    property Remainder:            Integer  read FRemainder write FRemainder;
    property ExternalPacketSize:   Integer  read FExternalPacketSize write FExternalPacketSize;
    property ExternalIncludedSms:  Integer  read FExternalIncludedSms write FExternalIncludedSms;
    property ExternalExtraSms:     Integer  read FExternalExtraSms write FExternalExtraSms;
    property ExternalReminder:     Integer  read FExternalReminder write FExternalReminder;
    property GeneralPacketSize:    Integer  read FGeneralPacketSize write FGeneralPacketSize;
    property GeneralIncludedSms:   Integer  read FGeneralIncludedSms write FGeneralIncludedSms;
    property GeneralExtraSms:      Integer  read FGeneralExtraSms write FGeneralExtraSms;
    property GeneralReminder:      Integer  read FGeneralReminder write FGeneralReminder;
    property MegafonRemainder:     Integer  read FMegafonRemainder write FMegafonRemainder;
    property BeelineRemainder:     Integer  read FBeelineRemainder write FBeelineRemainder;
    property MegafonTransactional: Integer  read FMegafonTransactional write FMegafonTransactional;
    property BeelineTransactional: Integer  read FBeelineTransactional write FBeelineTransactional;
    property TotalBilledSms:       Integer  read FTotalBilledSms write FTotalBilledSms;
  end;



  // ************************************************************************ //
  // XML       : StatisticsInfo, global, <element>
  // Namespace : http://mcommunicator.ru/M2M
  // ************************************************************************ //
  StatisticsInfo = class(StatisticsInfo2)
  private
  published
  end;


  // ************************************************************************ //
  // Namespace : http://mcommunicator.ru/M2M
  // soapAction: http://mcommunicator.ru/M2M/%operationName%
  // transport : http://schemas.xmlsoap.org/soap/http
  // style     : document
  // use       : literal
  // binding   : MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap12
  // service   : MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_API
  // port      : MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap12
  // URL       : http://www.mcommunicator.ru/m2m/m2m_api.asmx
  // ************************************************************************ //
  MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap = interface(IInvokable)
  ['{7AF6AC5B-4F32-1646-5C7F-81EA421E0EFC}']
    function  SendMessage(const msid: string; const message_: string; const naming: string; const login: string; const password: string): Int64; stdcall;
    function  SendMessageAtDate(const msid: string; const message_: string; const naming: string; const scheduledSendDate: TXSDateTime; const login: string; const password: string
                                ): Int64; stdcall;
    function  SendMessageToMultipleSubscribers(const msids: ArrayOfString; const message_: string; const naming: string; const login: string; const password: string): ArrayOfSendMessageIDs2; stdcall;
    function  SendMessageToMultipleSubscribersAtDate(const msids: ArrayOfString; const message_: string; const naming: string; const scheduledSendDate: TXSDateTime; const login: string; const password: string
                                                     ): ArrayOfSendMessageIDs2; stdcall;
    function  SendMessages(const msids: ArrayOfString; const message_: string; const naming: string; const login: string; const password: string): ArrayOfSendMessageIDs2; stdcall;
    function  SendMessagesAtDate(const msids: ArrayOfString; const message_: string; const naming: string; const scheduledSendDate: TXSDateTime; const login: string; const password: string
                                 ): ArrayOfSendMessageIDs2; stdcall;
    function  GetMessageStatus(const messageID: Int64; const login: string; const password: string): ArrayOfDeliveryInfo2; stdcall;
    function  GetMessagesStatus(const messageIDs: ArrayOfLong; const login: string; const password: string): ArrayOfMessageStatusWithID2; stdcall;
    function  GetMessages(const messageType: RequestMessageType; const subscriberMsids: ArrayOfString; const DateFrom: TXSDateTime; const DateTo: TXSDateTime; const login: string; const password: string
                          ): ArrayOfMessageInfo2; stdcall;
    function  GetStatistics(const login: string; const password: string): StatisticsInfo2; stdcall;
    function  CreateMailingList(const name_: string; const contacts: ArrayOfMailingListContact2; const login: string; const password: string): Int64; stdcall;
    function  GetMailingLists(const login: string; const password: string): ArrayOfResultMailingList2; stdcall;
    procedure DeleteMailingLists(const mailingListIDs: ArrayOfLong; const login: string; const password: string); stdcall;
    function  GetMailingListContacts(const mailingListId: Int64; const login: string; const password: string): ArrayOfMailingListContact2; stdcall;
    procedure SetMailingListContacts(const mailingListId: Int64; const contacts: ArrayOfMailingListContact2; const login: string; const password: string); stdcall;
    function  AddUser(const login: string; const password: string; const userName: string; const userMSID: string; const userEmail: string; const webAccessEnabled: Boolean; 
                      const accessLevel: AccessLevel; const userGroupId: Int64): Int64; stdcall;
    function  GetGroups(const login: string; const password: string): ArrayOfSubscriberGroupInfo2; stdcall;
    function  GetUsers(const login: string; const password: string): ArrayOfUserInfo2; stdcall;
  end;


  // ************************************************************************ //
  // Namespace : http://mcommunicator.ru/M2M
  // style     : ????
  // use       : ????
  // binding   : MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet
  // service   : MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_API
  // port      : MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet
  // ************************************************************************ //
  MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet = interface(IInvokable)
  ['{2CA6DE19-A328-D16D-BD1A-F81014C9CDBB}']
    function  SendMessage(const msid: string; const message_: string; const naming: string; const login: string; const password: string): long; stdcall;
    function  SendMessageAtDate(const msid: string; const message_: string; const naming: string; const scheduledSendDate: string; const login: string; const password: string
                                ): long; stdcall;
    function  SendMessageToMultipleSubscribers(const msids: StringArray; const message_: string; const naming: string; const login: string; const password: string): ArrayOfSendMessageIDs; stdcall;
    function  SendMessageToMultipleSubscribersAtDate(const msids: StringArray; const message_: string; const naming: string; const scheduledSendDate: string; const login: string; const password: string
                                                     ): ArrayOfSendMessageIDs; stdcall;
    function  SendMessages(const msids: StringArray; const message_: string; const naming: string; const login: string; const password: string): ArrayOfSendMessageIDs; stdcall;
    function  SendMessagesAtDate(const msids: StringArray; const message_: string; const naming: string; const scheduledSendDate: string; const login: string; const password: string
                                 ): ArrayOfSendMessageIDs; stdcall;
    function  GetMessageStatus(const messageID: string; const login: string; const password: string): ArrayOfDeliveryInfo; stdcall;
    function  GetMessagesStatus(const messageIDs: StringArray; const login: string; const password: string): ArrayOfMessageStatusWithID; stdcall;
    function  GetMessages(const messageType: string; const subscriberMsids: StringArray; const DateFrom: string; const DateTo: string; const login: string; const password: string
                          ): ArrayOfMessageInfo; stdcall;
    function  GetStatistics(const login: string; const password: string): StatisticsInfo; stdcall;
    function  GetMailingLists(const login: string; const password: string): ArrayOfResultMailingList; stdcall;
    procedure DeleteMailingLists(const mailingListIDs: StringArray; const login: string; const password: string); stdcall;
    function  GetMailingListContacts(const mailingListId: string; const login: string; const password: string): ArrayOfMailingListContact; stdcall;
    function  AddUser(const login: string; const password: string; const userName: string; const userMSID: string; const userEmail: string; const webAccessEnabled: string; 
                      const accessLevel: string; const userGroupId: string): long; stdcall;
    function  GetGroups(const login: string; const password: string): ArrayOfSubscriberGroupInfo; stdcall;
    function  GetUsers(const login: string; const password: string): ArrayOfUserInfo; stdcall;
  end;


  // ************************************************************************ //
  // Namespace : http://mcommunicator.ru/M2M
  // style     : ????
  // use       : ????
  // binding   : MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost
  // service   : MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_API
  // port      : MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost
  // ************************************************************************ //
  MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost = interface(IInvokable)
  ['{FB140344-B8BD-7136-E6DF-1C750923252C}']
    function  SendMessage(const msid: string; const message_: string; const naming: string; const login: string; const password: string): long; stdcall;
    function  SendMessageAtDate(const msid: string; const message_: string; const naming: string; const scheduledSendDate: string; const login: string; const password: string
                                ): long; stdcall;
    function  SendMessageToMultipleSubscribers(const msids: StringArray; const message_: string; const naming: string; const login: string; const password: string): ArrayOfSendMessageIDs; stdcall;
    function  SendMessageToMultipleSubscribersAtDate(const msids: StringArray; const message_: string; const naming: string; const scheduledSendDate: string; const login: string; const password: string
                                                     ): ArrayOfSendMessageIDs; stdcall;
    function  SendMessages(const msids: StringArray; const message_: string; const naming: string; const login: string; const password: string): ArrayOfSendMessageIDs; stdcall;
    function  SendMessagesAtDate(const msids: StringArray; const message_: string; const naming: string; const scheduledSendDate: string; const login: string; const password: string
                                 ): ArrayOfSendMessageIDs; stdcall;
    function  GetMessageStatus(const messageID: string; const login: string; const password: string): ArrayOfDeliveryInfo; stdcall;
    function  GetMessagesStatus(const messageIDs: StringArray; const login: string; const password: string): ArrayOfMessageStatusWithID; stdcall;
    function  GetMessages(const messageType: string; const subscriberMsids: StringArray; const DateFrom: string; const DateTo: string; const login: string; const password: string
                          ): ArrayOfMessageInfo; stdcall;
    function  GetStatistics(const login: string; const password: string): StatisticsInfo; stdcall;
    function  GetMailingLists(const login: string; const password: string): ArrayOfResultMailingList; stdcall;
    procedure DeleteMailingLists(const mailingListIDs: StringArray; const login: string; const password: string); stdcall;
    function  GetMailingListContacts(const mailingListId: string; const login: string; const password: string): ArrayOfMailingListContact; stdcall;
    function  AddUser(const login: string; const password: string; const userName: string; const userMSID: string; const userEmail: string; const webAccessEnabled: string; 
                      const accessLevel: string; const userGroupId: string): long; stdcall;
    function  GetGroups(const login: string; const password: string): ArrayOfSubscriberGroupInfo; stdcall;
    function  GetUsers(const login: string; const password: string): ArrayOfUserInfo; stdcall;
  end;

function GetMTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap;
function GetMTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet;
function GetMTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost;


implementation
  uses SysUtils;

function GetMTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap;
const
  defWSDL = 'http://www.mcommunicator.ru/m2m/m2m_api.asmx?WSDL';
  defURL  = 'http://www.mcommunicator.ru/m2m/m2m_api.asmx';
  defSvc  = 'MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_API';
  defPrt  = 'MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap12';
var
  RIO: THTTPRIO;
begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := defWSDL
    else
      Addr := defURL;
  end;
  if HTTPRIO = nil then
    RIO := THTTPRIO.Create(nil)
  else
    RIO := HTTPRIO;
  try
    Result := (RIO as MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap);
    if UseWSDL then
    begin
      RIO.WSDLLocation := Addr;
      RIO.Service := defSvc;
      RIO.Port := defPrt;
    end else
      RIO.URL := Addr;
  finally
    if (Result = nil) and (HTTPRIO = nil) then
      RIO.Free;
  end;
end;


function GetMTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet;
const
  defWSDL = 'http://www.mcommunicator.ru/m2m/m2m_api.asmx?WSDL';
  defURL  = '';
  defSvc  = 'MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_API';
  defPrt  = 'MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet';
var
  RIO: THTTPRIO;
begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := defWSDL
    else
      Addr := defURL;
  end;
  if HTTPRIO = nil then
    RIO := THTTPRIO.Create(nil)
  else
    RIO := HTTPRIO;
  try
    Result := (RIO as MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet);
    if UseWSDL then
    begin
      RIO.WSDLLocation := Addr;
      RIO.Service := defSvc;
      RIO.Port := defPrt;
    end else
      RIO.URL := Addr;
  finally
    if (Result = nil) and (HTTPRIO = nil) then
      RIO.Free;
  end;
end;


function GetMTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost;
const
  defWSDL = 'http://www.mcommunicator.ru/m2m/m2m_api.asmx?WSDL';
  defURL  = '';
  defSvc  = 'MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_API';
  defPrt  = 'MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost';
var
  RIO: THTTPRIO;
begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := defWSDL
    else
      Addr := defURL;
  end;
  if HTTPRIO = nil then
    RIO := THTTPRIO.Create(nil)
  else
    RIO := HTTPRIO;
  try
    Result := (RIO as MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost);
    if UseWSDL then
    begin
      RIO.WSDLLocation := Addr;
      RIO.Service := defSvc;
      RIO.Port := defPrt;
    end else
      RIO.URL := Addr;
  finally
    if (Result = nil) and (HTTPRIO = nil) then
      RIO.Free;
  end;
end;


procedure SendMessageIDs.SetMsid(Index: Integer; const Astring: string);
begin
  FMsid := Astring;
  FMsid_Specified := True;
end;

function SendMessageIDs.Msid_Specified(Index: Integer): boolean;
begin
  Result := FMsid_Specified;
end;

destructor DeliveryInfo.Destroy;
begin
  SysUtils.FreeAndNil(FDeliveryDate);
  SysUtils.FreeAndNil(FUserDeliveryDate);
  inherited Destroy;
end;

procedure DeliveryInfo.SetMsid(Index: Integer; const Astring: string);
begin
  FMsid := Astring;
  FMsid_Specified := True;
end;

function DeliveryInfo.Msid_Specified(Index: Integer): boolean;
begin
  Result := FMsid_Specified;
end;

procedure ResultMailingList.SetName_(Index: Integer; const Astring: string);
begin
  FName_ := Astring;
  FName__Specified := True;
end;

function ResultMailingList.Name__Specified(Index: Integer): boolean;
begin
  Result := FName__Specified;
end;

procedure MailingListAttribute.SetName_(Index: Integer; const Astring: string);
begin
  FName_ := Astring;
  FName__Specified := True;
end;

function MailingListAttribute.Name__Specified(Index: Integer): boolean;
begin
  Result := FName__Specified;
end;

procedure MailingListAttribute.SetValue(Index: Integer; const Astring: string);
begin
  FValue := Astring;
  FValue_Specified := True;
end;

function MailingListAttribute.Value_Specified(Index: Integer): boolean;
begin
  Result := FValue_Specified;
end;

procedure SubscriberGroupInfo.SetName_(Index: Integer; const Astring: string);
begin
  FName_ := Astring;
  FName__Specified := True;
end;

function SubscriberGroupInfo.Name__Specified(Index: Integer): boolean;
begin
  Result := FName__Specified;
end;

procedure UserInfo.SetName_(Index: Integer; const Astring: string);
begin
  FName_ := Astring;
  FName__Specified := True;
end;

function UserInfo.Name__Specified(Index: Integer): boolean;
begin
  Result := FName__Specified;
end;

procedure UserInfo.SetMsid(Index: Integer; const Astring: string);
begin
  FMsid := Astring;
  FMsid_Specified := True;
end;

function UserInfo.Msid_Specified(Index: Integer): boolean;
begin
  Result := FMsid_Specified;
end;

procedure UserInfo.SetEmail(Index: Integer; const Astring: string);
begin
  FEmail := Astring;
  FEmail_Specified := True;
end;

function UserInfo.Email_Specified(Index: Integer): boolean;
begin
  Result := FEmail_Specified;
end;

destructor MessageStatusWithID.Destroy;
var
  I: Integer;
begin
  for I := 0 to System.Length(FDelivery)-1 do
    SysUtils.FreeAndNil(FDelivery[I]);
  System.SetLength(FDelivery, 0);
  inherited Destroy;
end;

procedure MessageStatusWithID.SetDelivery(Index: Integer; const AArrayOfDeliveryInfo2: ArrayOfDeliveryInfo2);
begin
  FDelivery := AArrayOfDeliveryInfo2;
  FDelivery_Specified := True;
end;

function MessageStatusWithID.Delivery_Specified(Index: Integer): boolean;
begin
  Result := FDelivery_Specified;
end;

destructor MessageInfo.Destroy;
var
  I: Integer;
begin
  for I := 0 to System.Length(FDeliveryInfo)-1 do
    SysUtils.FreeAndNil(FDeliveryInfo[I]);
  System.SetLength(FDeliveryInfo, 0);
  SysUtils.FreeAndNil(FCreationDate);
  inherited Destroy;
end;

procedure MessageInfo.SetSenderMsid(Index: Integer; const Astring: string);
begin
  FSenderMsid := Astring;
  FSenderMsid_Specified := True;
end;

function MessageInfo.SenderMsid_Specified(Index: Integer): boolean;
begin
  Result := FSenderMsid_Specified;
end;

procedure MessageInfo.SetSenderName(Index: Integer; const Astring: string);
begin
  FSenderName := Astring;
  FSenderName_Specified := True;
end;

function MessageInfo.SenderName_Specified(Index: Integer): boolean;
begin
  Result := FSenderName_Specified;
end;

procedure MessageInfo.SetDeliveryInfo(Index: Integer; const AArrayOfDeliveryInfoExt: ArrayOfDeliveryInfoExt);
begin
  FDeliveryInfo := AArrayOfDeliveryInfoExt;
  FDeliveryInfo_Specified := True;
end;

function MessageInfo.DeliveryInfo_Specified(Index: Integer): boolean;
begin
  Result := FDeliveryInfo_Specified;
end;

procedure MessageInfo.SetMessageText(Index: Integer; const Astring: string);
begin
  FMessageText := Astring;
  FMessageText_Specified := True;
end;

function MessageInfo.MessageText_Specified(Index: Integer): boolean;
begin
  Result := FMessageText_Specified;
end;

destructor MailingListContact.Destroy;
var
  I: Integer;
begin
  for I := 0 to System.Length(FAttributes)-1 do
    SysUtils.FreeAndNil(FAttributes[I]);
  System.SetLength(FAttributes, 0);
  inherited Destroy;
end;

procedure MailingListContact.SetMsid(Index: Integer; const Astring: string);
begin
  FMsid := Astring;
  FMsid_Specified := True;
end;

function MailingListContact.Msid_Specified(Index: Integer): boolean;
begin
  Result := FMsid_Specified;
end;

procedure MailingListContact.SetName_(Index: Integer; const Astring: string);
begin
  FName_ := Astring;
  FName__Specified := True;
end;

function MailingListContact.Name__Specified(Index: Integer): boolean;
begin
  Result := FName__Specified;
end;

procedure MailingListContact.SetAttributes(Index: Integer; const AArrayOfMailingListAttribute: ArrayOfMailingListAttribute);
begin
  FAttributes := AArrayOfMailingListAttribute;
  FAttributes_Specified := True;
end;

function MailingListContact.Attributes_Specified(Index: Integer): boolean;
begin
  Result := FAttributes_Specified;
end;

destructor DeliveryInfoExt.Destroy;
begin
  SysUtils.FreeAndNil(FDeliveryDate);
  SysUtils.FreeAndNil(FUserDeliveryDate);
  inherited Destroy;
end;

procedure DeliveryInfoExt.SetTargetMsid(Index: Integer; const Astring: string);
begin
  FTargetMsid := Astring;
  FTargetMsid_Specified := True;
end;

function DeliveryInfoExt.TargetMsid_Specified(Index: Integer): boolean;
begin
  Result := FTargetMsid_Specified;
end;

procedure DeliveryInfoExt.SetTargetName(Index: Integer; const Astring: string);
begin
  FTargetName := Astring;
  FTargetName_Specified := True;
end;

function DeliveryInfoExt.TargetName_Specified(Index: Integer): boolean;
begin
  Result := FTargetName_Specified;
end;

initialization
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap }
  InvRegistry.RegisterInterface(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'http://mcommunicator.ru/M2M', 'utf-8');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'http://mcommunicator.ru/M2M/%operationName%');
  InvRegistry.RegisterInvokeOptions(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), ioDocument);
  InvRegistry.RegisterInvokeOptions(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), ioSOAP12);
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap.SendMessage }
  InvRegistry.RegisterMethodInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'SendMessage', '',
                                 '[ReturnName="SendMessageResult"]');
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'SendMessage', 'message_', 'message', '');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap.SendMessageAtDate }
  InvRegistry.RegisterMethodInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'SendMessageAtDate', '',
                                 '[ReturnName="SendMessageAtDateResult"]');
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'SendMessageAtDate', 'message_', 'message', '');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap.SendMessageToMultipleSubscribers }
  InvRegistry.RegisterMethodInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'SendMessageToMultipleSubscribers', '',
                                 '[ReturnName="SendMessageToMultipleSubscribersResult"]', IS_OPTN);
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'SendMessageToMultipleSubscribers', 'msids', '',
                                '[ArrayItemName="string"]');
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'SendMessageToMultipleSubscribers', 'message_', 'message', '');
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'SendMessageToMultipleSubscribers', 'SendMessageToMultipleSubscribersResult', '',
                                '[ArrayItemName="SendMessageIDs"]');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap.SendMessageToMultipleSubscribersAtDate }
  InvRegistry.RegisterMethodInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'SendMessageToMultipleSubscribersAtDate', '',
                                 '[ReturnName="SendMessageToMultipleSubscribersAtDateResult"]', IS_OPTN);
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'SendMessageToMultipleSubscribersAtDate', 'msids', '',
                                '[ArrayItemName="string"]');
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'SendMessageToMultipleSubscribersAtDate', 'message_', 'message', '');
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'SendMessageToMultipleSubscribersAtDate', 'SendMessageToMultipleSubscribersAtDateResult', '',
                                '[ArrayItemName="SendMessageIDs"]');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap.SendMessages }
  InvRegistry.RegisterMethodInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'SendMessages', '',
                                 '[ReturnName="SendMessagesResult"]', IS_OPTN);
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'SendMessages', 'msids', '',
                                '[ArrayItemName="string"]');
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'SendMessages', 'message_', 'message', '');
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'SendMessages', 'SendMessagesResult', '',
                                '[ArrayItemName="SendMessageIDs"]');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap.SendMessagesAtDate }
  InvRegistry.RegisterMethodInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'SendMessagesAtDate', '',
                                 '[ReturnName="SendMessagesAtDateResult"]', IS_OPTN);
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'SendMessagesAtDate', 'msids', '',
                                '[ArrayItemName="string"]');
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'SendMessagesAtDate', 'message_', 'message', '');
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'SendMessagesAtDate', 'SendMessagesAtDateResult', '',
                                '[ArrayItemName="SendMessageIDs"]');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap.GetMessageStatus }
  InvRegistry.RegisterMethodInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'GetMessageStatus', '',
                                 '[ReturnName="GetMessageStatusResult"]', IS_OPTN);
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'GetMessageStatus', 'GetMessageStatusResult', '',
                                '[ArrayItemName="DeliveryInfo"]');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap.GetMessagesStatus }
  InvRegistry.RegisterMethodInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'GetMessagesStatus', '',
                                 '[ReturnName="GetMessagesStatusResult"]', IS_OPTN);
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'GetMessagesStatus', 'messageIDs', '',
                                '[ArrayItemName="long"]');
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'GetMessagesStatus', 'GetMessagesStatusResult', '',
                                '[ArrayItemName="MessageStatusWithID"]');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap.GetMessages }
  InvRegistry.RegisterMethodInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'GetMessages', '',
                                 '[ReturnName="GetMessagesResult"]', IS_OPTN);
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'GetMessages', 'subscriberMsids', '',
                                '[ArrayItemName="string"]');
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'GetMessages', 'GetMessagesResult', '',
                                '[ArrayItemName="MessageInfo"]');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap.GetStatistics }
  InvRegistry.RegisterMethodInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'GetStatistics', '',
                                 '[ReturnName="GetStatisticsResult"]', IS_OPTN);
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap.CreateMailingList }
  InvRegistry.RegisterMethodInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'CreateMailingList', '',
                                 '[ReturnName="CreateMailingListResult"]');
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'CreateMailingList', 'name_', 'name', '');
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'CreateMailingList', 'contacts', '',
                                '[ArrayItemName="MailingListContact"]');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap.GetMailingLists }
  InvRegistry.RegisterMethodInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'GetMailingLists', '',
                                 '[ReturnName="GetMailingListsResult"]', IS_OPTN);
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'GetMailingLists', 'GetMailingListsResult', '',
                                '[ArrayItemName="ResultMailingList"]');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap.DeleteMailingLists }
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'DeleteMailingLists', 'mailingListIDs', '',
                                '[ArrayItemName="long"]');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap.GetMailingListContacts }
  InvRegistry.RegisterMethodInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'GetMailingListContacts', '',
                                 '[ReturnName="GetMailingListContactsResult"]', IS_OPTN);
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'GetMailingListContacts', 'GetMailingListContactsResult', '',
                                '[ArrayItemName="MailingListContact"]');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap.SetMailingListContacts }
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'SetMailingListContacts', 'contacts', '',
                                '[ArrayItemName="MailingListContact"]');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap.AddUser }
  InvRegistry.RegisterMethodInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'AddUser', '',
                                 '[ReturnName="AddUserResult"]');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap.GetGroups }
  InvRegistry.RegisterMethodInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'GetGroups', '',
                                 '[ReturnName="GetGroupsResult"]', IS_OPTN);
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'GetGroups', 'GetGroupsResult', '',
                                '[ArrayItemName="SubscriberGroupInfo"]');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap.GetUsers }
  InvRegistry.RegisterMethodInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'GetUsers', '',
                                 '[ReturnName="GetUsersResult"]', IS_OPTN);
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APISoap), 'GetUsers', 'GetUsersResult', '',
                                '[ArrayItemName="UserInfo"]');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet }
  InvRegistry.RegisterInterface(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet), 'http://mcommunicator.ru/M2M', 'utf-8');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet), '');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet.SendMessage }
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet), 'SendMessage', 'message_', 'message', '');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet.SendMessageAtDate }
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet), 'SendMessageAtDate', 'message_', 'message', '');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet.SendMessageToMultipleSubscribers }
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet), 'SendMessageToMultipleSubscribers', 'msids', '',
                                '[ArrayItemName="String"]');
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet), 'SendMessageToMultipleSubscribers', 'message_', 'message', '');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet.SendMessageToMultipleSubscribersAtDate }
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet), 'SendMessageToMultipleSubscribersAtDate', 'msids', '',
                                '[ArrayItemName="String"]');
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet), 'SendMessageToMultipleSubscribersAtDate', 'message_', 'message', '');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet.SendMessages }
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet), 'SendMessages', 'msids', '',
                                '[ArrayItemName="String"]');
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet), 'SendMessages', 'message_', 'message', '');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet.SendMessagesAtDate }
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet), 'SendMessagesAtDate', 'msids', '',
                                '[ArrayItemName="String"]');
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet), 'SendMessagesAtDate', 'message_', 'message', '');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet.GetMessagesStatus }
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet), 'GetMessagesStatus', 'messageIDs', '',
                                '[ArrayItemName="String"]');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet.GetMessages }
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet), 'GetMessages', 'subscriberMsids', '',
                                '[ArrayItemName="String"]');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet.DeleteMailingLists }
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpGet), 'DeleteMailingLists', 'mailingListIDs', '',
                                '[ArrayItemName="String"]');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost }
  InvRegistry.RegisterInterface(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost), 'http://mcommunicator.ru/M2M', 'utf-8');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost), '');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost.SendMessage }
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost), 'SendMessage', 'message_', 'message', '');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost.SendMessageAtDate }
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost), 'SendMessageAtDate', 'message_', 'message', '');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost.SendMessageToMultipleSubscribers }
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost), 'SendMessageToMultipleSubscribers', 'msids', '',
                                '[ArrayItemName="String"]');
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost), 'SendMessageToMultipleSubscribers', 'message_', 'message', '');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost.SendMessageToMultipleSubscribersAtDate }
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost), 'SendMessageToMultipleSubscribersAtDate', 'msids', '',
                                '[ArrayItemName="String"]');
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost), 'SendMessageToMultipleSubscribersAtDate', 'message_', 'message', '');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost.SendMessages }
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost), 'SendMessages', 'msids', '',
                                '[ArrayItemName="String"]');
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost), 'SendMessages', 'message_', 'message', '');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost.SendMessagesAtDate }
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost), 'SendMessagesAtDate', 'msids', '',
                                '[ArrayItemName="String"]');
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost), 'SendMessagesAtDate', 'message_', 'message', '');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost.GetMessagesStatus }
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost), 'GetMessagesStatus', 'messageIDs', '',
                                '[ArrayItemName="String"]');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost.GetMessages }
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost), 'GetMessages', 'subscriberMsids', '',
                                '[ArrayItemName="String"]');
  { MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost.DeleteMailingLists }
  InvRegistry.RegisterParamInfo(TypeInfo(MTS_x0020_Communicator_x0020_M2M_x0020_XML_x0020_APIHttpPost), 'DeleteMailingLists', 'mailingListIDs', '',
                                '[ArrayItemName="String"]');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfSendMessageIDs2), 'http://mcommunicator.ru/M2M', 'ArrayOfSendMessageIDs2', 'ArrayOfSendMessageIDs');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfString), 'http://mcommunicator.ru/M2M', 'ArrayOfString');
  RemClassRegistry.RegisterXSClass(SendMessageIDs, 'http://mcommunicator.ru/M2M', 'SendMessageIDs');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfDeliveryInfo2), 'http://mcommunicator.ru/M2M', 'ArrayOfDeliveryInfo2', 'ArrayOfDeliveryInfo');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfLong), 'http://mcommunicator.ru/M2M', 'ArrayOfLong');
  RemClassRegistry.RegisterXSInfo(TypeInfo(DeliveryStatus), 'http://mcommunicator.ru/M2M', 'DeliveryStatus');
  RemClassRegistry.RegisterXSClass(DeliveryInfo, 'http://mcommunicator.ru/M2M', 'DeliveryInfo');
  RemClassRegistry.RegisterXSInfo(TypeInfo(StringArray), 'http://mcommunicator.ru/M2M/AbstractTypes', 'StringArray');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfSendMessageIDs), 'http://mcommunicator.ru/M2M', 'ArrayOfSendMessageIDs');
  RemClassRegistry.RegisterXSInfo(TypeInfo(long), 'http://mcommunicator.ru/M2M', 'long');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfDeliveryInfo), 'http://mcommunicator.ru/M2M', 'ArrayOfDeliveryInfo');
  RemClassRegistry.RegisterXSClass(ResultMailingList, 'http://mcommunicator.ru/M2M', 'ResultMailingList');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(ResultMailingList), 'Name_', '[ExtName="Name"]');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfSubscriberGroupInfo2), 'http://mcommunicator.ru/M2M', 'ArrayOfSubscriberGroupInfo2', 'ArrayOfSubscriberGroupInfo');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfSubscriberGroupInfo), 'http://mcommunicator.ru/M2M', 'ArrayOfSubscriberGroupInfo');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfResultMailingList2), 'http://mcommunicator.ru/M2M', 'ArrayOfResultMailingList2', 'ArrayOfResultMailingList');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfResultMailingList), 'http://mcommunicator.ru/M2M', 'ArrayOfResultMailingList');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfMailingListAttribute), 'http://mcommunicator.ru/M2M', 'ArrayOfMailingListAttribute');
  RemClassRegistry.RegisterXSClass(MailingListAttribute, 'http://mcommunicator.ru/M2M', 'MailingListAttribute');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(MailingListAttribute), 'Name_', '[ExtName="Name"]');
  RemClassRegistry.RegisterXSClass(SubscriberGroupInfo, 'http://mcommunicator.ru/M2M', 'SubscriberGroupInfo');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(SubscriberGroupInfo), 'Name_', '[ExtName="Name"]');
  RemClassRegistry.RegisterXSInfo(TypeInfo(AccessLevel), 'http://mcommunicator.ru/M2M', 'AccessLevel');
  RemClassRegistry.RegisterXSInfo(TypeInfo(RequestMessageType), 'http://mcommunicator.ru/M2M', 'RequestMessageType');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfUserInfo2), 'http://mcommunicator.ru/M2M', 'ArrayOfUserInfo2', 'ArrayOfUserInfo');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfUserInfo), 'http://mcommunicator.ru/M2M', 'ArrayOfUserInfo');
  RemClassRegistry.RegisterXSClass(UserInfo, 'http://mcommunicator.ru/M2M', 'UserInfo');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(UserInfo), 'Name_', '[ExtName="Name"]');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfDeliveryInfoExt), 'http://mcommunicator.ru/M2M', 'ArrayOfDeliveryInfoExt');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfMessageInfo2), 'http://mcommunicator.ru/M2M', 'ArrayOfMessageInfo2', 'ArrayOfMessageInfo');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfMessageInfo), 'http://mcommunicator.ru/M2M', 'ArrayOfMessageInfo');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfMessageStatusWithID2), 'http://mcommunicator.ru/M2M', 'ArrayOfMessageStatusWithID2', 'ArrayOfMessageStatusWithID');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfMessageStatusWithID), 'http://mcommunicator.ru/M2M', 'ArrayOfMessageStatusWithID');
  RemClassRegistry.RegisterXSClass(MessageStatusWithID, 'http://mcommunicator.ru/M2M', 'MessageStatusWithID');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(MessageStatusWithID), 'Delivery', '[ArrayItemName="DeliveryInfo"]');
  RemClassRegistry.RegisterXSInfo(TypeInfo(MessageType), 'http://mcommunicator.ru/M2M', 'MessageType');
  RemClassRegistry.RegisterXSClass(MessageInfo, 'http://mcommunicator.ru/M2M', 'MessageInfo');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(MessageInfo), 'DeliveryInfo', '[ArrayItemName="DeliveryInfoExt"]');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfMailingListContact2), 'http://mcommunicator.ru/M2M', 'ArrayOfMailingListContact2', 'ArrayOfMailingListContact');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfMailingListContact), 'http://mcommunicator.ru/M2M', 'ArrayOfMailingListContact');
  RemClassRegistry.RegisterXSClass(MailingListContact, 'http://mcommunicator.ru/M2M', 'MailingListContact');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(MailingListContact), 'Name_', '[ExtName="Name"]');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(MailingListContact), 'Attributes', '[ArrayItemName="MailingListAttribute"]');
  RemClassRegistry.RegisterXSClass(DeliveryInfoExt, 'http://mcommunicator.ru/M2M', 'DeliveryInfoExt');
  RemClassRegistry.RegisterXSClass(StatisticsInfo2, 'http://mcommunicator.ru/M2M', 'StatisticsInfo2', 'StatisticsInfo');
  RemClassRegistry.RegisterXSClass(StatisticsInfo, 'http://mcommunicator.ru/M2M', 'StatisticsInfo');

end.