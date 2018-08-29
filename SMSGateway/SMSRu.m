(* ::Package:: *)


(* ::Title:: *)
(*SMSRu*)


(* ::Section:: *)
(*begin package*)


BeginPackage["SMSGateway`SMSRu`"]


(* ::Section:: *)
(*clear*)


Unprotect["`*"]
ClearAll["`*"]


(* ::Section:: *)
(*public names declaration*)


SMSRuSend::usage = 
"SMSRuSend[apiID, to, msg]
SMSRuSend[{login, password}, to, msg]
SMSRuSend[auth, {to1, to2, ..}, msg]
SMSRuSend[auth, {to1, to2, ..}, {msg1, msg2, ..}]
SMSRuSend[auth, to, msg, options]"


SMSRuStatus::usage = 
"SMSRuStatus[apiID, smsID]
SMSRuStatus[{login, password}, smsID]
SMSRuStatus[auth, {smsID1, smsID2, ..}]
SMSRuStatus[auth, smsID, options]"


SMSRuCost::usage = 
"SMSRuCost[apiID, to, msg]
SMSRuCost[{login, password}, to, msg]
SMSRuCost[auth, {to1, to2, ..}, msg]
SMSRuCost[auth, {to1, to2, ..}, {msg1, msg2, ..}]
SMSRuCost[auth, to, msg, options]"


SMSRuMyBalance::usage = 
"SMSRuMyBalance[apiID]
SMSRuMyBalance[{login, password}]
SMSRuMyBalance[auth, options]"


SMSRuMyLimit::usage = 
"SMSRuMyLimit[apiID]
SMSRuMyLimit[{login, password}]
SMSRuMyLimit[auth, options]"


SMSRuMyFree::usage = 
"SMSRuMyFree[apiID]
SMSRuMyFree[{login, password}]
SMSRuMyFree[auth, options]"


SMSRuMySenders::usage = 
"SMSRuMySenders[apiID]
SMSRuMySenders[{login, password}]
SMSRuMySenders[auth, options]"


SMSRuAuthCheck::usage = 
"SMSRuAuthCheck[apiID]
SMSRuAuthCheck[{login, password}]
SMSRuAuthCheck[auth, options]"


SMSRuStoplistAdd::usage = 
"SMSRuStoplistAdd[apiID, phone, text]
SMSRuStoplistAdd[{login, password}, phone, text]
SMSRuStoplistAdd[auth, phone, text, options]"


SMSRuStoplistDel::usage = 
"SMSRuStoplistDel[apiID, phone]
SMSRuStoplistDel[{login, password}, phone]
SMSRuStoplistDel[auth, phone, options]"


SMSRuStoplistGet::usage = 
"SMSRuStoplistGet[apiID]
SMSRuStoplistGet[{login, password}]
SMSRuStoplistGet[auth, options]"


(* ::Section:: *)
(*begin private context*)


Begin["`Private`"]


(* ::Section:: *)
(*internal functions and variables*)


authPattern[] := 
	_String | {_String, _String}


authParams[apiID_String] := 
	{"api_id" -> apiID}


authParams[{login_String, password_String}] := 
	{"login" -> login, "password" -> password}


optionValues[symbol_Symbol, options_?OptionQ] := 
	Table[o -> OptionValue[symbol, options, o], {o, Options[symbol][[All, 1]]}]


apiUrl = "https://sms.ru"


smsruRequest[method: _String | PatternSequence[_String, _String], parmeters: {(_String -> _).. }, options: {(_String -> _)... }] := 
	Module[{
		url, request, response, code, body
	}, 
		url = {apiUrl, "sms", method};
		request = URLBuild[url, Join[parmeters, DeleteCases[options, _[_, Automatic]]]];
		response = URLRead[request];
		code = response["StatusCode"];
		body = ExportString[response["Body"], "Text"];
		
		Switch["json" /. options, 
			1, ImportString[body, "JSON"], 
			_, ImportString[body, "Table"]
		]
	]


(* ::Section:: *)
(*send*)


(*https://sms.ru/api/send*)


Options[SMSRuSend] = 
	{
		"json" -> 1, 
		"from" -> Automatic, 
		"time" -> Automatic, 
		"translit" -> Automatic, 
		"test" -> Automatic, 
		"partner_id" -> Automatic
	}


SyntaxInformation[SMSRuSend] = 
	{"ArgumentsPattern" -> {_, _, _, OptionsPattern[]}}


(*send[auth, to, msg]*)
SMSRuSend[auth: authPattern[], to: _String | {__String}, msg_String, options: OptionsPattern[]] := 
	smsruRequest["send", Join[authParams[auth], {"to" -> StringRiffle[Flatten[{to}], ","], "msg" -> msg}], optionValues[SMSRuSend, Flatten[{options}]]]


(*send[auth, {to1, to2, ..}, {msg1, msg2m ..}]*)
SMSRuSend[auth: authPattern[], toList: {__String}, msgList: {__String}, options: OptionsPattern[]] /; 
	Length[toList] === Length[msgList] := 
	smsruRequest["send", Join[authParams[auth], MapThread["to[" <> #1 <> "]" -> #2&, {toList, msgList}]], optionValues[SMSRuSend, Flatten[{options}]]]


(* ::Section:: *)
(*status*)


(*https://sms.ru/api/cost*)


Options[SMSRuStatus] = 
	{"json" -> 1}


SyntaxInformation[SMSRuStatus] = 
	{"ArgumentsPattern" -> {_, _, OptionsPattern[]}}


(*status[auth, smsIDs]*)
SMSRuStatus[auth: authPattern[], smsID: _String | {__String}, options: OptionsPattern[]] := 
	smsruRequest["status", Join[authParams[auth], {"sms_id" -> StringRiffle[Flatten[{smsID}], ","]}], optionValues[SMSRuStatus, Flatten[{options}]]]


(* ::Section:: *)
(*cost*)


(*https://sms.ru/api/cost*)


Options[SMSRuCost] = 
	{
		"json" -> 1, 
		"from" -> Automatic, 
		"time" -> Automatic, 
		"translit" -> Automatic, 
		"partner_id" -> Automatic
	}


SyntaxInformation[SMSRuCost] = 
	{"ArgumentsPattern" -> {_, _, _, OptionsPattern[]}}


(*cost[auth, to, msg]*)
SMSRuCost[auth: authPattern[], to: _String | {__String}, msg_String, options: OptionsPattern[]] := 
	smsruRequest["cost", Join[authParams[auth], {"to" -> StringRiffle[Flatten[{to}], ","], "msg" -> msg}], optionValues[SMSRuCost, Flatten[{options}]]]


(*cost[auth, {to1, to2, ..}, {msg1, msg2m ..}]*)
SMSRuCost[auth: authPattern[], toList: {__String}, msgList: {__String}, options: OptionsPattern[]] /; 
	Length[toList] === Length[msgList] := 
	smsruRequest["cost", Join[authParams[auth], MapThread["to[" <> #1 <> "]" -> #2&, {toList, msgList}]], optionValues[SMSRuCost, Flatten[{options}]]]


(* ::Section:: *)
(*mybalance*)


(*https://sms.ru/api/balance*)


Options[SMSRuMyBalance] = 
	{"json" -> 1}


SyntaxInformation[SMSRuMyBalance] = 
	{"ArgumentsPattern" -> {_, OptionsPattern[]}}


(*mybalance[auth]*)
SMSRuMyBalance[auth: authPattern[], options: OptionsPattern[]] := 
	smsruRequest["my", "balance", authParams[auth], optionValues[SMSRuMyBalance, Flatten[{options}]]]


(* ::Section:: *)
(*mylimit*)


(*https://sms.ru/api/limit*)


Options[SMSRuMyLimit] = 
	{"json" -> 1}


SyntaxInformation[SMSRuMyLimit] = 
	{"ArgumentsPattern" -> {_, OptionsPattern[]}}


(*mylimit[auth]*)
SMSRuMyLimit[auth: authPattern[], options: OptionsPattern[]] := 
	smsruRequest["my", "limit", authParams[auth], optionValues[SMSRuMyLimit, Flatten[{options}]]]


(* ::Section:: *)
(*myfree*)


(*https://sms.ru/api/my_free*)


Options[SMSRuMyFree] = 
	{"json" -> 1}


SyntaxInformation[SMSRuMyFree] = 
	{"ArgumentsPattern" -> {_, OptionsPattern[]}}


(*myfree[auth]*)
SMSRuMyFree[auth: authPattern[], options: OptionsPattern[]] := 
	smsruRequest["my", "free", authParams[auth], optionValues[SMSRuMyFree, Flatten[{options}]]]


(* ::Section:: *)
(*mysenders*)


(*https://sms.ru/api/senders*)


Options[SMSRuMySenders] = 
	{"json" -> 1}


SyntaxInformation[SMSRuMySenders] = 
	{"ArgumentsPattern" -> {_, OptionsPattern[]}}


(*mysenders[auth]*)
SMSRuMySenders[auth: authPattern[], options: OptionsPattern[]] := 
	smsruRequest["my", "senders", authParams[auth], optionValues[SMSRuMySenders, Flatten[{options}]]]


(* ::Section:: *)
(*authcheck*)


(*https://sms.ru/api/auth_check*)


Options[SMSRuMySenders] = 
	{"json" -> 1}


SyntaxInformation[SMSRuAuthCheck] = 
	{"ArgumentsPattern" -> {_, OptionsPattern[]}}


(*authcheck[auth]*)
SMSRuAuthCheck[auth: authPattern[], options: OptionsPattern[]] := 
	smsruRequest["auth", "check", authParams[auth], optionValues[SMSRuAuthCheck, Flatten[{options}]]]


(* ::Section:: *)
(*stoplistadd*)


(*https://sms.ru/api/stoplist_add*)


Options[SMSRuStoplistAdd] = 
	{"json" -> 1}


SyntaxInformation[SMSRuStoplistAdd] = 
	{"ArgumentsPattern" -> {_, _, _, OptionsPattern[]}}


(*stoplistadd[auth, phone, text]*)
SMSRuStoplistAdd[auth: authPattern[], phone_String, text_String, options: OptionsPattern[]] := 
	smsruRequest["stoplist", "add", Join[authParams[auth], {"stoplist_phone" -> phone, "stoplist_text" -> text}], optionValues[SMSRuStoplistAdd, Flatten[{options}]]]


(* ::Section:: *)
(*stoplistdel*)


(*https://sms.ru/api/stoplist_del*)


Options[SMSRuStoplistDel] = 
	{"json" -> 1}


SyntaxInformation[SMSRuStoplistDel] = 
	{"ArgumentsPattern" -> {_, _, OptionsPattern[]}}


(*stoplistdel[auth, phone]*)
SMSRuStoplistDel[auth: authPattern[], phone_String, options: OptionsPattern[]] := 
	smsruRequest["stoplist", "del", Join[authParams[auth], {"stoplist_phone" -> phone}], optionValues[SMSRuStoplistDel, Flatten[{options}]]]


(* ::Section:: *)
(*stoplistget*)


(*https://sms.ru/api/stoplist_get*)


Options[SMSRuStoplistGet] = 
	{"json" -> 1}


SyntaxInformation[SMSRuStoplistGet] = 
	{"ArgumentsPattern" -> {_, OptionsPattern[]}}


(*stoplistget[auth]*)
SMSRuStoplistGet[auth: authPattern[], options: OptionsPattern[]] := 
	smsruRequest["stoplist", "get", authParams[auth], optionValues[SMSRuStoplistGet, Flatten[{options}]]]


(* ::Section:: *)
(*end private context*)


End[]


(* ::Section:: *)
(*from change protection*)


Protect["`*"]


(* ::Section:: *)
(*end package*)


EndPackage[]
