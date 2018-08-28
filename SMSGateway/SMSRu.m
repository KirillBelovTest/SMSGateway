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


smsruRequest[method_String, parmeters: {(_String -> _).. }, options: {(_String -> _)... }] := 
	Module[{
		url, request, response, code, body
	}, 
		url = {apiUrl, "sms", method};
		request = URLBuild[url, Join[parmeters, DeleteCases[options, _[_, Automatic]]]];
		response = URLRead[request];
		code = response["StatusCode"];
		body = response["Body"];
		
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
(*end private context*)


End[]


(* ::Section:: *)
(*from change protection*)


Protect["`*"]


(* ::Section:: *)
(*end package*)


EndPackage[]
