:- module(deepl, [translate/3]).

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

% Define your DeepL API authentication key
auth_key(Key):
  open('deepl_authkey', read, Str),
  read_string(Str, _, Key),
  close(Str).

% Define the endpoint URL for the free DeepL API
deepl_endpoint('https://api-free.deepl.com/v2/translate').
% deepl_endpoint([
%                     host('api-free.deepl.com'),
%                     path('/v2/translate'),
%                     scheme(https)
%                 ]).

% Define the parameters for the translation request
translate(TargetLang, Text, Result) :-
    Field = 'text',
    deepl_endpoint(Endpoint),
    auth_key(AuthKey),
    string_concat('text=', Text, String_),
    string_concat(String_, '&target_lang=', String_1),
    string_concat(String_1, TargetLang, Data),    
    http_open(Endpoint,Response,
      [method(post), post(atom(Data)), request_header('Content-Type'=application/x-www-form-urlencoded), request_header('Authorization'=AuthKey)]),
    json_read_dict(Response, Result_),
    Translations = Result_.get(translations),
    Translations = [Res|_],
    Result = Res.get(text).