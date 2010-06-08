%%%---------------------------------------------------------------------
%%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%%% use this file except in compliance with the License. You may obtain a copy of
%%% the License at
%%%
%%% http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%%% License for the specific language governing permissions and limitations under
%%% the License.
%%%---------------------------------------------------------------------

-module(etap_t_003).
-export([start/0]).

-include("etap.hrl").

%%--------------------------------------------------------------------
%% Tests the pillow_couch module
%%--------------------------------------------------------------------
start() ->
    etap:plan(12),
    test_content_types_provided(),
    test_content_types_accepted(),
    test_allowed_methods(),
    etap:end_tests().

test_content_types_provided() ->
    Response = pillow_couch:content_types_provided(req_data, context),
    {ContentTypes, req_data, context} = Response,
    ?etap_match(lists:keysearch("text/html", 1, ContentTypes), {value,{"text/html", _}}, "HTML content is supported"),
    ?etap_match(lists:keysearch("application/xml", 1, ContentTypes), {value,{"application/xml", _}}, "XML content is supported"),
    ?etap_match(lists:keysearch("application/json", 1, ContentTypes), {value,{"application/json", _}}, "JSON content is supported"),
    ?etap_match(lists:keysearch("text/plain", 1, ContentTypes), {value,{"text/plain", _}}, "Plain text content is supported"),
    ok.

test_content_types_accepted() ->
    Response = pillow_couch:content_types_accepted(req_data, context),
    {ContentTypes, req_data, context} = Response,
    ?etap_match(
        lists:keysearch("application/x-www-form-urlencoded", 1, ContentTypes),
        {value,{"application/x-www-form-urlencoded", _}},
        "Standard web forms are supported"),
    ?etap_match(lists:keysearch("application/json", 1, ContentTypes), {value,{"application/json", _}}, "JSON forms supported"),
    ?etap_match(lists:keysearch("text/plain", 1, ContentTypes), {value,{"text/plain", _}}, "Plain text forms are supported"),
    ok.

test_allowed_methods() ->
    Response = pillow_couch:allowed_methods(req_data, context),
    {Methods, req_data, context} = Response,
    etap:ok(lists:any(fun(Element) -> Element == 'PUT' end, Methods), "Verifies that HTTP PUT requests are supported"),
    etap:ok(lists:any(fun(Element) -> Element == 'GET' end, Methods), "Verifies that HTTP GET requests are supported"),
    etap:ok(lists:any(fun(Element) -> Element == 'POST' end, Methods), "Verifies that HTTP POST requests are supported"),
    etap:ok(lists:any(fun(Element) -> Element == 'DELETE' end, Methods), "Verifies that HTTP DELETE requests are supported"),
    ok.
