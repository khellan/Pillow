%% @author author <daniel.kwiecinski@lambder.com>
%% @copyright Daniel Kwiecinski.
%% @doc Static webmachine resource.

-module(static_resource).
-export([init/1, allowed_methods/2,
         content_types_provided/2, resource_exists/2, last_modified/2, provide_content/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("kernel/include/file.hrl").
-record(context, {docroot,fullpath,fileinfo}).

init(DocRoot) ->
    {ok, #context{docroot=DocRoot}}.

resource_exists(ReqData, Context) ->
  case get_full_path(Context#context.docroot, wrq:disp_path(ReqData)) of
    undefined -> {false, ReqData, Context};
    Path ->
      case filelib:is_regular(Path) of
        true ->
          case file:read_file_info(Path) of
            {ok, FileInfo} ->
              {true, ReqData, Context#context{fileinfo=FileInfo}};
            {error, _} ->
              {false, ReqData, Context}
          end;
        _ -> {false, ReqData, Context}
      end
  end.

content_types_provided(ReqData, Context) ->
    Path = get_full_path(Context#context.docroot, wrq:disp_path(ReqData)),
    {[{webmachine_util:guess_mime(Path), provide_content}], ReqData, Context#context{fullpath=Path}}.

allowed_methods(ReqData, Context) -> {['HEAD', 'GET'], ReqData, Context}.

last_modified(ReqData, Context) ->
  {(Context#context.fileinfo)#file_info.mtime, ReqData, Context}.

provide_content(ReqData, Context) ->
  {ok, Value} = file:read_file(Context#context.fullpath),
  {Value, ReqData, Context}.
% ------------------ PRIVATE ------------------------


get_full_path(DocRoot, Path) ->
   case mochiweb_util:safe_relative_path(Path) of
     undefined -> undefined;
     RelPath ->
      FullPath = filename:join([DocRoot, RelPath]),
      case filelib:is_dir(FullPath) of
        true ->
          filename:join([FullPath, "index.html"]);
        false ->
          FullPath
      end
    end.
