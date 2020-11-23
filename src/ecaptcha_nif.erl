%% @private
-module(ecaptcha_nif).

-export([pixels/3]).
-export([rand_size/1]).

-on_load(init/0).

-define(APPNAME, ecaptcha).
-define(LIBNAME, ecaptcha).

-define(NDOTS, 200).
-define(MIN_RAND, 200 + ?NDOTS * 4 + 2).

pixels(_NumChars, _Rand, _Opts) ->
    not_loaded(?LINE).

rand_size(NumChars) ->
    ?MIN_RAND + NumChars.

init() ->
    SoName =
        case code:priv_dir(?APPNAME) of
            {error, bad_name} ->
                case filelib:is_dir(filename:join(["..", priv])) of
                    true ->
                        filename:join(["..", priv, ?LIBNAME]);
                    _ ->
                        filename:join([priv, ?LIBNAME])
                end;
            Dir ->
                filename:join(Dir, ?LIBNAME)
        end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
