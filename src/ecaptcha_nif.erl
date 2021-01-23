%% @private
-module(ecaptcha_nif).

-export([pixels/4]).
-export([rand_size/0]).

-on_load(init/0).

-define(APPNAME, ecaptcha).
-define(LIBNAME, ecaptcha).

-define(NDOTS, 100).
-define(NREVDOTS, 20).
-define(MIN_RAND, 200 + (?NDOTS + ?NREVDOTS) * 4 + 3).

pixels(_Font, _Chars, _Rand, _Effects) ->
    not_loaded(?LINE).

rand_size() ->
    ?MIN_RAND.

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
