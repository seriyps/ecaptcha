-module(ecaptcha).
-export([pixels/2, gif/2]).
-on_load(init/0).

-define(APPNAME, ecaptcha).
-define(LIBNAME, ecaptcha).

-type opts() :: [line | blur | filter | dots].

-spec pixels(Size :: pos_integer(), opts()) -> {Str :: binary(), Pixels :: binary()}.
pixels(_, _) ->
    not_loaded(?LINE).

-spec gif(Size :: pos_integer(), opts()) -> {Str :: binary(), GifImg :: binary()}.
gif(_, _) ->
    not_loaded(?LINE).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
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
