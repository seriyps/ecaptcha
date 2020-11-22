%% @private
-module(ecaptcha_nif).

-export([pixels/3, gif/4, pixels_as_gif/2]).

-on_load(init/0).

-define(APPNAME, ecaptcha).
-define(LIBNAME, ecaptcha).

pixels(_NumChars, _Rand, _Opts) ->
    not_loaded(?LINE).

gif(_NumChars, _Rand, _Opts, _ColorIdx) ->
    not_loaded(?LINE).

pixels_as_gif(_PixelData, _ColorIdx) ->
    not_loaded(?LINE).

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
