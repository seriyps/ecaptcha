-module(ecaptcha).

-export([pixels/2, gif/2]).

-on_load(init/0).

-define(APPNAME, ecaptcha).
-define(LIBNAME, ecaptcha).

-define(NDOTS, 200).
-define(MIN_RAND, 200 + ?NDOTS * 4 + 2).

-type opts() :: [line | blur | filter | dots].
-type err_reason() ::
    length_not_integer
    | invalid_num_chars
    | bad_random
    | small_rand_binary
    | opts_not_list
    | non_atom_opt
    | unknown_option.

-spec pixels(NumChars :: pos_integer(), opts()) ->
    {Str :: binary(), Pixels :: binary()}
    | {error, err_reason()}.
pixels(NumChars, Opts) ->
    pixels_nif(NumChars, crypto:strong_rand_bytes(?MIN_RAND + NumChars), Opts).

-spec gif(NumChars :: pos_integer(), opts()) ->
    {Str :: binary(), GifImg :: binary()}
    | {error, err_reason()}.
gif(NumChars, Opts) ->
    gif_nif(NumChars, crypto:strong_rand_bytes(?MIN_RAND + NumChars), Opts).

%% Internal

pixels_nif(_NumChars, _Rand, _Opts) ->
    not_loaded(?LINE).

gif_nif(_NumChars, _Rand, _Opts) ->
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
