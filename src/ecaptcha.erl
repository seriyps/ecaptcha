-module(ecaptcha).

-export([pixels/2, gif/3]).
-export([pixels_as_gif/2]).

-export_type([opts/0, color/0, err_reason/0]).

-on_load(init/0).

-define(APPNAME, ecaptcha).
-define(LIBNAME, ecaptcha).

-define(NDOTS, 200).
-define(MIN_RAND, 200 + ?NDOTS * 4 + 2).

-type opts() :: [line | blur | filter | dots].
-type color() :: black | red | orange | blue | pink | blue.
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

-spec gif(NumChars :: pos_integer(), opts(), color()) ->
    {Str :: binary(), GifImg :: binary()}
    | {error, err_reason()}.
gif(NumChars, Opts, Color) ->
    gif_nif(NumChars, crypto:strong_rand_bytes(?MIN_RAND + NumChars), Opts, color_idx(Color)).

-spec pixels_as_gif(binary(), color()) ->
    binary() | {error, bad_image | wrong_pixels_size | invalid_color}.
pixels_as_gif(PixelData, Color) ->
    pixels_as_gif_nif(PixelData, color_idx(Color)).

%% Internal

color_idx(black) -> 0;
color_idx(red) -> 1;
color_idx(orange) -> 2;
color_idx(blue) -> 3;
color_idx(pink) -> 4;
color_idx(purple) -> 5.

pixels_nif(_NumChars, _Rand, _Opts) ->
    not_loaded(?LINE).

gif_nif(_NumChars, _Rand, _Opts, _ColorIdx) ->
    not_loaded(?LINE).

pixels_as_gif_nif(_PixelData, _ColorIdx) ->
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
