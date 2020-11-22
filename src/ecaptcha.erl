-module(ecaptcha).

-export([pixels/2, gif/3, png/3]).
-export([pixels_as_gif/2]).

-export_type([opts/0, color_name/0, err_reason/0]).

-define(APPNAME, ecaptcha).
-define(LIBNAME, ecaptcha).

-define(NDOTS, 200).
-define(MIN_RAND, 200 + ?NDOTS * 4 + 2).

-type opts() :: [line | blur | filter | dots].
-type color_name() :: ecaptcha_color:color_name().
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
    ecaptcha_nif:pixels(NumChars, crypto:strong_rand_bytes(?MIN_RAND + NumChars), Opts).

-spec gif(NumChars :: pos_integer(), opts(), color_name()) ->
    {Str :: binary(), GifImg :: binary()}
    | {error, err_reason()}.
gif(NumChars, Opts, Color) ->
    ecaptcha_nif:gif(NumChars, crypto:strong_rand_bytes(?MIN_RAND + NumChars), Opts,
                         color_idx(Color)).

-spec png(NumChars :: pos_integer(), opts(), color_name()) ->
    {Str :: binary(), GifImg :: binary()}
    | {error, err_reason()}.
png(NumChars, Opts, Color) ->
    case pixels(NumChars, Opts) of
        {error, _} = Err -> Err;
        {Str, Pixels} -> {Str, ecaptcha_png:encode(Pixels, 200, 70, Color)}
    end.

-spec pixels_as_gif(binary(), color_name()) ->
    binary() | {error, bad_image | wrong_pixels_size | invalid_color}.
pixels_as_gif(PixelData, Color) ->
    ecaptcha_nif:pixels_as_gif(PixelData, color_idx(Color)).

%% Internal

color_idx(black) -> 0;
color_idx(red) -> 1;
color_idx(orange) -> 2;
color_idx(blue) -> 3;
color_idx(pink) -> 4;
color_idx(purple) -> 5.
