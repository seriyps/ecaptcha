-module(ecaptcha).

-export([pixels/2, gif/3, png/3]).

-export_type([opts/0, color_name/0, err_reason/0]).

-define(APPNAME, ecaptcha).
-define(LIBNAME, ecaptcha).


-type opts() :: [line | blur | filter | dots].
-type color_name() :: ecaptcha_color:color_name().
-type err_reason() ::
    length_not_integer
    | invalid_num_chars
    | bad_random
    | small_rand_binary
    | opts_not_list
    | non_atom_option
    | unknown_option.

-spec pixels(NumChars :: pos_integer(), opts()) ->
    {Str :: binary(), Pixels :: binary()}
    | {error, err_reason()}.
pixels(NumChars, Opts) ->
    Rand = crypto:strong_rand_bytes(ecaptcha_nif:rand_size(NumChars)),
    ecaptcha_nif:pixels(NumChars, Rand, Opts).

-spec gif(NumChars :: pos_integer(), opts(), color_name()) ->
    {Str :: binary(), GifImg :: binary()}
    | {error, err_reason()}.
gif(NumChars, Opts, Color) ->
    img(NumChars, Opts, Color, fun ecaptcha_gif:encode/4).

-spec png(NumChars :: pos_integer(), opts(), color_name()) ->
    {Str :: binary(), GifImg :: binary()}
    | {error, err_reason()}.
png(NumChars, Opts, Color) ->
    img(NumChars, Opts, Color, fun ecaptcha_png:encode/4).

img(NumChars, Opts, Color, Encoder) ->
    case pixels(NumChars, Opts) of
        {error, _} = Err -> Err;
        {Str, Pixels} -> {Str, Encoder(Pixels, 200, 70, Color)}
    end.
