-module(ecaptcha).

-export([pixels/2, gif/3, png/3]).

-export_type([opts/0, color_name/0, err_reason/0]).

-define(APPNAME, ecaptcha).
-define(LIBNAME, ecaptcha).

-type opts() :: [line | blur | filter | dots].
-type color_name() :: ecaptcha_color:color_name().
-type err_reason() ::
    chars_not_binary
    | wrong_chars_length
    | invalid_character
    | bad_random
    | small_rand_binary
    | opts_not_list
    | non_atom_option
    | unknown_option.

-spec pixels(NumChars :: pos_integer(), opts()) ->
    {Str :: binary(), Pixels :: binary()}
    | {error, err_reason()}.
pixels(NumChars, Opts) ->
    <<CharsRand:NumChars/binary, InnerRand/binary>> = crypto:strong_rand_bytes(
        ecaptcha_nif:rand_size() + NumChars
    ),
    Chars = chars(CharsRand),
    case ecaptcha_nif:pixels(Chars, InnerRand, Opts) of
        {error, _} = Err -> Err;
        Pixels -> {Chars, Pixels}
    end.

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

%% Internal

img(NumChars, Opts, Color, Encoder) ->
    case pixels(NumChars, Opts) of
        {error, _} = Err -> Err;
        {Str, Pixels} -> {Str, Encoder(Pixels, 200, 70, Color)}
    end.

chars(Rand) ->
    Alphabet = alphabet(),
    AlphabetSize = byte_size(Alphabet),
    <<<<(binary:at(Alphabet, C rem AlphabetSize))>> || <<C>> <= Rand>>.

alphabet() ->
    %% XXX: no 'e' and 'g', they are confusing
    <<"abcdfhijklmnopqrstuvwxyz">>.
