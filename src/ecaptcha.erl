%% @doc Main interface for Erlang captcha library
-module(ecaptcha).

-export([pixels/2, gif/3, png/3]).

-export_type([opts/0, color_name/0, color_rgb/0, err_reason/0]).

-type opts() :: [line | blur | filter | dots | reverse_dots].
%% `line' - draws a curved horisontal line on top of the text
%% `blur' - blurs the image (averages each pixel's color with it's neighbours)
%% `filter' - makes letters hollow
%% `dots' - draws 100 random 2x2 white dots on top of the image, effectively removing small pathes
%% from it
%% `reverse_dots' - draws 20 random dots of a randomized size from 1 to 3px using a color opposite
%% to the current color (so, reversing the color - black becomes white, white becomes black)

-type color_name() :: ecaptcha_color:color_name().
-type color_rgb() :: ecaptcha_color:rgb().
-type err_reason() ::
    chars_not_binary
    | wrong_chars_length
    | invalid_character
    | bad_random
    | small_rand_binary
    | opts_not_list
    | non_atom_option
    | unknown_option.

%% @doc Generate greyscale array of pixels 200x70
%%
%% It returns a tuple where 1st element is a binary ASCII string which contains characters that are
%% printed on captcha image.
%% 2nd element is a plain binary that contains 200x70 = 14000 "pixels", 1 byte each, where
%% 0 means black and 255 - white, intermediate values are shades of grey.
%%
%% @param NumChars how many characters should be on a image, `1..6'
%% @param Opts list of additional effects to apply to the image
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

%% @doc Generate GIF image 200x70 with NumChars letters on it
%%
%% Same as {@link png/3}, but image is in GIF format.
-spec gif(NumChars :: pos_integer(), opts(), color_name() | color_rgb()) ->
    {Str :: binary(), GifImg :: binary()}
    | {error, err_reason()}.
gif(NumChars, Opts, Color) ->
    img(NumChars, Opts, Color, fun ecaptcha_gif:encode/4).

%% @doc Generate PNG image 200x70 with NumChars letters on it
%%
%% It returns a 2-tuple where 1st element is the binary containing of `NumChars' size containing
%% ASCII string that is printed on the image and 2nd element is PNG-encoded image that can be, eg
%% sent directly to the browser with `Content-Type: image/png'.
%% @param NumChars - same as in {@link pixels/2}
%% @param Opts - same as in {@link pixels/2}
%% @param Color - use shades of this color instead of shades of grey. Can be one from the set of
%%     recommended named colors or `{Red, Green, Blue}' RGB tuple
-spec png(NumChars :: pos_integer(), opts(), color_name() | color_rgb()) ->
    {Str :: binary(), PngImg :: binary()}
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
