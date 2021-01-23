%% @doc Main interface for Erlang captcha library
-module(ecaptcha).

-export([pixels/2, gif/2, png/2]).

-export_type([opts/0, effects/0, color_name/0, color_rgb/0, font_name/0, alphabet/0, err_reason/0]).

-type opts() :: #{
    color => color_name() | color_rgb(),
    effects => effects(),
    font => font_name(),
    alphabet => alphabet()
}.

%% `color' - what color to use for the gif/png image. predefined color name or RGB tuple;
%%           default: `black'
%% `effects' - list of additional effects to apply to the text rendering; default: `[]'
%% `font' - name of the font to use; default: `<<"hplhs-oldstyle">>'
%% `alphabet' - set of characters to use to generate random string. One of predefined sets or
%% binary string with all the allowed characters (as long as they are defined in selected font);
%% default: latin_lowercase

-type effects() :: [line | blur | filter | dots | reverse_dots].
%% `line' - draws a curved horisontal line on top of the text
%% `blur' - blurs the image (averages each pixel's color with it's neighbours)
%% `filter' - makes letters hollow
%% `dots' - draws 100 random 2x2 white dots on top of the image, effectively removing small pathes
%% from it
%% `reverse_dots' - draws 20 random dots of a randomized size from 1 to 3px using a color opposite
%% to the current color (so, reversing the color - black becomes white, white becomes black)

-type color_name() :: ecaptcha_color:color_name().
-type color_rgb() :: ecaptcha_color:rgb().
-type font_name() :: binary().
-type alphabet() :: numbers | latin_lowercase | latin_uppercase | binary().
-type err_reason() ::
    font_name_not_binary
    | font_not_found
    | chars_not_binary
    | wrong_chars_length
    | character_out_of_alphabet_range
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
%% @param Opts map of additional options such as `font', `alphabet' and `effects'
-spec pixels(NumChars :: pos_integer(), opts()) ->
    {Str :: binary(), Pixels :: binary()}
    | {error, err_reason()}.
pixels(NumChars, Opts) ->
    <<CharsRand:NumChars/binary, InnerRand/binary>> = crypto:strong_rand_bytes(
        ecaptcha_nif:rand_size() + NumChars
    ),
    Chars = chars(CharsRand, maps:get(alphabet, Opts, latin_lowercase)),
    Font = maps:get(font, Opts, <<"hplhs-oldstyle">>),
    Effects = maps:get(effects, Opts, []),
    case ecaptcha_nif:pixels(Font, Chars, InnerRand, Effects) of
        {error, _} = Err -> Err;
        Pixels -> {Chars, Pixels}
    end.

%% @doc Generate GIF image 200x70 with NumChars letters on it
%%
%% Same as {@link png/3}, but image is in GIF format.
-spec gif(NumChars :: pos_integer(), opts()) ->
    {Str :: binary(), GifImg :: binary()}
    | {error, err_reason()}.
gif(NumChars, Opts) ->
    img(NumChars, Opts, fun ecaptcha_gif:encode/4).

%% @doc Generate PNG image 200x70 with NumChars letters on it
%%
%% It returns a 2-tuple where 1st element is the binary containing of `NumChars' size containing
%% ASCII string that is printed on the image and 2nd element is PNG-encoded image that can be, eg
%% sent directly to the browser with `Content-Type: image/png'.
%% @param NumChars - same as in {@link pixels/2}
%% @param Opts - same as in {@link pixels/2}, but also includes `color'. See {@link opts()}.
-spec png(NumChars :: pos_integer(), opts()) ->
    {Str :: binary(), PngImg :: binary()}
    | {error, err_reason()}.
png(NumChars, Opts) ->
    img(NumChars, Opts, fun ecaptcha_png:encode/4).

%% Internal

img(NumChars, Opts, Encoder) ->
    case pixels(NumChars, Opts) of
        {error, _} = Err ->
            Err;
        {Str, Pixels} ->
            Color = maps:get(color, Opts, black),
            {Str, Encoder(Pixels, 200, 70, Color)}
    end.

chars(Rand, AlphabetSelector) ->
    Alphabet = alphabet(AlphabetSelector),
    AlphabetSize = byte_size(Alphabet),
    <<<<(binary:at(Alphabet, C rem AlphabetSize))>> || <<C>> <= Rand>>.

alphabet(numbers) ->
    <<"0123456789">>;
alphabet(latin_lowercase) ->
    <<"abcdefghijklmnopqrstuvwxyz">>;
alphabet(latin_uppercase) ->
    <<"ABCDEFGHIJKLMNOPQRSTUVWXYZ">>;
alphabet(Alphabet) when is_binary(Alphabet), byte_size(Alphabet) > 3 ->
    Alphabet.
