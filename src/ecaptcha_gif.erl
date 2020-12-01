%% @doc Basic GIF encoder
%%
%% [https://www.w3.org/Graphics/GIF/spec-gif89a.txt]
%% Originally based on [https://github.com/huacnlee/rucaptcha/tree/master/ext/rucaptcha]
-module(ecaptcha_gif).

-export([encode/4]).

%% For tests
-export([min_bits_to_fit/1]).

-spec encode(
    binary(),
    pos_integer(),
    pos_integer(),
    ecaptcha:color_name() | ecaptcha:color_rgb()
) -> iodata().
encode(Pixels, Width, Height, ColorName) when is_atom(ColorName) ->
    encode(Pixels, Width, Height, ecaptcha_color:by_name(ColorName));
encode(Pixels, Width, Height, Color) when byte_size(Pixels) =:= (Width * Height) ->
    Palette = ecaptcha_color:new_palette(Pixels, Color),
    [
        header(Width, Height, Palette),
        encode_raster_data(Pixels, Palette),
        trailer()
    ].

%% Header

header(Width, Height, Palette) ->
    NumColors = ecaptcha_color:palette_size(Palette),
    [
        screen_descriptor(Width, Height, NumColors),
        palette(Palette),
        image_descriptor(Width, Height)
    ].

%% erlfmt-ignore
screen_descriptor(Width, Height, PaletteSize) ->
    Bits = min_bits_to_fit(PaletteSize) - 1,
    <<
      "GIF89a",
      Width:16/little,                          % Logical screen width
      Height:16/little,                         % Logical screen height
      1:1,                                      % GCTFlag:1 - 1 - Global Color Table exists
      Bits:3,                                   % ColorResolution:3
      0:1,                                      % SortFlag:1 - 0 - not sorted
      Bits:3,                                   % GCTSize:3 - `2^(3+1)' - size of GCT
      0,                                        % BackgroundColor index in palette
      0                                         % Aspect ratio
    >>.

palette(Palette) ->
    %% Palette size must be power of 2. If number of colors is not exactly power of 2, we fill
    %% extra space with dumy color (white).
    NumColors = ecaptcha_color:palette_size(Palette),
    ColorsRGB = ecaptcha_color:palette_colors_by_frequency(Palette),
    PaletteLenBits = min_bits_to_fit(NumColors),
    % 2 ^ PaletteLenBits
    PaletteSize = 1 bsl PaletteLenBits,
    NumDummyColors = PaletteSize - NumColors,
    White = ecaptcha_color:by_name(white),
    PaletteAndDummy = ColorsRGB ++ lists:duplicate(NumDummyColors, White),
    lists:map(fun ecaptcha_color:bin_3b/1, PaletteAndDummy).

min_bits_to_fit(Size) ->
    if
        Size =< 2 -> 1;
        Size =< 4 -> 2;
        Size =< 8 -> 3;
        Size =< 16 -> 4;
        Size =< 32 -> 5;
        Size =< 64 -> 6;
        Size =< 128 -> 7;
        Size =< 256 -> 8;
        true -> error(size_overflow)
    end.

%% erlfmt-ignore
image_descriptor(Width, Height) ->
    <<
      ",",
      0:16/little, 0:16/little,                 % (x0, y0) - start of image
      Width:16/little, Height:16/little,        % (xN, yN) - end of image
      0:1,                                      % no local color table
      0:1,                                      % sequential order (not interlaced)
      0:3,                                      % reserved
      0:3                                       % Local color table size
    >>.

%% Body

encode_raster_data(Pixels, Palette) ->
    PaletteSize = ecaptcha_color:palette_size(Palette),
    PalettePixels = map_to_palette(Pixels, Palette),
    CodeSize = max(min_bits_to_fit(PaletteSize), 2),
    Compressed = ecaptcha_gif_lzw:compress(PalettePixels, CodeSize),
    % LZW minimum code size
    [CodeSize | encode_chunks(Compressed)].

map_to_palette(Pixels, Palette) ->
    <<
        <<(ecaptcha_color:palette_get_index(Pixel, Palette))>>
        || <<Pixel>> <= Pixels
    >>.

encode_chunks(<<Chunk:255/binary, Tail/binary>>) ->
    [255, Chunk | encode_chunks(Tail)];
encode_chunks(<<>>) ->
    %Stream terminator
    [0];
encode_chunks(Remaining) ->
    [
        byte_size(Remaining),
        Remaining,
        %Stream terminator
        0
    ].

%% Trailer
trailer() ->
    <<";">>.
