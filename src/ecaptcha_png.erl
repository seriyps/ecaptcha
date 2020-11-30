%% @doc Basic PNG encoder
%%
%% [http://www.w3.org/TR/PNG]
-module(ecaptcha_png).

-export([encode/4]).

%% Pixels is a greyscale 1 byte per pixel, 0 = black, 255 = white.
%% row-by-row, from top-left to bottom-right.
-spec encode(
    binary(),
    pos_integer(),
    pos_integer(),
    ecaptcha:color_name() | ecaptcha:color_rgb()
) -> iodata().
encode(Pixels, Width, Height, ColorName) when is_atom(ColorName) ->
    encode(Pixels, Width, Height, ecaptcha_color:by_name(ColorName));
encode(Pixels, Width, Height, Color) when byte_size(Pixels) =:= (Width * Height) ->
    Signature = <<137, "PNG", "\r\n", 26, "\n">>,
    %% we abuse the fact that color indexes in RGB and 8bit palette are the same, so we encode
    %% colors in "PLTE" section from RGB palette, but use 8bit palette for "IDAT" lookups, because
    %% `Pixels' are 8bit (greyscale).
    {PaletteRGB, ColorLookup} = ecaptcha_color:map_palettes(Pixels, Color),
    [
        Signature,
        chunk(<<"IHDR">>, mk_hdr(Width, Height)),
        chunk(<<"PLTE">>, mk_palette(PaletteRGB)),
        chunk(<<"IDAT">>, mk_data(Pixels, Width, ColorLookup)),
        chunk(<<"IEND">>, <<>>)
    ].

%% erlfmt-ignore
mk_hdr(Width, Height) ->
    <<
      Width:32/big,
      Height:32/big,
      8,                                        % BitDepth - bits per palette index
      3,                                        % ColourType - indexed-colour
      0,                                        % CompressionMethod - deflate
      0,                                        % FilterMethod
      0                                         % InterlaceMethod - no interlace
    >>.

mk_palette(Palette) ->
    lists:map(fun ecaptcha_color:bin_3b/1, Palette).

mk_data(Pixels, Width, ColorLookup) ->
    zlib:compress([mk_row(Row, ColorLookup) || <<Row:Width/binary>> <= Pixels]).

mk_row(RowPixels, ColorLookup) ->
    [
        % Filter type - None
        0,
        <<
            <<(maps:get(Pixel, ColorLookup))>>
            || <<Pixel>> <= RowPixels
        >>
    ].

chunk(Type, Data) ->
    Length = iolist_size(Data),
    CRC = erlang:crc32([Type, Data]),
    [<<Length:32/big>>, Type, Data, <<CRC:32/big>>].
