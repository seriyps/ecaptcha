%% @doc Basic PNG encoder
%%
%% [http://www.w3.org/TR/PNG]
-module(ecaptcha_png).

-export([encode/4]).

%% Pixels is a greyscale 1 byte per pixel, 0 = black, 255 = white.
%% row-by-row, from top-left to bottom-right.
-spec encode(binary(), pos_integer(), pos_integer(), ecaptcha:color_name()) -> iodata().
encode(Pixels, 200 = Width, 70 = Height, Color) when byte_size(Pixels) =:= (Width * Height) ->
    Signature = <<137, "PNG", "\r\n", 26, "\n">>,
    Palette = mk_palette(Pixels, Color),
    [
        Signature,
        chunk(<<"IHDR">>, mk_hdr(Width, Height)),
        chunk(<<"PLTE">>, Palette),
        chunk(<<"IDAT">>, mk_data(Pixels, Width, Palette)),
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

mk_palette(_Pixels, Color) ->
    [ecaptcha_color:bin_3b(ecaptcha_color:by_name(C)) || C <- [Color, white]].

mk_data(Pixels, Width, Palette) ->
    zlib:compress([mk_row(Row, Palette) || <<Row:Width/binary>> <= Pixels]).

mk_row(RowPixels, _Palette) ->
    [
        0,
        <<
            <<(case Pixel < 255 of
                    % 0 - index in Palette (black)
                    true -> 0;
                    % 1 - index in Palette (white)
                    false -> 1
                end)>>
            || <<Pixel>> <= RowPixels
        >>
    ].

chunk(Type, Data) ->
    Length = iolist_size(Data),
    CRC = erlang:crc32([Type, Data]),
    [<<Length:32/big>>, Type, Data, <<CRC:32/big>>].
