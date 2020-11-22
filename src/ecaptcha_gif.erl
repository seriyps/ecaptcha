%% https://www.w3.org/Graphics/GIF/spec-gif89a.txt
-module(ecaptcha_gif).

-export([encode/4]).

-spec encode(binary(), pos_integer(), pos_integer(), ecaptcha:color()) -> iodata().
encode(Pixels, 200 = Width, 70 = Height, Color) when byte_size(Pixels) =:= (Width * Height) ->
    [
        header(Width, Height, Color),
        encode_rows(Pixels, Width, Height, <<>>),
        trailer()
    ].

%% Header

header(Width, Height, Color) ->
    Palette = binary:copy(palette(Color), 15),
    [header0(Width, Height), Palette, header1(Width, Height)].

%% erlfmt-ignore
header0(Width, Height) ->
    <<
      "GIF89a",
      Width:16/little,                          % Logical screen width
      Height:16/little,                         % Logical screen height
      1:1,                                      % GCTFlag:1 - 1 - Global Color Table exists
      0:3,                                      % ColorResolution:3
      0:1,                                      % SortFlag:1 - 0 - not sorted
      3:3,                                      % GCTSize:3 - `2^(3+1)' - size of GCT
      0,                                        % BackgroundColor index in palette
      0                                         % Aspect ratio
    >>.

palette(black) ->
    <<16#0, 16#0, 16#0>>;
palette(red) ->
    <<16#D5, 16#0, 16#0>>;
palette(orange) ->
    <<16#DD, 16#2C, 16#0>>;
palette(blue) ->
    <<16#29, 16#62, 16#FF>>;
palette(pink) ->
    <<16#C5, 16#11, 16#62>>;
palette(purple) ->
    <<16#62, 16#00, 16#EA>>.

%% erlfmt-ignore
header1(Width, Height) ->
    <<
      16#ff, 16#ff, 16#ff,                      % Palette - white
      ",",
      0:16/little, 0:16/little,                 % (x0, y0) - start of image
      Width:16/little, Height:16/little,        % (xN, yN) - end of image
      0,                                        % no local color table
      16#4                                      % LZW minimum code size
    >>.

%% Body

encode_rows(<<>>, _, 0, Acc) ->
    Acc;
encode_rows(Pixels, X, Y, Acc) ->
    <<RowPixels:X/binary, PixelsTail/binary>> = Pixels,
    % 250
    RowSize = (X div 4) * 5,
    Acc1 = encode_row(RowPixels, <<Acc/binary, RowSize>>),
    encode_rows(PixelsTail, X, Y - 1, Acc1).

%% No idea how this works. Blindly translated from ecaptcha.c#makegif
%% erlfmt-ignore
encode_row(<<A0, B0, C0, D0, Rest/binary>>, Acc) ->
    A = A0 bsr 4,
    B = B0 bsr 4,
    C = C0 bsr 4,
    D = D0 bsr 4,
    encode_row(
        Rest,
        <<Acc/binary,
          (16 bor (A bsl 5)),
          ((A bsr 3) bor 64 bor ((B bsl 7) rem 256)),
          (B bsr 1),
          (1 bor (C bsl 1)),
          (4 bor (D bsl 3))>>
    );
encode_row(<<>>, Acc) ->
    Acc.

%% Trailer
trailer() ->
    <<16#01, 16#11, 0, ";">>.
