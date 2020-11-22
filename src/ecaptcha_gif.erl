-module(ecaptcha_gif).

-export([encode/4]).

-spec encode(binary(), pos_integer(), pos_integer(), ecaptcha:color()) -> iodata().
encode(Pixels, 200 = X, 70 = Y, Color) when byte_size(Pixels) =:= (X * Y) ->
    [
        header(Color),
        encode_rows(Pixels, X, Y, <<>>),
        trailer()
    ].

%% Header

header(Color) ->
    Palette = binary:copy(palette(Color), 15),
    [header0(), Palette, header1()].

header0() ->
    <<"GIF89a", 16#c8, 0, 16#46, 0, 16#83, 0, 0>>.

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

header1() ->
    <<16#ff, 16#ff, 16#ff, ",", 0, 0, 0, 0, 16#c8, 0, 16#46, 0, 0, 16#4>>.

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
