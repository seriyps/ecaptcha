-module(prop_ecaptcha).

-export([
    prop_nif_no_crashes/1,
    prop_captcha_pixels_no_crashes/1,
    prop_captcha_pixels_valid/1,
    prop_gif_no_crashes/1,
    prop_gif_valid/1,
    prop_gif_gradient_identify/1,
    prop_gif_lzw_vs_py/1,
    prop_png_valid/1,
    prop_png_rgb_valid/1,
    prop_png_gradient_identify/1
]).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(ERR_REASONS, [
    chars_not_binary,
    wrong_chars_length,
    invalid_character,
    bad_random,
    small_rand_binary,
    opts_not_list,
    non_atom_option,
    unknown_option
]).

%% Nif

prop_nif_no_crashes(doc) ->
    "Checks that NIF does not crash whatever input is".

prop_nif_no_crashes() ->
    ?FORALL(
        {Chars, Rand, Opts},
        nif_input_gen(),
        case ecaptcha_nif:pixels(Chars, Rand, Opts) of
            {error, Err} ->
                lists:member(Err, ?ERR_REASONS);
            Bytes when is_binary(Bytes) ->
                true
        end
    ).

nif_input_gen() ->
    Valid = {alpha_gen(1, 7), proper_types:binary(ecaptcha_nif:rand_size()), opts_gen()},
    BadRand = {alpha_gen(1, 7), proper_types:binary(), opts_gen()},
    BadOpts = {alpha_gen(1, 7), proper_types:binary(ecaptcha_nif:rand_size()), proper_types:any()},
    BadChars = {proper_types:any(), proper_types:binary(), opts_gen()},
    Mess = {proper_types:any(), proper_types:any(), proper_types:any()},
    proper_types:oneof([Valid, BadRand, BadOpts, BadChars, Mess]).

%% Pixels

prop_captcha_pixels_no_crashes(doc) ->
    "Checks that ecaptcha:pixels/2 never crashes".

prop_captcha_pixels_no_crashes() ->
    ?FORALL(
        {Size, Opts},
        {proper_types:non_neg_integer(), proper_types:any()},
        case ecaptcha:pixels(Size, Opts) of
            {error, Err} ->
                lists:member(Err, ?ERR_REASONS);
            {Text, Bytes} when is_binary(Text), is_binary(Bytes) ->
                true
        end
    ).

prop_captcha_pixels_valid(doc) ->
    "Checks that ecaptcha:pixels/2 always produces binary pixel data for valid input".

prop_captcha_pixels_valid() ->
    ?FORALL(
        {Size, Opts},
        {proper_types:range(1, 7), opts_gen()},
        begin
            {Text, Pixels} = ecaptcha:pixels(Size, Opts),
            ?assertEqual(Size, byte_size(Text)),
            ?assertEqual(70 * 200, byte_size(Pixels)),
            true
        end
    ).

%% GIF

prop_gif_no_crashes(doc) ->
    "Checks that ecaptcha:gif/2 never crashes".

prop_gif_no_crashes() ->
    ?FORALL(
        {Size, Opts, Color},
        {proper_types:range(1, 7), proper_types:any(), color_gen()},
        case ecaptcha:gif(Size, Opts, Color) of
            {error, Reason} ->
                lists:member(Reason, ?ERR_REASONS);
            {Text, GifBytes} when is_binary(Text) ->
                iolist_size(GifBytes) > 0
        end
    ).

prop_gif_valid(doc) ->
    "Checks that ecaptcha:gif/2 always produces binary GIF data for valid input".

prop_gif_valid() ->
    exec:start([]),
    ?FORALL(
        {Size, Opts, Color},
        {proper_types:range(1, 7), opts_gen(), color_gen()},
        begin
            {Text, Gif} = ecaptcha:gif(Size, Opts, Color),
            GifBin = iolist_to_binary(Gif),
            ?assertEqual(Size, byte_size(Text)),
            ?assertMatch(<<"GIF89a", _/binary>>, GifBin),
            Res = identify(GifBin),
            ?WHENFAIL(
                begin
                    dump("tst_~s_~s_~s.gif", Text, Opts, Color, Gif),
                    io:format(
                        "In: ~p~nRes: ~120p~n",
                        [{Size, Opts, Color}, Res]
                    )
                end,
                normal == maps:get(code, Res)
            )
        end
    ).

prop_gif_gradient_identify(doc) ->
    "Checks that imagemagic `identify` command validates all the GIF images".

prop_gif_gradient_identify() ->
    exec:start([]),
    ?FORALL(
        {X, Y, NColors},
        ?LET(
            {X0, Y0},
            {proper_types:range(4, 500), proper_types:range(4, 500)},
            begin
                MaxColors = min(X0 * Y0, 256),
                {X0, Y0, proper_types:range(3, MaxColors)}
            end
        ),
        begin
            Colors = lists:seq(0, NColors - 1),
            Pixels = mk_pixels(Colors, X * Y),
            Iodata = ecaptcha_gif:encode(Pixels, X, Y, black),
            Res = identify(Iodata),
            ?WHENFAIL(
                io:format(
                    "In: ~p~nPixels: ~120p~nRes: ~120p~n",
                    [{X, Y, NColors}, Pixels, Res]
                ),
                normal == maps:get(code, Res)
            )
        end
    ).

prop_gif_lzw_vs_py(doc) ->
    "Compares output of ecaptcha_gif_lzw with output of reference Python implementation".

prop_gif_lzw_vs_py() ->
    exec:start([]),
    ?FORALL(
        {Len, NColors},
        ?LET(
            Len0,
            proper_types:range(4, 2000),
            begin
                MaxColors = min(Len0, 256),
                {Len0, proper_types:range(3, MaxColors)}
            end
        ),
        begin
            Colors = lists:seq(0, NColors - 1),
            Pixels = mk_pixels(Colors, Len),
            CodeSize = ecaptcha_gif:min_bits_to_fit(NColors),
            ECCompressed = ecaptcha_gif_lzw:compress(Pixels, CodeSize),
            PYCompressedRes = lzw_py(Pixels, CodeSize),
            ?WHENFAIL(
                begin
                    Stdout = maps:get(stdout, PYCompressedRes, <<>>),
                    LongestPrefix = binary:longest_common_prefix([ECCompressed, Stdout]),
                    io:format(
                        "Len:~p,NColors:~p,~nPixels:~p~nEcaptcha:~n~200p~n"
                        "Python:~n~200p~nLongestPrefix:~p~nPyErr:~n~s~n",
                        [
                            Len,
                            NColors,
                            Pixels,
                            ECCompressed,
                            Stdout,
                            LongestPrefix,
                            maps:get(stderr, PYCompressedRes, "")
                        ]
                    )
                end,
                ECCompressed == maps:get(stdout, PYCompressedRes)
            )
        end
    ).

%% PNG

prop_png_valid(doc) ->
    "Checks that ecaptcha_png:encode/2 produces binary PNG for valid input".

prop_png_valid() ->
    exec:start([]),
    ?FORALL(
        {NumChars, Opts, Color},
        {proper_types:range(1, 7), opts_gen(), color_gen()},
        begin
            {Text, Png} = ecaptcha:png(NumChars, Opts, Color),
            PngBin = iolist_to_binary(Png),
            ?assertMatch(<<137, "PNG\r\n", _/binary>>, PngBin),
            Res = identify(PngBin),
            ?WHENFAIL(
                begin
                    dump("tst_~s_~s_~s.png", Text, Opts, Color, Png),
                    io:format(
                        "In: ~p~nRes: ~120p~n",
                        [{NumChars, Opts, Color}, Res]
                    )
                end,
                normal == maps:get(code, Res)
            )
        end
    ).

prop_png_rgb_valid(doc) ->
    "Checks that ecaptcha_png:encode/2 produces PNG for valid input with arbitrary RGB colors".

prop_png_rgb_valid() ->
    ?FORALL(
        {NumChars, Opts, Color},
        {proper_types:range(1, 7), opts_gen(), rgb_gen()},
        begin
            {_Text, Png} = ecaptcha:png(NumChars, Opts, Color),
            PngBin = iolist_to_binary(Png),
            %% dump("tst_~s_~s_~p.png", _Text, Opts, Color, Png),
            ?assertMatch(<<137, "PNG\r\n", _/binary>>, PngBin),
            true
        end
    ).

prop_png_gradient_identify(doc) ->
    "Checks that imagemagic `identify` command validates all the PNG images".

prop_png_gradient_identify() ->
    exec:start([]),
    ?FORALL(
        {X, Y, NColors, Color},
        ?LET(
            {X0, Y0},
            {proper_types:range(4, 1000), proper_types:range(4, 1000)},
            begin
                MaxColors = min(X0 * Y0, 256),
                {X0, Y0, proper_types:range(3, MaxColors), rgb_gen()}
            end
        ),
        begin
            Colors = lists:seq(0, NColors - 1),
            Pixels = mk_pixels(Colors, X * Y),
            Iodata = ecaptcha_png:encode(Pixels, X, Y, Color),
            Res = identify(Iodata),
            ?WHENFAIL(
                io:format(
                    "In: ~p~nPixels: ~120p~nRes: ~120p~n",
                    [{X, Y, NColors}, Pixels, Res]
                ),
                normal == maps:get(code, Res)
            )
        end
    ).

prop_map_palettes_consistent() ->
    ?FORALL(
       {Pixels, Color},
       {proper_types:non_empty(proper_types:binary()), rgb_gen()},
       begin
           %% Pixels = list_to_binary(lists:seq(0, MaxGreyscale)),
           {Palette8bit, PaletteRGB} = ecaptcha_color:map_palettes(Pixels, Color),
           ?assertEqual(ecaptcha_color:palette_size(Palette8bit),
                        ecaptcha_color:palette_size(PaletteRGB)),
           true
       end
      ).

prop_map_histogram_consistent() ->
    ?FORALL(
       {Pixels, Color},
       {proper_types:non_empty(proper_types:binary()), rgb_gen()},
       begin
           Histogram8b = ecaptcha_color:histogram_from_8b_pixels(Pixels),
           HistogramRGB = ecaptcha_color:histogram_map_channel_to_rgb(Histogram8b, Color),
           ?WHENFAIL(
              io:format("Hist8b:~p~nHistRGB:~p~n", [Histogram8b, HistogramRGB]),
              map_size(Histogram8b) == map_size(HistogramRGB))
       end).

%% Generator helpers

opts_gen() ->
    proper_types:list(proper_types:oneof([line, blur, filter, dots])).

color_gen() ->
    proper_types:oneof([black, red, orange, blue, pink, purple]).

rgb_gen() ->
    {proper_types:range(0, 255), proper_types:range(0, 255), proper_types:range(0, 255)}.

alpha_gen(Min, Max) ->
    ?LET(
        CharList,
        ?SUCHTHAT(
            List,
            proper_types:list(proper_types:oneof(lists:seq($a, $z))),
            begin
                LL = length(List),
                Min =< LL andalso LL =< Max
            end
        ),
        list_to_binary(CharList)
    ).

%% Helpers

mk_pixels(Colors, N) ->
    list_to_binary(mk_pixels(Colors, Colors, N)).

mk_pixels(_, _, 0) -> [];
mk_pixels([], Colors, N) -> mk_pixels(Colors, Colors, N);
mk_pixels([Color | NextColors], AllColors, N) -> [Color | mk_pixels(NextColors, AllColors, N - 1)].

identify(Iodata) ->
    Cmd = os:find_executable("identify"),
    exec(Cmd, ["-verbose", "-"], Iodata).

lzw_py(Iodata, MinCode) ->
    exec("test/lzw_io.py", [], [MinCode, Iodata]).

exec(Cmd, Args, Input) ->
    FullCmd = Cmd ++ lists:flatten([[" " | A] || A <- Args]),
    {ok, Pid, OsPid} = exec:run(FullCmd, [stdin, stdout, stderr, monitor]),
    ok = exec:send(Pid, iolist_to_binary(Input)),
    ok = exec:send(Pid, eof),
    recv(OsPid, #{}).

recv(OsPid, Acc) ->
    receive
        {stdout, OsPid, Data} ->
            recv(OsPid, Acc#{stdout => Data});
        {stderr, OsPid, Data} ->
            recv(OsPid, Acc#{stderr => Data});
        {'DOWN', OsPid, process, _, Code} ->
            Acc#{code => Code}
    after 5000 -> error({timeout, Acc})
    end.

dump(Fmt, Text, Opts, Color, Data) ->
    OptsStr = lists:join(",", lists:map(fun erlang:atom_to_binary/1, lists:usort(Opts))),
    Name = lists:flatten(io_lib:format(Fmt, [Text, OptsStr, Color])),
    ok = file:write_file(Name, Data).
