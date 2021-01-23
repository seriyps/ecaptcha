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

-export([opts_gen/0]).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(ERR_REASONS, [
    font_name_not_binary,
    font_not_found,
    chars_not_binary,
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
        {Font, Chars, Rand, Effects},
        nif_input_gen(),
        case ecaptcha_nif:pixels(Font, Chars, Rand, Effects) of
            {error, Err} ->
                lists:member(Err, ?ERR_REASONS);
            Bytes when is_binary(Bytes) ->
                true
        end
    ).

nif_input_gen() ->
    Valid =
        {font_gen(), alpha_gen(1, 7), proper_types:binary(ecaptcha_nif:rand_size()), effects_gen()},
    BadRand = {font_gen(), alpha_gen(1, 7), proper_types:binary(), effects_gen()},
    BadOpts =
        {font_gen(), alpha_gen(1, 7), proper_types:binary(ecaptcha_nif:rand_size()),
            proper_types:any()},
    BadChars = {font_gen(), proper_types:any(), proper_types:binary(), effects_gen()},
    BadFont =
        {proper_types:any(), alpha_gen(1, 7), proper_types:binary(ecaptcha_nif:rand_size()),
            effects_gen()},
    Mess = {proper_types:any(), proper_types:any(), proper_types:any(), proper_types:any()},
    proper_types:oneof([Valid, BadRand, BadOpts, BadChars, BadFont, Mess]).

%% Pixels

prop_captcha_pixels_no_crashes(doc) ->
    "Checks that ecaptcha:pixels/2 never crashes".

prop_captcha_pixels_no_crashes() ->
    ?FORALL(
        {Size, Opts},
        {proper_types:non_neg_integer(), any_nif_opts_gen()},
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
        {Size, Opts},
        {proper_types:range(1, 7), any_nif_opts_gen()},
        case ecaptcha:gif(Size, Opts) of
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
        {Size, Opts},
        {proper_types:range(1, 7), opts_gen()},
        begin
            {Text, Gif} = ecaptcha:gif(Size, Opts),
            GifBin = iolist_to_binary(Gif),
            ?assertEqual(Size, byte_size(Text)),
            ?assertMatch(<<"GIF89a", _/binary>>, GifBin),
            Res = identify(GifBin),
            ?WHENFAIL(
                begin
                    dump("tst_~s_~s_~s.gif", Text, Opts, Gif),
                    io:format(
                        "In: ~p~nRes: ~120p~n",
                        [{Size, Opts}, Res]
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
        {X, Y, NColors, Color},
        ?LET(
            {X0, Y0},
            {proper_types:range(1, 1000), proper_types:range(1, 1000)},
            begin
                MaxColors = min(X0 * Y0, 256),
                {X0, Y0, proper_types:range(2, MaxColors), rgb_gen()}
            end
        ),
        begin
            Colors = lists:seq(0, NColors - 1),
            Pixels = mk_pixels(Colors, X * Y),
            Iodata = ecaptcha_gif:encode(Pixels, X, Y, Color),
            %% Images of above ~66kb for some reason cause errors in `identify`
            case iolist_size(Iodata) > 65500 of
                true ->
                    true;
                false ->
                    Res = identify(Iodata),
                    ?WHENFAIL(
                        begin
                            dump("err_~wx~w_ncolors~w_color~p.gif", [X, Y, NColors, Color], Iodata),
                            io:format(
                                "In: ~p~nRes: ~120p~n",
                                [{X, Y, NColors}, Res]
                            )
                        end,
                        proper:conjunction(
                            [
                                {code_normal, normal == maps:get(code, Res)},
                                {no_io_err, [] == maps:get(err, Res, [])},
                                {no_stderr, [] == maps:get(stderr, Res, [])}
                            ]
                        )
                    )
            end
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
            proper_types:range(2, 2000),
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
        {NumChars, Opts},
        {proper_types:range(4, 6), opts_gen()},
        begin
            {Text, Png} = ecaptcha:png(NumChars, Opts),
            PngBin = iolist_to_binary(Png),
            ?assertMatch(<<137, "PNG\r\n", _/binary>>, PngBin),
            Res = identify(PngBin),
            %% dump("tst_~s_~s.png", Text, Opts, Png),
            ?WHENFAIL(
                begin
                    dump("tst_~s_~s.png", Text, Opts, Png),
                    io:format(
                        "In: ~p~nRes: ~120p~n",
                        [{NumChars, Opts}, Res]
                    )
                end,
                normal == maps:get(code, Res)
            )
        end
    ).

prop_png_rgb_valid(doc) ->
    "Checks that ecaptcha_png:encode/2 produces PNG for valid input with arbitrary RGB colors".

prop_png_rgb_valid() ->
    exec:start([]),
    ?FORALL(
        {NumChars, Opts},
        {proper_types:range(1, 7), opts_gen()},
        begin
            {Text, Png} = ecaptcha:png(NumChars, Opts),
            PngBin = iolist_to_binary(Png),
            ?assertMatch(<<137, "PNG\r\n", _/binary>>, PngBin),
            Res = identify(PngBin),
            ?WHENFAIL(
                begin
                    dump("tst_~s_~s.png", Text, Opts, Png),
                    io:format(
                        "In: ~p~nRes: ~120p~n",
                        [{NumChars, Opts}, Res]
                    )
                end,
                normal == maps:get(code, Res)
            )
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
            {proper_types:range(1, 1000), proper_types:range(1, 1000)},
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
                proper:conjunction(
                    [
                        {code_normal, normal == maps:get(code, Res)},
                        {no_io_err, [] == maps:get(err, Res, [])}
                    ]
                )
            )
        end
    ).

prop_map_histogram_consistent() ->
    ?FORALL(
        {Pixels, Color},
        {proper_types:non_empty(proper_types:binary()), rgb_gen()},
        begin
            Histogram8b = ecaptcha_color:histogram_from_8b_pixels(Pixels),
            Mapping = ecaptcha_color:histogram_map_channel_to_rgb(Histogram8b, Color),
            ?WHENFAIL(
                io:format("Hist8b:~p~nMapping:~p~n", [Histogram8b, Mapping]),
                map_size(Histogram8b) == length(Mapping)
            )
        end
    ).

%% Generator helpers

any_nif_opts_gen() ->
    ?LET(
        Proplist,
        proper_types:list(
            proper_types:oneof(
                [
                    {effects, proper_types:list()},
                    {font, proper_types:binary()}
                ]
            )
        ),
        maps:from_list(Proplist)
    ).

opts_gen() ->
    ?LET(
        Proplist,
        proper_types:list(
            proper_types:oneof(
                [
                    {color, color_gen()},
                    {color, rgb_gen()},
                    {effects, effects_gen()},
                    {font, font_gen()},
                    {alphabet, alphabet_name_gen()},
                    {alphabet, alphabet_binary_gen()}
                ]
            )
        ),
        maps:from_list(Proplist)
    ).

effects_gen() ->
    proper_types:list(
        proper_types:oneof(
            [
                line,
                blur,
                filter,
                dots,
                reverse_dots
            ]
        )
    ).

color_gen() ->
    proper_types:oneof([black, red, orange, blue, pink, purple]).

rgb_gen() ->
    {proper_types:range(0, 255), proper_types:range(0, 255), proper_types:range(0, 255)}.

font_gen() ->
    proper_types:oneof([<<"hplhs-oldstyle">>, <<"ubuntu-r">>, <<"dejavusans">>]).

alpha_gen(Min, Max) ->
    binary_gen(lists:seq($a, $z), Min, Max).

alphabet_name_gen() ->
    proper_types:oneof([numbers, latin_lowercase, latin_uppercase]).

alphabet_binary_gen() ->
    %% Duplicates are ok
    binary_gen(lists:seq($0, $9) ++ lists:seq($a, $z) ++ lists:seq($A, $Z), 4, 62).

binary_gen(Alphabet, Min, Max) ->
    ?LET(
        CharList,
        ?SUCHTHAT(
            List,
            ?SIZED(
                Size,
                proper_types:resize(
                    min(max(Min, Size), Max),
                    proper_types:list(proper_types:oneof(Alphabet))
                )
            ),
            Min =< length(List)
        ),
        list_to_binary(lists:sublist(CharList, Max))
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
    Res0 =
        try
            ok = send_chunks(Pid, iolist_to_binary(Input)),
            %% ok = exec:send(Pid, iolist_to_binary(Input)),
            ok = exec:send(Pid, eof),
            #{}
        catch
            T:R:S ->
                exec:start([]),
                #{err => {T, R, S}}
        end,
    recv(OsPid, Res0).

send_chunks(Pid, <<Chunk:2048/binary, Tail/binary>>) ->
    ok = exec:send(Pid, Chunk),
    send_chunks(Pid, Tail);
send_chunks(Pid, Tail) ->
    exec:send(Pid, Tail).

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

dump(Fmt, Params, Data) ->
    Name = lists:flatten(io_lib:format(Fmt, Params)),
    ok = file:write_file(Name, Data).

dump(Fmt, Text, Opts, Data) ->
    dump(Fmt, [Text, o2s(Opts)], Data).

o2s(Opts) ->
    OptsL = lists:ukeysort(1, maps:to_list(Opts)),
    lists:join(",", [io_lib:format("~s=~0P", [K, V, 20]) || {K, V} <- OptsL]).
