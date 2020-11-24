-module(prop_ecaptcha).

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

prop_pixels_no_crashes(doc) ->
    "Checks that ecaptcha:pixels/2 never crashes".

prop_pixels_no_crashes() ->
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

prop_pixels_valid(doc) ->
    "Checks that ecaptcha:pixels/2 always produces binary pixel data for valid input".

prop_pixels_valid() ->
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
    ?FORALL(
        {Size, Opts, Color},
        {proper_types:range(1, 7), opts_gen(), color_gen()},
        begin
            {Text, Gif} = ecaptcha:gif(Size, Opts, Color),
            GifBin = iolist_to_binary(Gif),
            %% dump("tst_~s_~s_~s.gif", Text, Opts, Color, Gif),
            ?assertEqual(Size, byte_size(Text)),
            ?assertEqual(17646, byte_size(GifBin)),
            ?assertMatch(<<"GIF89a", _/binary>>, GifBin),
            true
        end
    ).

%% PNG

prop_png_valid(doc) ->
    "Checks that ecaptcha_png:encode/2 produces binary PNG for valid input".

prop_png_valid() ->
    ?FORALL(
        {NumChars, Opts, Color},
        {proper_types:range(1, 7), opts_gen(), color_gen()},
        begin
            {_Text, Png} = ecaptcha:png(NumChars, Opts, Color),
            PngBin = iolist_to_binary(Png),
            %% dump("tst_~s_~s_~s.png", _Text, Opts, Color, Png),
            ?assertMatch(<<137, "PNG\r\n", _/binary>>, PngBin),
            true
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

%% Generator helpers

opts_gen() ->
    proper_types:list(proper_types:oneof([line, blur, filter, dots])).

color_gen() ->
    proper_types:oneof([black, red, orange, blue, pink, purple]).

rgb_gen() ->
    {proper_types:range(0, 255),
     proper_types:range(0, 255),
     proper_types:range(0, 255)}.

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

%% dump(Fmt, Text, Opts, Color, Data) ->
%%     OptsStr = lists:join(",", lists:map(fun erlang:atom_to_binary/1, lists:usort(Opts))),
%%     Name = lists:flatten(io_lib:format(Fmt, [Text, OptsStr, Color])),
%%     ok = file:write_file(Name, Data).
