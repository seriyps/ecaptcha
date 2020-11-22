-module(prop_ecaptcha).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Pixels
-define(ERR_REASONS, [
    length_not_integer,
    invalid_num_chars,
    bad_random,
    small_rand_binary,
    opts_not_list,
    non_atom_opt,
    unknown_option
]).

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
        {proper_types:range(1, 7), filter_gen()},
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
            {Text, GifBytes} when is_binary(Text), is_binary(GifBytes) ->
                true
        end
    ).

prop_gif_valid(doc) ->
    "Checks that ecaptcha:gif/2 always produces binary GIF data for valid input".

prop_gif_valid() ->
    ?FORALL(
        {Size, Opts, Color},
        {proper_types:range(1, 7), filter_gen(), color_gen()},
        begin
            {Text, GifBytes} = ecaptcha:gif(Size, Opts, Color),
            ?assertEqual(Size, byte_size(Text)),
            ?assertEqual(17646, byte_size(GifBytes)),
            ?assertMatch(<<"GIF89a", _/binary>>, GifBytes),
            true
        end
    ).

prop_nif_vs_erl_gif(doc) ->
    "Compares NIF GIF encoder with ecaptcha_gif".

prop_nif_vs_erl_gif() ->
    ?FORALL(
        {NumChars, Opts, Color},
        {proper_types:range(1, 7), filter_gen(), color_gen()},
        begin
            {_Text, Pixels} = ecaptcha:pixels(NumChars, Opts),
            ?assertEqual(
                ecaptcha:pixels_as_gif(Pixels, Color),
                iolist_to_binary(ecaptcha_gif:encode(Pixels, 200, 70, Color))
            ),
            true
        end
    ).

%% PNG

prop_png_no_crashes(doc) ->
    "Checks that ecaptcha_png:encode/2 never crashes".

prop_png_no_crashes() ->
    ?FORALL(
        {NumChars, Opts, Color},
        {proper_types:range(1, 7), filter_gen(), color_gen()},
        begin
            {_Text, Pixels} = ecaptcha:pixels(NumChars, Opts),
            Png = ecaptcha_png:encode(Pixels, 200, 70, Color),
            PngBin = iolist_to_binary(Png),
            ?assertMatch(<<137, "PNG\r\n", _/binary>>, PngBin),
            true
        end
    ).

%% Generator helpers

filter_gen() ->
    proper_types:list(proper_types:oneof([line, blur, filter, dots])).

color_gen() ->
    proper_types:oneof([black, red, orange, blue, pink, purple]).

%% dump(Fmt, Text, Opts, Color, Data) ->
%%     OptsStr = lists:join(",", lists:map(fun erlang:atom_to_binary/1, lists:usort(Opts))),
%%     Name = lists:flatten(io_lib:format(Fmt, [Text, OptsStr, Color])),
%%     ok = file:write_file(Name, Data).
