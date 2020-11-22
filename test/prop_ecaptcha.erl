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
        any_input_gen(),
        case ecaptcha:pixels(Size, Opts) of
            {error, Err} ->
                lists:member(Err, ?ERR_REASONS);
            {Text, Bytes} when is_binary(Text), is_binary(Bytes) ->
                true
        end
    ).

any_input_gen() ->
    {proper_types:non_neg_integer(), proper_types:any()}.

prop_pixels_valid(doc) ->
    "Checks that ecaptcha:pixels/2 always produces binary pixel data for valid input".

prop_pixels_valid() ->
    ?FORALL(
        {Size, Opts},
        valid_input_gen(),
        begin
            {Text, Pixels} = ecaptcha:pixels(Size, Opts),
            ?assertEqual(Size, byte_size(Text)),
            ?assertEqual(70 * 200, byte_size(Pixels)),
            true
        end
    ).

valid_input_gen() ->
    {proper_types:range(1, 7), proper_types:list(proper_types:oneof([line, blur, filter, dots]))}.

%% GIF

prop_gif_no_crashes(doc) ->
    "Checks that ecaptcha:gif/2 never crashes".

prop_gif_no_crashes() ->
    ?FORALL(
        {Size, Opts},
        any_input_gen(),
        case ecaptcha:gif(Size, Opts) of
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
        {Size, Opts},
        valid_input_gen(),
        begin
            {Text, GifBytes} = ecaptcha:gif(Size, Opts),
            ?assertEqual(Size, byte_size(Text)),
            ?assertEqual(17646, byte_size(GifBytes)),
            true
        end
    ).
