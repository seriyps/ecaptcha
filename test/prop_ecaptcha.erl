-module(prop_ecaptcha).

-export([prop_pixels_no_crashes/0, prop_pixels_no_crashes/1]).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").

prop_pixels_no_crashes(doc) ->
    "Checks that ecaptcha:pixels/2 never crashes".

prop_pixels_no_crashes() ->
    ?FORALL(
        {Size, Opts},
        any_input_gen(),
        case ecaptcha:pixels(Size, Opts) of
            {error, _} ->
                true;
            {Text, Bytes} ->
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
