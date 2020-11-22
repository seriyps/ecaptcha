%% @doc Helpers to lookup / manipulate RGB colors
-module(ecaptcha_color).

-export([new/3, by_name/1, bin_3b/1]).

-export_type([color_name/0, color/0]).

-type color() :: {byte(), byte(), byte()}.
-type color_name() :: black | white | red | orange | blue | pink | purple.

-spec new(byte(), byte(), byte()) -> color().
new(R, G, B) ->
    {R, G, B}.

-spec bin_3b(color()) -> binary().
bin_3b({R, G, B}) ->
    <<R, G, B>>.

-spec by_name(color_name()) -> color().
by_name(black) ->
    {0, 0, 0};
by_name(red) ->
    {16#D5, 16#0, 16#0};
by_name(orange) ->
    {16#DD, 16#2C, 16#0};
by_name(pink) ->
    {16#C5, 16#11, 16#62};
by_name(purple) ->
    {16#62, 16#00, 16#EA};
by_name(blue) ->
    {16#29, 16#62, 16#FF};
by_name(white) ->
    {16#FF, 16#FF, 16#FF}.
