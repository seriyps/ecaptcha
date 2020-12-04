%% @doc Helpers to lookup / manipulate RGB colors
-module(ecaptcha_color).

-export([new/3, by_name/1, bin_3b/1]).
-export([
    palette_colors_by_frequency/1,
    palette_get_index/2,
    palette_size/1,
    new_palette/2,
    histogram_from_8b_pixels/1,
    histogram_map_channel_to_rgb/2
]).

-export_type([color_name/0, rgb/0, palette/0]).

%single greyscale value
-type channel() :: byte().
-type rgb() :: {channel(), channel(), channel()}.
-type color_name() :: black | white | red | orange | blue | pink | purple.

-spec new(channel(), channel(), channel()) -> rgb().
new(R, G, B) ->
    {R, G, B}.

-spec bin_3b(rgb()) -> binary().
bin_3b({R, G, B}) ->
    <<R, G, B>>.

-spec by_name(color_name()) -> rgb().
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

%%
%% Palette
%%
-type histogram(Color) :: #{Color => pos_integer()}.

-record(palette, {
    colors :: [rgb()],
    lookup_tab :: #{channel() => non_neg_integer()}
}).

-opaque palette() :: #palette{}.

%% @doc Get color index from palette
-spec palette_get_index(channel(), palette()) -> non_neg_integer().
palette_get_index(Color, #palette{lookup_tab = LookupTab}) ->
    maps:get(Color, LookupTab).

%% @doc Number of colors in palette
-spec palette_size(palette()) -> non_neg_integer().
palette_size(#palette{colors = Colors}) ->
    length(Colors).

%% @doc Returns a list with colors sorted by their frequency in desc order (most frequent first)
%%
%% Colors with the same frequency are sorted by value
-spec palette_colors_by_frequency(palette()) -> [rgb()].
palette_colors_by_frequency(#palette{colors = Colors}) ->
    Colors.

%% @doc Create a palette by mapping Greyscale raster to RGB color
-spec new_palette(binary(), rgb()) -> palette().
new_palette(Pixels, Color) ->
    %% We need 2 strctures:
    %% 1. List of RGB colors sorted by frequency {freq_index(), rgb()}
    %% 2. Mapping from greyscale to freq_index() (can have duplicates)
    Histogram8bit = histogram_from_8b_pixels(Pixels),
    % [{rgb(), channel(), freq()}]
    Mapping = histogram_map_channel_to_rgb(Histogram8bit, Color),
    %% one channel() can map to more than one rgb()
    ChannelToRGB = maps:from_list([{Ch, RGB} || {RGB, Ch, _} <- Mapping]),
    %% RGBHistogram can be smaller than ChannelToRGB
    RGBHistogram = lists:foldl(
        fun({RGB, _, Freq}, Acc) ->
            Freq0 = maps:get(RGB, Acc, 0),
            Acc#{RGB => Freq0 + Freq}
        end,
        #{},
        Mapping
    ),
    %% [{rgb(), freq_index()}], sorted by freq_index()
    RGBByFreq = indexed_from_histogram(RGBHistogram),
    FreqIndexByRGB = maps:from_list(RGBByFreq),
    ChannelToIndex = maps:map(fun(_Ch, RGB) -> maps:get(RGB, FreqIndexByRGB) end, ChannelToRGB),
    #palette{
        colors = [RGB || {RGB, _Idx} <- RGBByFreq],
        lookup_tab = ChannelToIndex
    }.

indexed_from_histogram(Histogram) ->
    Sorted = hist_sort_by_frequency(Histogram),
    {LookupList, _Size} = lists:mapfoldl(
        fun({Color, _}, Idx) ->
            {{Color, Idx}, Idx + 1}
        end,
        0,
        Sorted
    ),
    LookupList.

hist_sort_by_frequency(Hist) ->
    lists:sort(
        fun
            ({ValA, Freq}, {ValB, Freq}) ->
                ValA >= ValB;
            ({_, FreqA}, {_, FreqB}) ->
                FreqA >= FreqB
        end,
        maps:to_list(Hist)
    ).

%% @doc Builds a greyscale histogram from 8bit pixels binary
-spec histogram_from_8b_pixels(binary()) -> histogram(channel()).
histogram_from_8b_pixels(Pixels) ->
    histogram_from_8b_pixels(Pixels, #{}).

histogram_from_8b_pixels(<<>>, Hist) ->
    Hist;
histogram_from_8b_pixels(<<Pixel, Pixels/binary>>, Hist) ->
    Count = maps:get(Pixel, Hist, 0),
    histogram_from_8b_pixels(Pixels, Hist#{Pixel => Count + 1}).

%% @doc Maps 8bit greyscale color histogram to the RGB color.
%%
%% This is to, kind of, use colors from greyscale as a "saturation" value for RGB color.
%% Or, to convert a greyscale image to a single-color-tone image.
-spec histogram_map_channel_to_rgb(histogram(channel()), rgb()) ->
    [{rgb(), channel(), Freq :: pos_integer()}].
histogram_map_channel_to_rgb(Histogram8b, RGB) ->
    {Pixels, Frequences} = lists:unzip(maps:to_list(Histogram8b)),
    lists:zip3(palette_map_channel_to_rgb(Pixels, RGB), Pixels, Frequences).

palette_map_channel_to_rgb(Pixels8b, {R, G, B}) ->
    %% It's a bit similar to changing "Saturation" in HSV color model, but not really
    %% In greyscale:      0 means black,                 255 means white
    %% Translated to RGB: 0 means is the color provided, 255 means white
    %%
    %% So, when (1-indexed) Color = {256, 128, 0} and Channel = 128, it should translate
    %% to {256 + 0, 128 + 64, 0 + 128}
    Channels = [R, G, B],
    Velocities = [(255 - C) / 255 || C <- Channels],
    [
        list_to_tuple(
            map_one(Channels, Velocities, P)
        )
        || P <- Pixels8b
    ].

map_one([C | Channels], [V | Velocities], P) ->
    [round(C + P * V) | map_one(Channels, Velocities, P)];
map_one([], [], _) ->
    [].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

channel_to_rgb_test() ->
    ?assertEqual([{128, 128, 128}], palette_map_channel_to_rgb([128], {0, 0, 0})),
    ?assertEqual(
        [{128 + 64, 128 + 64, 128 + 64}],
        palette_map_channel_to_rgb([128], {128, 128, 128})
    ),
    ?assertEqual(
        [{255, 128 + 64, 128}],
        palette_map_channel_to_rgb([128], {256, 128, 0})
    ).
-endif.
