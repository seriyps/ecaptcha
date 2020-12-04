%% @doc GIF-specific version of LZW compression algorithm
%%
%% Based on:
%% * http://www.matthewflickinger.com/lab/whatsinagif/lzw_image_data.asp
%% * http://giflib.sourceforge.net/gifstandard/LZW-and-GIF-explained.html
%% * https://www.w3.org/Graphics/GIF/spec-gif87.txt
%%
%% * https://github.com/mochi/erl_img
%% * https://github.com/marcelog/erl_lzw
%% * http://rosettacode.org/wiki/LZW_compression#Erlang

-module(ecaptcha_gif_lzw).

-export([compress/2]).

-define(MAX_BITS_PER_CODE, 12).

-record(st, {
    dict :: #{
        binary() => 0..4096,
        clear => 0..4096,
        eoi => 0..4096
    },
    bits_per_code :: 2..12, %?MAX_BITS_PER_CODE,
    start_bits_per_code :: 2..12 %?MAX_BITS_PER_CODE
}).

-record(acc, {
    out :: binary(),
    acc :: [bitstring()],
    acc_bits :: 0..7
}).

-spec compress(binary(), 2..12) -> binary().
compress(Data, InitBitsPerCode) ->
    ClearDictionary = 1 bsl InitBitsPerCode,
    EndOfInformation = ClearDictionary + 1,
    %+1 to account for `clear' and `eoi'
    FirstCodeSize = InitBitsPerCode + 1,
    Dict = maps:from_list([{<<I>>, I} || I <- lists:seq(0, ClearDictionary - 1)]),
    Acc = #acc{
        out = <<>>,
        acc = [],
        acc_bits = 0
    },
    St = #st{
        dict = Dict#{
            clear => ClearDictionary,
            eoi => EndOfInformation
        },
        start_bits_per_code = InitBitsPerCode,
        bits_per_code = FirstCodeSize
    },
    encode(Data, <<>>, push_code(ClearDictionary, FirstCodeSize, Acc), St).

%% internal

encode(<<C, Data/binary>>, Buf, Acc, #st{dict = Dict, bits_per_code = BitsPerCode} = St) ->
    ExtBuf = <<Buf/binary, C>>,
    case maps:get(ExtBuf, Dict, []) of
        [] ->
            Code = maps:get(Buf, Dict),
            NewCode = map_size(Dict),
            Dict1 = Dict#{ExtBuf => NewCode},
            Acc1 = push_code(Code, BitsPerCode, Acc),
            case (1 bsl BitsPerCode) =< NewCode of
                true when BitsPerCode < 12 ->
                    encode(
                        Data,
                        <<C>>,
                        Acc1,
                        St#st{dict = Dict1, bits_per_code = BitsPerCode + 1}
                    );
                true when BitsPerCode =:= 12 ->
                    %% reinit table
                    %% TODO: DRY with compress/2
                    Acc2 = push_code(maps:get(clear, Dict), BitsPerCode, Acc1),
                    InitBitsPerCode = St#st.start_bits_per_code,
                    #{
                        clear := ClearDictionary,
                        eoi := EndOfInformation
                    } = Dict,
                    FirstCodeSize = InitBitsPerCode + 1,
                    NewDict = maps:from_list([{<<I>>, I} || I <- lists:seq(0, ClearDictionary - 1)]),
                    St1 = #st{
                        dict = NewDict#{
                            clear => ClearDictionary,
                            eoi => EndOfInformation
                        },
                        start_bits_per_code = InitBitsPerCode,
                        bits_per_code = FirstCodeSize
                    },
                    encode(Data, <<C>>, Acc2, St1);
                false ->
                    encode(Data, <<C>>, Acc1, St#st{dict = Dict1})
            end;
        _Found ->
            encode(Data, ExtBuf, Acc, St)
    end;
encode(<<>>, Buf, Acc, #st{dict = Dict, bits_per_code = BitsPerCode} = St) when
    byte_size(Buf) =/= 0
->
    encode(<<>>, <<>>, push_code(maps:get(Buf, Dict), BitsPerCode, Acc), St);
encode(<<>>, <<>>, Acc, #st{dict = Dict, bits_per_code = BitsPerCode}) ->
    End = maps:get(eoi, Dict),
    finalize(push_code(End, BitsPerCode, Acc)).

%% Why have they choosen such a weird way to encode bits to bytes? :(
%% Bits like this:
%% [#1, #2, #3], [#4, #5, #6], [#7, #8, #9], ...
%% Would be encoded as
%% <<
%%  #8, #9, #4, #5, #6, #1, #2, #3,  % 1st byte
%%  .., .., .., .., .., .., .., #7   % 2nd byte
%% >>
%% ...
push_code(Code, BitsPerCode, #acc{out = Out, acc = Acc, acc_bits = AccBits} = A) ->
    %% io:format(?MODULE_STRING " write(~w:~w)~n", [Code, BitsPerCode]),
    BitCode = <<Code:BitsPerCode>>,
    AccTotal = AccBits + BitsPerCode,
    case AccTotal >= 8 of
        true ->
            %% Wrap to the next byte
            NewAccBits = AccTotal rem 8,
            %% ToAdd can be more than 8 bits!
            %% TODO: optimize
            <<NewAcc:NewAccBits/bitstring, ToAdd/bitstring>> = BitCode,
            PushedBytes =
                lists:foldl(
                    fun(Bits, Output) ->
                        <<Output/bitstring, Bits/bitstring>>
                    end,
                    <<>>,
                    [ToAdd | Acc]
                ),
            NewOut =
                case PushedBytes of
                    <<B1, B2>> ->
                        <<Out/binary, B2, B1>>;
                    <<B>> ->
                        <<Out/binary, B>>
                end,
            %% <<_:(byte_size(Out))/binary, Pushed/binary>> = NewOut,
            %% io:format(?MODULE_STRING " push(~w)~n", [Pushed]),
            A#acc{
                out = NewOut,
                acc =
                    case NewAccBits =:= 0 of
                        true -> [];
                        false -> [NewAcc]
                    end,
                acc_bits = NewAccBits
            };
        false ->
            A#acc{acc = [BitCode | Acc], acc_bits = AccBits + BitsPerCode}
    end.

finalize(#acc{out = Out, acc_bits = 0}) ->
    Out;
finalize(#acc{out = Out, acc_bits = AccBits, acc = Acc}) ->
    Extra = <<0:(8 - AccBits)>>,
    lists:foldl(
        fun(Bits, Output) ->
            <<Output/bitstring, Bits/bitstring>>
        end,
        Out,
        [Extra | Acc]
    ).

%% bits(Bin) ->
%%     [I || <<I:1>> <= Bin].
