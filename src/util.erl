-module(util).

-export([hnt_to_dc/1]).

-include("util.hrl").

hnt_to_dc(HNT) ->
    HNTInBones = HNT * ?BONES_PER_HNT,
    {ok, Price} = price_oracle:get_price(),
    HNTInUSD = ((HNTInBones / ?BONES_PER_HNT) * Price) / ?ORACLE_PRICE_SCALING_FACTOR,
    ceil(HNTInUSD * ?USD_TO_DC).
