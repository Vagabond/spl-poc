%% HNT satoshis
-define(BONES_PER_HNT, 100000000).

%% global constant
-define(ORACLE_PRICE_SCALING_FACTOR, 100000000).

%% global constant
-define(USD_TO_DC, 100000).

%% HNT to LWT, does not change
-define(HNT_TO_LWT_RATE, 1000).

%% in HNT-DC
-define(HotspotAddFee, 10 * ?USD_TO_DC).

%% LWT satoshis
-define(STONES_PER_LWT, ?BONES_PER_HNT div ?HNT_TO_LWT_RATE).

%% 10k HNT = 10K * 1000 LWT (in stones)
-define(ValidatorCost, 10000 * ?HNT_TO_LWT_RATE * ?STONES_PER_LWT).

%% Number of validators to reward
-define(CGCount, 2).

%% in Blocks
-define(ValidatorStakingPeriod, 250000).

%% in Blocks
-define(ValidatorStakeReturnBlocks, 10).
