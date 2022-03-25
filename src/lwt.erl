%%%-------------------------------------------------------------------
%% @doc
%% This module models the LWT SubDAO contract
%% In its state it maintains:
%%
%% - Nonce of synced state with LWT chain
%%
%% - Holders of LWT tokens
%%
%% - Staked validator information
%%
%% - How much HNT needs to be burned next time the HNT contract is updated
%%
%% - Pending operations for the LWT chain
%%
%% @end
%%%-------------------------------------------------------------------

-module(lwt).

-behaviour(gen_server).

-include("util.hrl").

-record(state, {
    nonce = 0,
    holders = #{},
    %% val_address => {owner, init_height}
    validators = #{},
    %% pending amount of HNT that needs to be destroyed
    burns = 0,
    %% some stack of pending operations we need to do to the l2
    pending_operations = [],
    chain_ht = 0
}).

-export([init/1, handle_info/2, handle_cast/2, handle_call/3]).

-export([start_link/1]).

-export([transfer/3, convert_to_hnt/2, burn_to_dc/3]).

-export([oracle/0, update_from_chain/5]).

-export([stake_validator/2, unstake_validator/2]).

%% @private
start_link(LWTHolders) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [LWTHolders], []).

%% @doc Transfer `Amount' LWTs from `Payer' to `Payee'.
transfer(Payer, Payee, Amount) ->
    gen_server:call(?MODULE, {transfer, Payer, Payee, Amount}, infinity).

%% @doc Convert `Amount' of `Payer''s LWT into HNT at the HNT-LWT exchange rate.
convert_to_hnt(Payer, Amount) ->
    %% essentially we destroy some LWT and then send some of the HNT this contract
    %% controls to the Payer's address via the hnt contract api
    gen_server:call(?MODULE, {convert, Payer, Amount}, infinity).

%% @doc Burn `Amount' of `Payer''s LWTs into LWT-DCs in favor of `Payee'.
burn_to_dc(Burner, Burnee, Amount) ->
    %% destroy LWT and mark the equivalent amount of HNT to be burned next time we
    %% send an update to the hnt contract
    gen_server:call(?MODULE, {burn, Burner, Burnee, Amount}, infinity).

%% @doc Function to get the state of the LWT contract.
oracle() ->
    %% get the nonce and the l2 pending operations stack
    %% multiple attempts at oracling may give longer lists of pending operations
    %% but we can simply select the longest common prefix
    gen_server:call(?MODULE, oracle, infinity).

stake_validator(Owner, ValidatorAddress) ->
    gen_server:call(?MODULE, {stake_validator, Owner, ValidatorAddress}, infinity).

unstake_validator(Owner, ValidatorAddress) ->
    gen_server:call(?MODULE, {unstake_validator, Owner, ValidatorAddress}, infinity).

update_from_chain(Nonce, OpCount, RewardShares, Power, ChainHt) ->
    gen_server:call(?MODULE, {update, Nonce, OpCount, RewardShares, Power, ChainHt}, infinity).

%% @private
init([LWTHolders]) ->
    {ok, #state{holders = LWTHolders}}.

%% @private
handle_info(_Any, State) ->
    {noreply, State}.

%% @private
handle_cast(_Any, State) ->
    {noreply, State}.

%% @private
handle_call({stake_validator, Owner, ValidatorAddress}, _From, State = #state{holders = Holders}) ->
    case lists:member(ValidatorAddress, maps:keys(State#state.validators)) of
        true ->
            throw({reply, {error, already_staked}, State});
        false ->
            %% NOTE:
            %% - Immediate debit of LWT stake to prevent potential spending resulting
            %% in the stake being considered invalid
            %% - Also add the stake_validator instruction to pending_operations list
            case maps:get(Owner, Holders, 0) of
                OwnerLWT when OwnerLWT > ?ValidatorCost ->
                    NewHolders = debit(Owner, ?ValidatorCost, Holders),
                    NewPendingOps =
                        State#state.pending_operations ++
                            [{stake_validator, Owner, ValidatorAddress}],
                    {reply, ok, State#state{
                        pending_operations = NewPendingOps,
                        holders = NewHolders
                    }};
                _ ->
                    throw({reply, {error, insufficient_staking_balance}, State})
            end
    end;
handle_call(
    {unstake_validator, Owner, ValidatorAddress},
    _From,
    State = #state{chain_ht = ChainHeight}
) ->
    case lists:member(ValidatorAddress, maps:keys(State#state.validators)) of
        false ->
            throw({reply, {error, unknown_validator}, State});
        true ->
            Owner = maps:get(ValidatorAddress, State#state.validators),
            case (ChainHeight + ?ValidatorStakeReturnBlocks) >= ?ValidatorStakingPeriod of
                false ->
                    throw({reply, {error, insufficient_staking_period}, State});
                true ->
                    %% NOTE:
                    %% - Add the unstake_validator instruction to pending_operations list
                    %% - Do NOT immediately return the stake, wait for the unstake_validator
                    %% operation to succeed before crediting stake back to the owner
                    NewPendingOps =
                        State#state.pending_operations ++
                            [{unstake_validator, Owner, ValidatorAddress}],
                    {reply, ok, State#state{
                        pending_operations = NewPendingOps
                    }}
            end
    end;
handle_call({transfer, Payer, Payee, Amt}, _From, State) when Amt > 0 ->
    PayerHolding = maps:get(Payer, State#state.holders, 0),

    case PayerHolding =< Amt of
        %% cannot zero out payer on transfer
        true ->
            throw({reply, {error, insufficient_transfer_balance}, State});
        false ->
            NewHolders = credit(Payee, Amt, debit(Payer, Amt, State#state.holders)),
            {reply, ok, State#state{holders = NewHolders}}
    end;
handle_call({convert, Payer, Amount}, _From, State) ->
    case maps:get(Payer, State#state.holders, 0) >= Amount of
        false ->
            throw({reply, {error, insufficient_balance}, State});
        true ->
            ok
    end,
    NewHolders = debit(Payer, Amount, State#state.holders),
    %% module is a lazy identifier for this contract, would be a pubkey normally
    ok = hnt:transfer_hnt(?MODULE, Payer, Amount div ?HNT_TO_LWT_RATE),
    {reply, ok, State#state{holders = NewHolders}};
handle_call({burn, Burner, Burnee, LWTAmount}, _From, State) ->
    case maps:get(Burner, State#state.holders, 0) >= LWTAmount of
        false ->
            throw({reply, {error, insufficient_balance}, State});
        true ->
            ok
    end,
    %% calculate the amount of DC this LWT is worth by converting LWT to HNT and
    %% then consulting the HNT price oracle.
    HNT = LWTAmount div ?HNT_TO_LWT_RATE,
    DC = util:hnt_to_dc(HNT),
    lager:info("Burnee: ~p, LWT: ~p, HNT: ~p, DC: ~p", [Burnee, LWTAmount, HNT, DC]),

    {reply, ok, State#state{
        pending_operations = State#state.pending_operations ++ [{dc, Burnee, DC}],
        burns = State#state.burns + HNT
    }};
handle_call(oracle, _From, State) ->
    {reply, {ok, {State#state.nonce, State#state.pending_operations}}, State};
handle_call(
    {update, Nonce, OpCount, RewardShares, Power, ChainHt},
    _From,
    State = #state{nonce = Nonce, pending_operations = Ops}
) ->
    lager:debug("LWT got an update msg, Current Holders: ~p", [State#state.holders]),
    {ok, HNT} = hnt:update_from_l2(?MODULE, Nonce, Power, State#state.burns),
    %% ok, we got some HNT, now we need to convert that to LWT and disburse it according to the reward shares
    LWT = HNT * ?HNT_TO_LWT_RATE,
    TotalShares = maps:fold(
        fun(_K, V, Acc) ->
            Acc + V
        end,
        0,
        RewardShares
    ),
    RewardShare = LWT / TotalShares,
    NewHolders = maps:fold(
        fun(K, V, Acc) ->
            credit(K, trunc(V * RewardShare), Acc)
        end,
        State#state.holders,
        RewardShares
    ),
    lager:debug("New Holders: ~p", [NewHolders]),

    %% Now we need to remove the first `OpCount' operations from our pending operations stack, zero out our burns
    %% and increment our nonce
    {reply, ok, State#state{
        nonce = Nonce + 1,
        pending_operations = lists:sublist(Ops, OpCount + 1, length(Ops)),
        burns = 0,
        holders = NewHolders,
        chain_ht = ChainHt
    }}.

debit(Key, Amount, Map) ->
    maps:update_with(Key, fun(V) -> V - Amount end, Map).

credit(Key, Amount, Map) ->
    maps:update_with(Key, fun(V) -> V + Amount end, Amount, Map).
