Bakery
=====

An application to demonstrate Erlang's Open Telecom Platform with a bakery.

There are three OTP members in this package:

1. A supervisor (src/bakery\_sup) which launches the following two
2. A baker (src/baker.erl) which:
    1. Sells bread
    2. Puts bread in the oven to bake
    3. Has a stock of premade bread
    4. Sells from the stock first, then blocks while waiting for bread to bake
3. An oven (src/oven.erl) which:
    1. Bakes bread
    2. Calls back to the baker when bread is ready.  (This callback is defined by the
       caller to `oven:bake_bread`.  I.e. the baker passes a callback function)
    3. Bakes batches of bread (5 loaves per batch) because the oven is finite.

## Getting Started

1. Install erlang and rebar3
2. Build:

    ```bash
    $ rebar3 compile
    ```

3. Run:

    ```bash
    $ rebar3 shell
    ```

4. Sell bread:
    
    ```
    1> baker:sell_bread(3).
    ...
    {ok, [bread, bread, bread]}

