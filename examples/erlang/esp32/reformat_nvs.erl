-module(reformat_nvs).

-export([start/0]).

start() ->
    esp:nvs_reformat().
