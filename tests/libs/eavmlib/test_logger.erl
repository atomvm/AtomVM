-module(test_logger).

-export([test/0]).

-include("logger.hrl").

test() ->
    ok = ?LOG_INFO(ok),
    ok = ?LOG_WARNING(ok),
    ok = ?LOG_ERROR(ok),
    ok = ?LOG_DEBUG(ok),

    ok.
