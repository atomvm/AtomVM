-module(test_ordering_0).

-export([start/0]).

start() ->
    L1 = [
        {items1, [
            {item1, [{a, 0}, {b, 1}, {c, 2}, {d, 3}]},
            {item2, [{a, 0}, {b, 1}, {c, 2}, {d, 3}]},
            {item3, [{a, 0}, {b, 1}, {c, 2}, {d, 3}]}
        ]},

        {items2, [
            {item1, [{a, 0}, {b, 1}, {c, 2}, {d, 3}]},
            {list_to_atom("item2"), [{a, 0}, {b, 1}, {c, 2}, {d, 3}]},
            {item3, [{a, 0}, {b, 1}, {c, 2}, {d, 3}]}
        ]},

        {items3, [
            {item1, [{a, 0}, {b, 1}, {c, 2}, {d, 3}]},
            {item2, [{a, 0}, {b, 1}, {c, 2}, {d, 3}]},
            {item3, [{a, 0}, {b, 1}, {z, 2}, {d, 3}]}
        ]}
    ],
    L2 = [
        {items1, [
            {item1, [{a, factorial(1) - 1}, {b, factorial(1)}, {c, factorial(2)}, {d, 3}]},
            {item2, [{a, factorial(1) - 1}, {b, factorial(1)}, {c, factorial(2)}, {d, 3}]},
            {item3, [{a, factorial(1) - 1}, {b, factorial(0)}, {c, factorial(2)}, {d, 3}]}
        ]},

        {items2, [
            {item1, [{a, factorial(1) - 1}, {b, factorial(1)}, {c, factorial(2)}, {d, 3}]},
            {item2, [{a, factorial(1) - 1}, {b, factorial(0)}, {c, factorial(2)}, {d, 3}]},
            {item3, [{a, factorial(1) - 1}, {b, factorial(1)}, {c, factorial(2)}, {d, 3}]}
        ]},

        {items3, [
            {list_to_atom("item1"), [
                {a, factorial(1) - 1},
                {b, factorial(1)},
                {c, factorial(2)},
                {d, 3}
            ]},
            {item2, [{a, factorial(1) - 1}, {b, factorial(0)}, {c, factorial(2)}, {d, 3}]},
            {item3, [{a, factorial(1) - 1}, {b, factorial(0)}, {c, factorial(11)}, {d, 3}]}
        ]}
    ],
    bool_to_n(L1 > L2).

factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

bool_to_n(true) ->
    1;
bool_to_n(false) ->
    0.
