# Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

"""Seeded boundary-heavy inputs and reductions for differential.erl."""

import json
import math
import random


def erlang(term):
    if isinstance(term, dict):
        kind, values = next(iter(term.items()))
        if kind == "tuple":
            return "{" + ",".join(map(erlang, values)) + "}"
        if kind == "map":
            return "#{" + ",".join(erlang(k) + "=>" + erlang(v) for k, v in values) + "}"
        if kind == "bits":
            return f"<<{values[0]}:{values[1]}>>"
        raise ValueError(f"Unknown term: {term}")
    if isinstance(term, float):
        value = repr(term)
        if "e" in value and "." not in value.split("e")[0]:
            value = value.replace("e", ".0e")
        return value
    return str(term)


def expression(case):
    return "differential:" + case["operation"] + "(" + ",".join(map(erlang, case["args"])) + ")"


def generate(seed, count):
    rng = random.Random(seed)
    numbers = [0, 1, -1, 0.0, -0.0, 0.1, -1.5, 5e-324, 1.7976931348623157e308]
    for power in [28, 31, 32, 53, 59, 60, 63, 64, 127, 128, 255]:
        for delta in [-1, 0, 1]:
            numbers.extend([2**power + delta, -(2**power + delta), float(2**power + delta)])

    def number():
        return rng.choice(numbers) if rng.randrange(4) else rng.randint(-(2**127), 2**127)

    def key():
        value = number()
        shape = rng.randrange(5)
        if shape == 0:
            return {"tuple": ["key", value]}
        if shape == 1:
            return {"map": [["value", value]]}
        if shape == 2:
            return {"bits": [rng.randrange(256), rng.choice([1, 3, 8, 9])]}
        return value

    # Guarantee the important asymmetric boundaries before random exploration.
    boundaries = [
        ("compare", [0.0, -0.0]),
        ("compare", [-0.0, 0.0]),
        ("compare", [2**53 + 1, float(2**53)]),
        ("compare", [float(2**53), 2**53 + 1]),
        ("compare", [-(2**63) - 1, float(-(2**63))]),
        ("maps", [0.0, -0.0, 0]),
        ("maps", [{"map": [["v", 0.0]]}, {"map": [["v", -0.0]]}, 33]),
        ("bits", [-257, 12, 3, "little"]),
        ("bits", [2**100 + 7, 101, 7, "big"]),
        ("arithmetic", ["idiv", -(2**63), -1]),
    ]
    result = []
    for index in range(count):
        if index < len(boundaries):
            operation, args = boundaries[index]
        elif index % 4 == 0:
            operation, args = "compare", [number(), number()]
        elif index % 4 == 1:
            a = key()
            b = rng.choice([a, key(), 0.0, -0.0, 1, 1.0])
            operation, args = "maps", [a, b, rng.choice([0, 1, 2, 30, 31, 32, 33, 34])]
        elif index % 4 == 2:
            value = rng.choice([-1, 0, 1, -257, 2**64 - 1, rng.getrandbits(120)])
            operation, args = "bits", [value, rng.choice([0, 1, 7, 8, 9, 31, 32, 63, 64, 65, 127, 128, 129, 255, -1]),
                                       rng.choice([0, 1, 3, 7, 8]), rng.choice(["big", "little"])]
        else:
            op = rng.choice(["add", "sub", "mul", "idiv", "mod", "shl", "shr", "band_op", "bxor_op", "fdiv"])
            a = rng.choice([0, -1, 2**63 - 1, -(2**63), rng.randint(-(2**119), 2**119)])
            b = rng.randint(-64, 64) if op in ["shl", "shr"] else rng.choice([0, -1, 3, rng.randint(-(2**119), 2**119)])
            operation, args = "arithmetic", [op, a, b]
        case = {"seed": seed, "index": index, "operation": operation, "args": args}
        case["expression"] = expression(case)
        result.append(case)
    return result


def reductions(value):
    if isinstance(value, int):
        choices = [0, 1, -1, value // 2]
        if value:
            boundary = 1 << (abs(value).bit_length() - 1)
            choices += [boundary, boundary - 1, -boundary, -boundary + 1]
        yield from (v for v in choices if abs(v) < abs(value))
    elif isinstance(value, float):
        yield from (v for v in [0.0, -0.0, 1.0, -1.0, value / 2]
                    if math.isfinite(v) and abs(v) < abs(value))
    elif isinstance(value, dict):
        kind, values = next(iter(value.items()))
        yield 0
        if kind == "tuple":
            yield {"tuple": []}
            for i, item in enumerate(values):
                yield item
                for smaller in reductions(item):
                    yield {kind: values[:i] + [smaller] + values[i + 1:]}
        elif kind == "map":
            yield {"map": []}
            for key, item in values:
                yield item
                for smaller in reductions(item):
                    yield {kind: [[key, smaller]]}
        elif kind == "bits" and values[1]:
            yield {"bits": [0, 0]}


def candidates(case):
    seen = {json.dumps(case["args"], sort_keys=True)}
    for index, value in enumerate(case["args"]):
        for smaller in reductions(value):
            if case["operation"] == "maps" and index == 2 and smaller < 0:
                continue
            args = case["args"][:index] + [smaller] + case["args"][index + 1:]
            encoded = json.dumps(args, sort_keys=True)
            if encoded not in seen:
                seen.add(encoded)
                candidate = {**case, "args": args}
                candidate["expression"] = expression(candidate)
                yield candidate
