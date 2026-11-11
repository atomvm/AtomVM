# Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

"""Seeded boundary-heavy inputs and reductions for differential.erl."""

import json
import math
import random


def erlang(term):
    if isinstance(term, list):
        return "[" + ",".join(map(erlang, term)) + "]"
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


def float_boundaries():
    # Keep known float16 defects and float32 even/odd ties in short smoke runs.
    cases = [
        [6546.0, 16, 0, "big"],
        [1.0e-6, 16, 3, "little"],
        [6546.0001, 16, 0, "little"],
        [1.0 + 2.0**-24, 32, 0, "big"],
        [2.0**-150, 32, 3, "little"],
        [1.0 + 3 * 2.0**-24, 32, 7, "big"],
    ]
    for width, precision, min_exp, max_exp in [(16, 11, -14, 15), (32, 24, -126, 127)]:
        subnormal = 2.0**(min_exp - precision + 1)
        normal = 2.0**min_exp
        maximum = 2.0**(max_exp + 1) - 2.0**(max_exp - precision + 1)
        values = [0.0, 1.0, 2.25, subnormal, normal, maximum]
        ties = [subnormal / 2, normal - subnormal / 2,
                1.0 + 2.0**-precision, 1.0 + 3 * 2.0**-precision,
                2.0**(max_exp + 1) - 2.0**(max_exp - precision)]
        if width == 16:
            ties.append(6546.0)
        for tie in ties:
            values.extend([math.nextafter(tie, -math.inf), tie, math.nextafter(tie, math.inf)])
        for value in values:
            for sign in [1, -1]:
                for offset, endian in [(0, "big"), (3, "little")]:
                    cases.append([sign * value, width, offset, endian])
    # Invalid widths should raise the same exception, not escape the harness.
    cases.extend([[1.5, 8, 0, "big"], [1.5, 24, 3, "little"], [1.5, 0, 7, "big"]])
    return cases


def generate(seed, count, float_encoding=False):
    rng = random.Random(seed)
    encodings = float_boundaries()
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

    def sequence(kind, size, a, b):
        def step(*items):
            return {"tuple": list(items)}

        # Setup is immutable during reduction. Filler keys cannot alias probes.
        def filler(n):
            return {"tuple": ["filler", n]}

        setup = [step("sentinel", "untouched")]
        setup += [step(filler(n), n) for n in range(size)]
        steps = [step("get", a), step("put", a, "first"),
                 step("put", a, "replaced"), step("put", b, "second"),
                 step("get", a), step("get", b), step("selected", "second"),
                 step("erase", a), step("get", a), step("get", b),
                 step("erase", a), step("put", a, "reinserted")]
        steps += [step("put", a, 0), step("put", b, 0.0),
                  step("selected", 0), step("selected", 0.0)]
        if kind == "map_sequence":
            steps += [step("update", "missing", "bad"), step("update", a, "updated"),
                      step("take", a), step("take", a), step("put", a, "reinserted")]
        # Near-boundary setups grow through 32, shrink back, and regrow.
        steps += [step("put", filler(n), n) for n in range(size, size + 4)]
        steps += [step("erase", filler(n)) for n in range(max(0, size - 4), size + 4)]
        steps += [step("put", filler(n), -n) for n in range(max(0, size - 4), size + 4)]
        keys = [a, b, "missing", filler(0), filler(size + 3)]
        operations = ["put", "get", "erase", "selected"]
        if kind == "map_sequence":
            operations += ["update", "take"]
        for _ in range(rng.randint(8, 24)):
            op, k = rng.choice(operations), rng.choice(keys)
            if op in ["put", "update"]:
                steps.append(step(op, k, number()))
            elif op == "selected":
                steps.append(step(op, rng.choice(["second", "undefined", 0, 0.0])))
            else:
                steps.append(step(op, k))
        steps += [step("get", "sentinel"), step("clear"), step("get", a),
                  step("put", b, "undefined"), step("get", b),
                  step("selected", "undefined"), step("erase", b)]
        return [setup, steps]

    def receives(a, b, empty=False):
        def step(*items):
            return {"tuple": list(items)}

        setup = [] if empty else [
            "sentinel", b, a, step("tag", a), step("tag", b), 0, "bad",
            step(), {"map": []}, {"map": [["key", "wanted"]]},
            {"bits": [2, 8]}, "false", "fallback", step("last", "wanted"),
        ]
        steps = [step("exact", "missing"), step("exact", a), step("exact", a),
                 step("send", a), step("exact", a), step("tag", "tag"), step("tag", "tag")]
        guards = ["division", "tuple", "map", "binary", "boolean", "alternatives"]
        steps += [step("guard", kind) for kind in guards]
        steps += [step("send", step("tag", a)), step("tag", "tag"),
                  step("clauses", a), step("any")]
        messages = [a, b, step("tag", a), step("tag", b), "fallback", "sentinel", key()]
        for _ in range(rng.randint(4, 12)):
            op = rng.choice(["send", "exact", "tag", "guard", "clauses", "any"])
            if op == "any":
                steps.append(step(op))
            else:
                value = rng.choice(guards) if op == "guard" else (
                    rng.choice(["tag", a, b]) if op == "tag" else rng.choice(messages))
                steps.append(step(op, value))
        steps.append(step("any"))
        return [setup, steps]

    def binary_scopes(size):
        def step(*items):
            return {"tuple": list(items)}

        def scope(start, length):
            return step("scope", step(start, length))

        a, b, c, pad = rng.sample(range(256), 4)
        motif = [a, b, a, b, c]
        setup = ([pad] + (motif * 20)[:size - 2] + [pad]) if size else []
        short = {"bits": [a * 256 + b, 16]}
        long = {"bits": [a * 65536 + b * 256 + c, 24]}
        patterns = [short, long]

        def queries(length):
            start = rng.randrange(length + 1)
            end = rng.randrange(start, length + 1)
            positive = scope(start, end - start)
            negative = scope(end, start - end)
            flags = rng.choice([[], ["global"], ["global", "trim"], ["global", "trim_all"]])
            return [step("match", patterns, [positive]),
                    step("match", patterns[::-1], [negative]),
                    step("match", patterns[::-1], [positive]),
                    step("split", patterns, flags + [positive]),
                    step("split", patterns, [negative] + flags),
                    step("split", patterns[::-1], flags + [positive])]

        steps = [step("match", patterns, []), step("split", patterns, ["global"])]
        steps += queries(size)
        for scopes in [[scope(0, 0), scope(0, size)], [scope(0, size), scope(0, 0)]]:
            steps += [step("match", patterns, scopes), step("split", patterns, ["global"] + scopes)]
        steps += [step("part", -1, 1), step("match", patterns, [scope(0, size + 1)])]
        start, length = (1, size - 2) if size else (0, 0)
        steps.append(step("part", start, length))
        start = min(1, length)
        length -= start
        steps.append(step("part", start, length))
        steps += queries(length)
        steps.append(step("append", motif))
        length += len(motif)
        steps += queries(length)
        steps += [step("part", length, -3), step("match", patterns, [])]
        if rng.randrange(4) == 0:
            steps.append(step("split", [short, {"bits": [0, 0]}], ["global"]))
        return [setup, steps]

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
    boundaries += [("float_bits", args) for args in encodings[:6]]
    pairs = [(1, 1.0), (0.0, -0.0),
             ({"tuple": ["key", 1]}, {"tuple": ["key", 1.0]}),
             ({"map": [["v", 0.0]]}, {"map": [["v", -0.0]]})]
    if not float_encoding:
        for size, (a, b) in zip([0, 30, 31, 32], pairs):
            for kind in ["map_sequence", "dictionary_sequence"]:
                boundaries.append((kind, sequence(kind, size, a, b)))
        for index, (a, b) in enumerate(pairs):
            boundaries.append(("receive_sequence", receives(a, b, empty=index == 3)))
        for size in [0, 7, 64, 65]:
            boundaries.append(("binary_sequence", binary_scopes(size)))
    if float_encoding:
        boundaries = [("float_bits", args) for args in encodings]
    result = []
    for index in range(count):
        if index < len(boundaries):
            operation, args = boundaries[index]
        elif float_encoding or index % 9 == 4:
            value, width = rng.choice(encodings[:-3])[:2]
            operation, args = "float_bits", [value, width, rng.choice([0, 1, 3, 7]), rng.choice(["big", "little"])]
        elif index % 9 == 0:
            operation, args = "compare", [number(), number()]
        elif index % 9 == 1:
            a = key()
            b = rng.choice([a, key(), 0.0, -0.0, 1, 1.0])
            operation, args = "maps", [a, b, rng.choice([0, 1, 2, 30, 31, 32, 33, 34])]
        elif index % 9 == 2:
            value = rng.choice([-1, 0, 1, -257, 2**64 - 1, rng.getrandbits(120)])
            operation, args = "bits", [value, rng.choice([0, 1, 7, 8, 9, 31, 32, 63, 64, 65, 127, 128, 129, 255, -1]),
                                       rng.choice([0, 1, 3, 7, 8]), rng.choice(["big", "little"])]
        elif index % 9 in [5, 6]:
            operation = "map_sequence" if index % 9 == 5 else "dictionary_sequence"
            args = sequence(operation, rng.choice([0, 30, 31, 32, 33]), key(), key())
        elif index % 9 == 7:
            operation, args = "receive_sequence", receives(key(), key(), empty=rng.randrange(4) == 0)
        elif index % 9 == 8:
            operation, args = "binary_sequence", binary_scopes(rng.choice([7, 31, 32, 63, 64, 65, 96]))
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
    if case["operation"] in ["map_sequence", "dictionary_sequence", "receive_sequence", "binary_sequence"]:
        setup, steps = case["args"]
        # Preserve setup and literal operation payloads. Deleting a state change
        # can expose a timeout or range error, but needs no result reference repair.
        width = max(1, len(steps) // 2)
        while width:
            for start in range(0, len(steps) - width + 1, width):
                remaining = steps[:start] + steps[start + width:]
                if not remaining:
                    continue
                candidate = {**case, "args": [setup, remaining]}
                candidate["expression"] = expression(candidate)
                yield candidate
            width //= 2
        return
    seen = {json.dumps(case["args"], sort_keys=True)}
    for index, value in enumerate(case["args"]):
        if case["operation"] == "float_bits" and index != 0:
            continue
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
