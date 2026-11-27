# Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

"""Public CLI integration tests; requires a built AtomVM and host OTP/Elixir."""

import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

import lean_runtime
from generated import generate


# An ordered-list oracle, deliberately using no queue operations or probe helpers.
QUEUE_MODEL = """
[path] = System.argv()
read = fn name -> JSON.decode!(File.read!(Path.join(path, name))) end
decode = fn decode, value ->
  case value do
    %{"tuple" => xs} -> xs |> Enum.map(&decode.(decode, &1)) |> List.to_tuple()
    %{"map" => xs} -> Map.new(xs, fn [k,v] -> {decode.(decode,k), decode.(decode,v)} end)
    %{"bits" => [v,n]} -> <<v::size(n)>>
    xs when is_list(xs) -> Enum.map(xs, &decode.(decode, &1))
    atom when is_binary(atom) -> String.to_atom(atom)
    n -> n
  end
end
snapshot = fn q, saved ->
  peek = fn xs -> case xs do [] -> :empty; [v|_] -> {:value,v} end end
  {length(q), q === [], q, peek.(q), peek.(Enum.reverse(q)), saved}
end
model = fn [setup, steps] ->
  {rows, _, _} = Enum.reduce(steps, {[], setup, setup}, fn step, {rows,q,saved} ->
    ok = fn v, next, retained -> {{:ok,v}, next, retained} end
    bad = fn reason -> {{:exception,:error,reason}, q, saved} end
    {result, next, retained} = case step do
      {:in,v} -> next = q ++ [v]; ok.(next,next,saved)
      {:in_r,v} -> next = [v|q]; ok.(next,next,saved)
      {op} when op in [:out,:out_r,:drop,:drop_r,:get,:get_r] ->
        xs = if op in [:out_r,:drop_r,:get_r], do: Enum.reverse(q), else: q
        case xs do
          [] when op in [:out,:out_r] -> ok.(:empty,q,saved)
          [] -> bad.(:empty)
          [v|rest] ->
            next = if op in [:out_r,:drop_r], do: Enum.reverse(rest), else: rest
            cond do
              op in [:get,:get_r] -> ok.(v,q,saved)
              op in [:out,:out_r] -> ok.({:value,v},next,saved)
              true -> ok.(next,next,saved)
            end
        end
      {:reverse} -> next = Enum.reverse(q); ok.(next,next,saved)
      {:split,n} when is_integer(n) and n >= 0 and n <= length(q) ->
        {left,right} = Enum.split(q,n); ok.({left,right},left,saved)
      {:split,_} -> bad.(:badarg)
      {:join,:left,items} -> next = items ++ q; ok.(next,next,saved)
      {:join,:right,items} -> next = q ++ items; ok.(next,next,saved)
      {:member,v} -> ok.(Enum.any?(q, &(&1 === v)),q,saved)
      {op,v} when op in [:delete,:delete_r] ->
        xs = if op === :delete_r, do: Enum.reverse(q), else: q
        index = Enum.find_index(xs, &(&1 === v))
        xs = if index === nil, do: xs, else: List.delete_at(xs,index)
        next = if op === :delete_r, do: Enum.reverse(xs), else: xs
        ok.(next,next,saved)
      {:save} -> ok.(:saved,q,q)
      {:restore} -> ok.(:restored,saved,saved)
      {:gc} -> ok.(true,q,saved)
    end
    {[{result,snapshot.(next,retained)}|rows],next,retained}
  end)
  {:ok,{snapshot.(setup,setup),Enum.reverse(rows)}}
end
expected = Enum.map(read.("cases.json"), fn entry ->
  model.(decode.(decode,entry["args"]))
end)
from_etf = fn s -> :erlang.binary_to_term(Base.decode64!(s)) end
true = Enum.map(read.("manifest.json")["expected"],from_etf) === expected
actual = Enum.map(read.("actuals.json"), fn row ->
  "ok" = row["status"]
  from_etf.(row["value"])
end)
true = actual === expected
IO.puts("Checked #{length(expected)} queue traces against independent list model")
"""


# List-based callback contracts. No queue API or differential helper is invoked.
QUEUE_CALLBACK_MODEL = """
[path] = System.argv()
read = fn name -> JSON.decode!(File.read!(Path.join(path,name))) end
decode = fn decode, value ->
  case value do
    %{"tuple" => xs} -> xs |> Enum.map(&decode.(decode,&1)) |> List.to_tuple()
    %{"map" => xs} -> Map.new(xs, fn [k,v] -> {decode.(decode,k),decode.(decode,v)} end)
    %{"bits" => [v,n]} -> <<v::size(n)>>
    xs when is_list(xs) -> Enum.map(xs,&decode.(decode,&1))
    s when is_binary(s) -> String.to_atom(s)
    n -> n
  end
end
seed = {:seed,<<5::3>>}
model = fn [op,_layout,items,mode] ->
  xs = if op === :delete_with_r, do: Enum.reverse(items), else: items
  size = length(xs)
  {result, count} = cond do
    mode in [:non_function,:wrong_arity] -> {{:exception,:error,:badarg},0}
    is_tuple(mode) and elem(mode,0) === :fail and elem(mode,2) <= size ->
      {{:exception,elem(mode,1),{:callback_failed,<<5::3>>}},elem(mode,2)}
    mode === :invalid_return and size >= 2 ->
      {{:exception,:error,{:case_clause,:not_boolean}},2}
    op in [:any,:all] ->
      {answer,n} = case {op,mode,size} do
        {:any,_,0} -> {false,0}
        {:all,_,0} -> {true,0}
        {:any,m,_} when m in [:all_true,:short] -> {true,1}
        {:all,:all_false,_} -> {false,1}
        {:all,:short,_} -> {false,1}
        {:any,_,_} -> {false,size}
        {:all,_,_} -> {true,size}
      end
      {{:ok,answer},n}
    op === :fold ->
      {{:ok,Enum.reduce(items,seed,fn x,acc -> {x,acc} end)},size}
    op in [:delete_with,:delete_with_r] ->
      index = if mode === :normal, do: Enum.find_index(xs,&(&1 === 1)), else: nil
      kept = if index === nil, do: xs, else: List.delete_at(xs,index)
      output = if op === :delete_with_r, do: Enum.reverse(kept), else: kept
      {{:ok,output},if(index === nil,do: size,else: index+1)}
    true ->
      output = Enum.flat_map(items, fn x ->
        case {mode,op,x} do
          {:all_false,_,_} -> []
          {:transform,_,{:drop,_}} -> []
          {:transform,:filter,{:nil,_}} -> []
          {:transform,:filter,{:one,v}} -> [{:wrapped,v}]
          {:transform,:filter,{:many,v}} -> [{:left,v},{:right,v}]
          {:transform,:filtermap,{tag,_}} when tag in [:nil,:one,:many] -> [{:mapped,x}]
          _ -> [x]
        end
      end)
      {{:ok,output},size}
  end
  visited = Enum.take(xs,count)
  trace = cond do
    op in [:any,:all] -> {count,true}
    op === :fold ->
      {_,rows} = Enum.reduce(visited,{seed,[]},fn x,{acc,rows} -> {{x,acc},rows++[{x,acc}]} end)
      rows
    true -> visited
  end
  {result,trace,items}
end
cases = read.("cases.json")
specs = Enum.map(cases, &decode.(decode,&1["queue_callback"]))
expected = Enum.map(specs, &{:ok,model.(&1)})
from_etf = fn s -> :erlang.binary_to_term(Base.decode64!(s)) end
true = Enum.map(read.("manifest.json")["expected"],from_etf) === expected
true = Enum.map(read.("actuals.json"),fn row ->
  "ok" = row["status"]
  from_etf.(row["value"])
end) === expected

# Verify raw unordered visits, not just the helper's validity boolean. A valid
# trace consumes each input occurrence at most once, using exact equality.
valid_calls = fn calls,items ->
  Enum.reduce_while(calls,items,fn x,left ->
    case Enum.find_index(left,&(&1 === x)) do
      nil -> {:halt,:invalid}
      index -> {:cont,List.delete_at(left,index)}
    end
  end) !== :invalid
end
unordered = Enum.filter(specs, fn [op|_] -> op in [:any,:all] end)
logs = ["proofs.log"] ++ Enum.map(cases,&(&1["module"]<>".log"))
raw = Enum.map(logs,fn name ->
  File.read!(Path.join(path,name)) |> String.split("\\n") |> Enum.flat_map(fn line ->
    case String.split(line,"LYNX_QUEUE_TRACE ",parts: 2) do
      ["",bytes] -> [bytes |> JSON.decode!() |> :erlang.list_to_binary() |> :erlang.binary_to_term()]
      _ -> []
    end
  end)
end)
check_raw = fn rows, selected ->
  true = Enum.frequencies(Enum.map(rows,fn {op,layout,items,mode,_,_} -> [op,layout,items,mode] end))
         === Enum.frequencies(selected)
  Enum.each(rows,fn {op,layout,items,mode,outcome,calls} ->
    {^outcome,{count,true},^items} = model.([op,layout,items,mode])
    true = length(calls) === count and valid_calls.(calls,items)
  end)
end
[otp_raw|target_raw] = raw
check_raw.(otp_raw,unordered)
check_raw.(List.flatten(target_raw),unordered)
IO.puts("Checked #{length(cases)} callback contracts and #{length(unordered)*2} raw unordered traces")
"""


# Independent exact-key/list contracts, with no sets or observer API calls.
SET_CALLBACK_MODEL = """
[path] = System.argv()
read = fn name -> JSON.decode!(File.read!(Path.join(path,name))) end
decode = fn decode, value ->
  case value do
    %{"tuple" => xs} -> xs |> Enum.map(&decode.(decode,&1)) |> List.to_tuple()
    %{"map" => xs} -> Map.new(xs, fn [k,v] -> {decode.(decode,k),decode.(decode,v)} end)
    %{"bits" => [v,n]} -> <<v::size(n)>>
    xs when is_list(xs) -> Enum.map(xs,&decode.(decode,&1))
    s when is_binary(s) -> String.to_atom(s)
    n -> n
  end
end
members = fn xs -> Map.new(xs,&{&1,:present}) end
snapshot = fn xs ->
  m = members.(xs)
  n = map_size(m)
  {n,n === 0,n,m}
end
seed = {{:seed,<<5::3>>},7,%{}}
advance = fn x,{tag,n,seen} -> {tag,n+1,Map.put(seen,x,:present)} end
model = fn [op,_history,items,mode] ->
  keys = items |> members.() |> Map.keys()
  size = length(keys)
  {outcome,count} = cond do
    mode in [:non_function,:wrong_arity] -> {{:exception,:error,:function_clause},0}
    is_tuple(mode) and elem(mode,0) === :fail and elem(mode,2) <= size ->
      {{:exception,elem(mode,1),{:callback_failed,<<5::3>>}},elem(mode,2)}
    mode === :invalid_return and size >= 2 ->
      reason = if op === :filter, do: :bad_filter, else: :case_clause
      {{:exception,:error,{reason,:not_boolean}},2}
    op === :fold -> {{:ok,{{:seed,<<5::3>>},7+size,members.(keys)}},size}
    true ->
      values = Enum.flat_map(keys, fn x ->
        case {op,mode,x} do
          {:map,:identity,_} -> [x]
          {:map,:constant,_} -> [:collision]
          {:map,_,{:merge,_}} -> [:collision]
          {:map,_,v} when v === 1 or v === 1.0 -> [v]
          {:map,_,_} -> [{:wrapped,x}]
          {_,:all_false,_} -> []
          {_,:all_true,_} -> [x]
          {:filter,_,1} -> []
          {:filter,_,_} -> [x]
          {:filtermap,_,{:drop,_}} -> []
          {:filtermap,_,{:keep,_}} -> [x]
          {:filtermap,_,{:merge,_}} -> [:collision]
          {:filtermap,_,_} -> [{:wrapped,x}]
        end
      end)
      {{:ok,snapshot.(values)},size}
  end
  {outcome,{count,true},snapshot.(keys)}
end
cases = read.("cases.json")
specs = Enum.map(cases,&decode.(decode,&1["set_callback"]))
expected = Enum.map(specs,&{:ok,model.(&1)})
from_etf = fn s -> :erlang.binary_to_term(Base.decode64!(s)) end
oracle = Enum.map(read.("manifest.json")["expected"],from_etf)
actual = Enum.map(read.("actuals.json"),fn row ->
  "ok" = row["status"]
  from_etf.(row["value"])
end)

# Independently check each VM's arbitrary visitation order, including fold's
# accumulator inputs. Repeated visits cannot hide behind a true helper boolean.
logs = ["proofs.log"] ++ Enum.map(cases,&(&1["module"]<>".log"))
raw = Enum.map(logs,fn name ->
  File.read!(Path.join(path,name)) |> String.split("\\n") |> Enum.flat_map(fn line ->
    case String.split(line,"LYNX_SET_TRACE ",parts: 2) do
      ["",bytes] -> [bytes |> JSON.decode!() |> :erlang.list_to_binary() |> :erlang.binary_to_term()]
      _ -> []
    end
  end)
end)
check_raw = fn rows, reports ->
  true = Enum.frequencies(Enum.map(rows,fn {op,history,items,mode,_,_} -> [op,history,items,mode] end))
         === Enum.frequencies(specs)
  outcomes = Map.new(Enum.zip(specs,reports),fn {spec,{:ok,{outcome,_,_}}} -> {spec,outcome} end)
  Enum.each(rows,fn {op,history,items,mode,outcome,calls} ->
    spec = [op,history,items,mode]
    {_,{count,true},_} = model.(spec)
    true = outcome === Map.fetch!(outcomes,spec)
    visits = if op === :fold, do: Enum.map(calls,&elem(&1,0)), else: calls
    true = length(visits) === count and map_size(members.(visits)) === count
    true = Enum.all?(visits,&Map.has_key?(members.(items),&1))
    if op === :fold do
      last = Enum.reduce(calls,seed,fn {x,acc},previous ->
        true = acc === previous
        advance.(x,previous)
      end)
      case outcome do
        {:ok,result} -> true = result === last
        {:exception,_,_} -> :ok
      end
    end
  end)
end
[otp_raw|target_raw] = raw
check_raw.(otp_raw,oracle)
check_raw.(List.flatten(target_raw),actual)
IO.puts("Checked #{length(cases)*2} raw set traces and fold accumulator threading")
true = oracle === expected
IO.puts("Checked #{length(cases)} OTP set callback contracts")
true = actual === expected
IO.puts("Checked #{length(cases)} AtomVM set callback contracts")
"""


# Independent ordered equations; do not call any of the four lists fold APIs.
LIST_FOLD_MODEL = """
[path] = System.argv()
read = fn name -> JSON.decode!(File.read!(Path.join(path,name))) end
from_etf = fn s -> :erlang.binary_to_term(Base.decode64!(s)) end
decode = fn decode, value ->
  case value do
    %{"tuple" => xs} -> xs |> Enum.map(&decode.(decode,&1)) |> List.to_tuple()
    %{"map" => xs} -> Map.new(xs, fn [k,v] -> {decode.(decode,k),decode.(decode,v)} end)
    %{"bits" => [v,n]} -> <<v::size(n)>>
    xs when is_list(xs) -> Enum.map(xs,&decode.(decode,&1))
    s when is_binary(s) -> String.to_atom(s)
    n -> n
  end
end
model = fn [op,shape,items,mode] ->
  captured = {:captured,items,<<5::3>>}
  seed = {:seed,7,<<3::2>>}
  right = op in [:foldr,:mapfoldr]
  mapping = op in [:mapfoldl,:mapfoldr]
  input = case shape do
    :proper -> items
    :improper -> Enum.reduce(Enum.reverse(items),:tail,fn x,t -> [x|t] end)
    :non_list -> :not_a_list
  end
  {outcome,trace} = if mode in [:non_function,:wrong_arity] or
                         shape === :non_list or (right and shape === :improper) do
    # foldl's guarded entry uses a case on the original argument; recursive
    # malformed tails and the other entry points fail a function clause.
    reason = if op === :foldl and shape === :non_list and
                   mode not in [:non_function,:wrong_arity],
                do: {:case_clause,:not_a_list}, else: :function_clause
    {{:exception,:error,reason},[]}
  else
    visits = if right, do: Enum.reverse(items), else: items
    state = Enum.reduce_while(Enum.with_index(visits,1),{seed,[],[]},
      fn {x,n},{acc,ys,trace} ->
        trace = [{x,acc,captured}|trace]
        case mode do
          {:fail,kind,^n} ->
            {:halt,{:failed,{:exception,kind,{:callback_failed,x,<<5::3>>}},trace}}
          {:bad_return,bad,^n} ->
            {:halt,{:failed,{:exception,:error,{:badmatch,bad}},trace}}
          _ ->
            next = if mode === :replace_acc and n === 2,
                      do: {:replacement,<<5::3>>}, else: {x,acc}
            {:cont,{next,[{:mapped,x,acc,captured}|ys],trace}}
        end
      end)
    case state do
      {:failed,failure,trace} -> {failure,Enum.reverse(trace)}
      {acc,ys,trace} ->
        outcome = cond do
          shape !== :proper -> {:exception,:error,:function_clause}
          mapping -> {:ok,{if(right,do: ys,else: Enum.reverse(ys)),acc}}
          true -> {:ok,acc}
        end
        {outcome,Enum.reverse(trace)}
    end
  end
  {outcome,trace,input,captured}
end
cases = read.("cases.json")
expected = Enum.map(cases,&{:ok,model.(decode.(decode,&1["list_fold"]))})
oracle = Enum.map(read.("manifest.json")["expected"],from_etf)
actual = Enum.map(read.("actuals.json"),fn row ->
  "ok" = row["status"]
  from_etf.(row["value"])
end)
Enum.each([{"OTP",oracle},{"AtomVM",actual}],fn {vm,rows} ->
  true = length(rows) === length(expected)
  failures = Enum.zip(rows,expected) |> Enum.with_index() |> Enum.flat_map(fn
    {{a,e},n} when a !== e -> [n]
    _ -> []
  end)
  IO.puts("#{vm}: #{length(rows)-length(failures)}/#{length(rows)} independent ordered fold contracts")
  if failures !== [], do: raise("#{vm} contract failures at indices #{inspect(failures,limit: :infinity)}")
end)
"""


# Independent sequential contracts, including flatmap's deferred append checks.
LIST_CALLBACK_MODEL = """
[path] = System.argv()
read = fn name -> JSON.decode!(File.read!(Path.join(path,name))) end
from_etf = fn s -> :erlang.binary_to_term(Base.decode64!(s)) end
decode = fn decode, value ->
  case value do
    %{"tuple" => xs} -> List.to_tuple(for x <- xs, do: decode.(decode,x))
    %{"map" => xs} -> Map.new(for [k,v] <- xs, do: {decode.(decode,k),decode.(decode,v)})
    %{"bits" => [v,n]} -> <<v::size(n)>>
    xs when is_list(xs) -> for x <- xs, do: decode.(decode,x)
    s when is_binary(s) -> String.to_atom(s)
    n -> n
  end
end
build = fn build, xs, tail ->
  case xs do [] -> tail; [x|rest] -> [x|build.(build,rest,tail)] end
end
# Validate each returned prefix only after the remaining callbacks complete.
append = fn append, xs, tail ->
  case xs do
    [] -> {:ok,tail}
    [x|rest] -> case append.(append,rest,tail) do
      {:ok,ys} -> {:ok,[x|ys]}
      error -> error
    end
    _ -> {:exception,:error,:badarg}
  end
end
model = fn [op,shape,items,mode] ->
  captured = {:captured,items,<<5::3>>}
  tail = if shape === :proper, do: [], else: :tail
  input = if shape === :non_list, do: :not_a_list, else: build.(build,items,tail)
  fail = fn kind,x -> {:raised,{:exception,kind,{:callback_failed,x,<<5::3>>}}} end
  callback = fn x,n ->
    mapped = {:mapped,n,x,captured}
    default = fn -> case op do
      :map -> mapped
      :foreach -> %{ignored: mapped}
      :flatmap -> case rem(n,3) do 1 -> []; 2 -> [mapped]; 0 -> [mapped,{:extra,x}] end
      :filter -> rem(n,3) !== 2
      :filtermap -> case rem(n,3) do 1 -> false; 2 -> {true,mapped}; 0 -> true end
      :search -> false
      :dropwhile -> true
    end end
    case mode do
      {:fail,kind,^n} -> fail.(kind,x)
      {:return,value,^n} -> {:returned,value}
      {:return_then_fail,_,_,kind,^n} -> fail.(kind,x)
      {:return_then_fail,value,^n,_,_} -> {:returned,value}
      {:improper_return,^n} -> {:returned,[x|:returned_tail]}
      {:stop,at} when n > at -> {:raised,{:exception,:error,{:unnecessary_callback,x}}}
      {:stop,at} -> {:returned,if(op === :search,do: n === at,else: n !== at)}
      :all_true -> {:returned,true}
      :all_false -> {:returned,false}
      :exact -> {:returned,if(op === :dropwhile,do: x !== 1,else: x === 1)}
      _ -> {:returned,default.()}
    end
  end
  bad_input = fn value ->
    reason = if op === :filter, do: {:bad_generator,value}, else: :function_clause
    {:exception,:error,reason}
  end
  run = fn run, rest, n, output, trace ->
    case rest do
      [] ->
        outcome = if shape === :improper do
          bad_input.(:tail)
        else
          case op do
            :foreach -> {:ok,:ok}
            :search -> {:ok,false}
            :dropwhile -> {:ok,[]}
            :flatmap -> Enum.reduce_while(output,{:ok,[]},fn xs,{:ok,ys} ->
              case append.(append,xs,ys) do
                {:ok,_}=ok -> {:cont,ok}
                error -> {:halt,error}
              end
            end)
            _ -> {:ok,Enum.reverse(output)}
          end
        end
        {outcome,trace}
      [x|xs] ->
        trace = [{x,n,captured}|trace]
        case callback.(x,n) do
          {:raised,error} -> {error,trace}
          {:returned,value} ->
            case {op,value} do
              {:search,true} -> {{:ok,{:value,x}},trace}
              {:dropwhile,false} -> {{:ok,build.(build,rest,tail)},trace}
              {pred,v} when pred in [:filter,:filtermap,:search,:dropwhile] ->
                cond do
                  pred === :filtermap and is_tuple(v) and tuple_size(v) === 2 and elem(v,0) === true ->
                    run.(run,xs,n+1,[elem(v,1)|output],trace)
                  v === true or v === false ->
                    next = if pred in [:filter,:filtermap] and v === true, do: [x|output], else: output
                    run.(run,xs,n+1,next,trace)
                  true ->
                    reason = if pred === :filter, do: {:bad_filter,v}, else: {:case_clause,v}
                    {{:exception,:error,reason},trace}
                end
              _ -> run.(run,xs,n+1,[value|output],trace)
            end
        end
    end
  end
  {outcome,trace} = cond do
    mode in [:non_function,:wrong_arity] -> {{:exception,:error,:function_clause},[]}
    shape === :non_list and op === :map -> {{:exception,:error,{:case_clause,:not_a_list}},[]}
    shape === :non_list -> {bad_input.(:not_a_list),[]}
    true -> run.(run,items,1,[],[])
  end
  {outcome,Enum.reverse(trace),input,captured}
end
cases = read.("cases.json")
expected = for c <- cases, do: {:ok,model.(decode.(decode,c["list_callback"]))}
oracle = for s <- read.("manifest.json")["expected"], do: from_etf.(s)
actual = for row <- read.("actuals.json") do
  "ok" = row["status"]
  from_etf.(row["value"])
end
Enum.each([{"OTP",oracle},{"AtomVM",actual}],fn {vm,rows} ->
  true = length(rows) === length(expected)
  failures = for {{a,e},n} <- Enum.with_index(Enum.zip(rows,expected)), a !== e, do: n
  IO.puts("#{vm}: #{length(rows)-length(failures)}/#{length(rows)} independent unary list contracts")
  if failures !== [], do: raise("#{vm} contract failures at indices #{inspect(failures,limit: :infinity)}")
end)
"""


# Paired traversal model: no zip/zipwith API or observer is used to derive results.
LIST_ZIPWITH_MODEL = """
[path] = System.argv()
read = fn name -> JSON.decode!(File.read!(Path.join(path,name))) end
from_etf = fn s -> :erlang.binary_to_term(Base.decode64!(s)) end
decode = fn decode, value ->
  case value do
    %{"tuple" => xs} -> List.to_tuple(for x <- xs, do: decode.(decode,x))
    %{"map" => xs} -> Map.new(xs,fn [k,v] -> {decode.(decode,k),decode.(decode,v)} end)
    %{"bits" => [v,n]} -> <<v::size(n)>>
    xs when is_list(xs) -> for x <- xs, do: decode.(decode,x)
    s when is_binary(s) -> String.to_atom(s)
    n -> n
  end
end
build = fn build, xs, tail ->
  case xs do [] -> tail; [x|rest] -> [x|build.(build,rest,tail)] end
end
input = fn shape, items, tail ->
  case shape do
    :proper -> items
    :improper -> build.(build,items,tail)
    :non_list -> {:not_a_list,tail}
  end
end
model = fn [ls,left,rs,right,mode] ->
  left = input.(ls,left,:left_tail)
  right = input.(rs,right,:right_tail)
  captured = {:captured,left,right,<<5::3>>}
  invalid = mode in [:non_function,:arity_zero,:arity_one,:arity_three]
  error = fn reason -> {:exception,:error,reason} end
  effect = fn a,b,n ->
    raised = fn kind -> {:raised,{:exception,kind,{:callback_failed,n,a,b,<<5::3>>}}} end
    case mode do
      {:fail,kind,^n} -> raised.(kind)
      {:return_then_fail,_,_,kind,^n} -> raised.(kind)
      {:return_then_fail,value,^n,_,_} -> {:returned,value}
      {:return,value,^n} -> {:returned,value}
      {:improper_return,^n} -> {:returned,[a|b]}
      :pair -> {:returned,{a,b}}
      _ -> {:returned,{:combined,n,a,b,captured}}
    end
  end
  run = fn run, l, r, n, output, trace ->
    case {l,r} do
      {[],[]} ->
        outcome = if invalid, do: error.(:function_clause), else: {:ok,Enum.reverse(output)}
        {outcome,trace}
      {[a|xs],[b|ys]} ->
        if invalid do
          reason = if mode === :non_function, do: {:badfun,:not_fun}, else: {:badarity,true,[a,b]}
          {error.(reason),trace}
        else
          trace = [{a,b,n,captured}|trace]
          case effect.(a,b,n) do
            {:raised,exception} -> {exception,trace}
            {:returned,value} -> run.(run,xs,ys,n+1,[value|output],trace)
          end
        end
      _ -> {error.(:function_clause),trace}
    end
  end
  {outcome,trace} = run.(run,left,right,1,[],[])
  {outcome,Enum.reverse(trace),left,right,captured}
end
cases = read.("cases.json")
expected = for c <- cases, do: {:ok,model.(decode.(decode,c["list_zipwith"]))}
oracle = for s <- read.("manifest.json")["expected"], do: from_etf.(s)
actual = for row <- read.("actuals.json") do
  "ok" = row["status"]
  from_etf.(row["value"])
end
Enum.each([{"OTP",oracle},{"AtomVM",actual}],fn {vm,rows} ->
  true = length(rows) === length(expected)
  failures = for {{a,e},n} <- Enum.with_index(Enum.zip(rows,expected)), a !== e, do: n
  IO.puts("#{vm}: #{length(rows)-length(failures)}/#{length(rows)} independent zipwith contracts")
  if failures !== [], do: raise("#{vm} contract failures at indices #{inspect(failures,limit: :infinity)}")
end)
"""


# Rank each tagged item by all strictly preceding keys plus earlier equal keys.
# This does not call sorting/merging APIs or reproduce either VM's algorithm.
LIST_ORDERING_MODEL = """
[path] = System.argv()
read = fn name -> JSON.decode!(File.read!(Path.join(path,name))) end
from_etf = fn s -> :erlang.binary_to_term(Base.decode64!(s)) end
decode = fn decode, value ->
  case value do
    %{"tuple" => xs} -> List.to_tuple(for x <- xs, do: decode.(decode,x))
    %{"map" => xs} -> Map.new(xs,fn [k,v] -> {decode.(decode,k),decode.(decode,v)} end)
    %{"bits" => [v,n]} -> <<v::size(n)>>
    xs when is_list(xs) -> for x <- xs, do: decode.(decode,x)
    s when is_binary(s) -> String.to_atom(s)
    n -> n
  end
end
model = fn [operation,direction,left,right,mode] ->
  captured = {:captured,left,right,<<5::3>>,Integer.pow(2,100)}
  items = if operation === :merge, do: left ++ right, else: left
  ordered = fn ->
    indexed = Enum.with_index(items)
    ranks = for {x,i} <- indexed do
      rank = Enum.count(indexed,fn {y,j} ->
        a = elem(x,0)
        b = elem(y,0)
        before = if direction === :ascending, do: b < a, else: b > a
        before or (a == b and j < i)
      end)
      {rank,x}
    end
    sorted = for rank <- Enum.take(Stream.iterate(0,&(&1+1)),length(items)),
                 {r,x} <- ranks, r === rank, do: x
    if operation === :usort do
      for {x,i} <- Enum.with_index(sorted),
          not Enum.any?(Enum.take(sorted,i),fn y -> elem(y,0) == elem(x,0) end), do: x
    else
      sorted
    end
  end
  invoked = if operation === :merge, do: left !== [] and right !== [], else: length(left) > 1
  outcome = cond do
    mode in [:non_function,:arity_one,:arity_three] ->
      reason = if operation === :sort and invoked do
        if mode === :non_function, do: {:badfun,:not_fun}, else: {:badarity,true}
      else
        :function_clause
      end
      {:exception,:error,reason}
    not invoked -> {:ok,items}
    match?({:fail,_},mode) -> {:exception,elem(mode,1),{:comparator_failed,<<5::3>>}}
    match?({:return,_},mode) -> {:exception,:error,{:case_clause,elem(mode,1)}}
    true -> {:ok,ordered.()}
  end
  {:ok,{outcome,true,left,right,captured}}
end
cases = read.("cases.json")
expected = for c <- cases, do: model.(decode.(decode,c["list_ordering"]))
oracle = for s <- read.("manifest.json")["expected"], do: from_etf.(s)
actual = for row <- read.("actuals.json") do
  "ok" = row["status"]
  from_etf.(row["value"])
end
Enum.each([{"OTP",oracle},{"AtomVM",actual}],fn {vm,rows} ->
  true = length(rows) === length(expected)
  failures = for {{a,e},n} <- Enum.with_index(Enum.zip(rows,expected)), a !== e, do: n
  IO.puts("#{vm}: #{length(rows)-length(failures)}/#{length(rows)} independent ordering contracts")
  if failures !== [], do: raise("#{vm} contract failures at indices #{inspect(failures,limit: :infinity)}")
end)
"""


LIST_KEYS_MODEL = """
[path] = System.argv()
read = fn name -> JSON.decode!(File.read!(Path.join(path,name))) end
from_etf = fn s -> :erlang.binary_to_term(Base.decode64!(s)) end
decode = fn decode, value ->
  case value do
    %{"tuple" => xs} -> List.to_tuple(for x <- xs, do: decode.(decode,x))
    %{"map" => xs} -> Map.new(xs,fn [k,v] -> {decode.(decode,k),decode.(decode,v)} end)
    %{"bits" => [v,n]} -> <<v::size(n)>>
    xs when is_list(xs) -> for x <- xs, do: decode.(decode,x)
    s when is_binary(s) -> String.to_atom(s)
    n -> n
  end
end
model = fn [operation,key,position,shape,items,replacement] ->
  tail = if shape === :improper, do: :tail, else: []
  input = if shape === :non_list, do: {:not_a_list,items}, else: items ++ tail
  valid_position = is_integer(position) and position > 0
  sorting = operation in [:keysort,:ukeysort]
  valid_element = fn x -> is_tuple(x) and tuple_size(x) >= position end
  exception = fn reason -> {:exception,:error,reason} end
  outcome = cond do
    not valid_position -> exception.(:function_clause)
    operation in [:keyreplace,:keystore] and not is_tuple(replacement) ->
      exception.(:function_clause)
    sorting ->
      cond do
        shape === :non_list or (shape === :improper and length(items) < 2) ->
          exception.({:case_clause,input})
        shape === :proper and length(items) < 2 -> {:ok,items}
        not Enum.all?(items,valid_element) -> exception.(:badarg)
        shape === :improper -> exception.(:function_clause)
        true ->
          indexed = Enum.with_index(items)
          ranks = for {x,i} <- indexed do
            rank = Enum.count(indexed,fn {y,j} ->
              a = elem(x,position-1)
              b = elem(y,position-1)
              b < a or (a == b and j < i)
            end)
            {rank,x}
          end
          sorted = for rank <- Enum.take(Stream.iterate(0,&(&1+1)),length(items)),
                       {r,x} <- ranks, r === rank, do: x
          unique = for {x,i} <- Enum.with_index(sorted),
                       not Enum.any?(Enum.take(sorted,i),fn y ->
                         elem(y,position-1) == elem(x,position-1)
                       end), do: x
          {:ok,if(operation === :ukeysort, do: unique, else: sorted)}
      end
    shape === :non_list -> exception.(:function_clause)
    true ->
      match = Enum.find_index(items,fn x -> valid_element.(x) and elem(x,position-1) == key end)
      cond do
        match === nil and shape === :improper -> exception.(:function_clause)
        match === nil and operation === :keystore -> {:ok,items ++ [replacement]}
        match === nil and operation === :keytake -> {:ok,false}
        match === nil -> {:ok,input}
        true ->
          remaining = for {x,i} <- Enum.with_index(items), i !== match, do: x
          replaced = for {x,i} <- Enum.with_index(items), do: if(i === match, do: replacement, else: x)
          result = case operation do
            :keydelete -> remaining ++ tail
            :keytake -> {:value,Enum.at(items,match),remaining ++ tail}
            _ -> replaced ++ tail
          end
          {:ok,result}
      end
  end
  {:ok,{outcome,input,{:captured,key,position,input,replacement,<<5::3>>,Integer.pow(2,100)}}}
end
cases = read.("cases.json")
expected = for c <- cases, do: model.(decode.(decode,c["list_keys"]))
oracle = for s <- read.("manifest.json")["expected"], do: from_etf.(s)
actual = for row <- read.("actuals.json") do
  "ok" = row["status"]
  from_etf.(row["value"])
end
Enum.each([{"OTP",oracle},{"AtomVM",actual}],fn {vm,rows} ->
  true = length(rows) === length(expected)
  failures = for {{a,e},n} <- Enum.with_index(Enum.zip(rows,expected)), a !== e, do: n
  IO.puts("#{vm}: #{length(rows)-length(failures)}/#{length(rows)} independent keyed-list contracts")
  if failures !== [], do: raise("#{vm} contract failures at indices #{inspect(failures,limit: :infinity)}")
end)
"""


# Structural oracle: no lists:flatten/append or ++ to calculate expectations.
DEEP_LIST_MODEL = """
[path] = System.argv()
read = fn name -> JSON.decode!(File.read!(Path.join(path,name))) end
literal = fn text ->
  {:ok,tokens,_} = :erl_scan.string(String.to_charlist(text <> "."))
  {:ok,value} = :erl_parse.parse_term(tokens)
  value
end
concat = fn
  _rec, [], tail -> tail
  rec, [head | rest], tail -> [head | rec.(rec,rest,tail)]
  _rec, _, _ -> throw({:model_error,:badarg})
end
flatten = fn
  _rec, [] -> []
  rec, [head | rest] ->
    left = case head do
      [] -> []
      [_ | _] -> rec.(rec,head)
      leaf -> [leaf]
    end
    concat.(concat,left,rec.(rec,rest))
  _rec, _ -> throw({:model_error,:function_clause})
end
append_list = fn
  _rec, [] -> []
  _rec, [last] -> last
  rec, [head | rest] -> concat.(concat,head,rec.(rec,rest))
  _rec, _ -> throw({:model_error,:function_clause})
end
cases = read.("cases.json")
expected = Enum.map(cases, fn c ->
  [operation,input_text,tail_text] = c["deep_lists"]
  input = literal.(input_text)
  tail = literal.(tail_text)
  result = try do
    value = case operation do
      "flatten" -> flatten.(flatten,input)
      "append_list" -> append_list.(append_list,input)
      operation when operation in ["append_pair","concat","concat_apply"] ->
        concat.(concat,input,tail)
    end
    {:ok,value}
  catch
    :throw, {:model_error,reason} -> {:exception,:error,reason}
  end
  {:ok,{result,input,tail,{:captured,input,tail,%{retained: <<5::3>>}}}}
end)
for {vm,rows} <- [otp: read.("manifest.json")["expected"],
                  atomvm: Enum.map(read.("actuals.json"), fn row ->
                    "ok" = row["status"]
                    row["value"]
                  end)] do
  true = length(rows) === length(expected)
  failures = Enum.zip([cases,expected,rows]) |> Enum.filter(fn {_c,want,row} ->
    :erlang.binary_to_term(Base.decode64!(row)) !== want
  end)
  if failures !== [], do: raise(inspect({vm,failures},limit: 10))
  IO.puts("#{vm}: #{length(rows)} independent deep-list contracts")
end
"""


ROOT = Path(__file__).resolve().parents[3]
SCRIPT = Path(__file__).with_name("validate.py")


@unittest.skipUnless((ROOT / "build/src/AtomVM").is_file(), "Build AtomVM first")
class ValidationTests(unittest.TestCase):
    def invoke(self, output, *args):
        result = subprocess.run(
            [sys.executable, str(SCRIPT), "--build", str(ROOT / "build"),
             "--output", str(output), *map(str, args)],
            capture_output=True, text=True, timeout=900,
        )
        self.assertTrue((output / "summary.json").is_file(), result.stdout + result.stderr)
        summary = json.loads((output / "summary.json").read_text())
        self.assertEqual(result.returncode,
                         int(summary["failed"] > 0 or summary["unverified"] > 0
                             or (summary["uncovered"] > 0 and "--replay" not in args)),
                         result.stdout + result.stderr)
        return summary

    def test_seed_and_replay_preserve_inputs_and_outcomes(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            first = work / "first"
            summary = self.invoke(first, "--seed", 42, "--generated", 32)
            again = work / "again"
            self.assertEqual(self.invoke(again, "--seed", 42, "--generated", 32), summary)
            self.assertEqual((first / "cases.json").read_text(), (again / "cases.json").read_text())
            replay = work / "replay"
            self.assertEqual(self.invoke(replay, "--replay", first / "cases.json"), summary)
            self.assertEqual((first / "cases.json").read_text(), (replay / "cases.json").read_text())
            # OTP hash-map iteration can change ETF byte order across VMs.
            # Check the decoded oracle terms, not incidental serialization order.
            check = subprocess.run([
                "elixir", "-e", """
                [a, b] = System.argv()
                read = fn p -> JSON.decode!(File.read!(p))["expected"] end
                decode = fn s -> :erlang.binary_to_term(Base.decode64!(s)) end
                true = Enum.all?(Enum.zip(read.(a), read.(b)), fn {x,y} -> decode.(x) === decode.(y) end)
                """, str(first / "manifest.json"), str(replay / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)
            other = work / "other"
            self.invoke(other, "--seed", 43, "--generated", 32)
            self.assertNotEqual((first / "cases.json").read_text(), (other / "cases.json").read_text())
            self.assertEqual(summary["passed"] + summary["failed"], 32)
            self.assertEqual(summary["laws"], 0)
            selected = json.loads((first / "cases.json").read_text())
            self.assertEqual({entry["operation"] for entry in selected},
                             {"compare", "maps", "bits", "arithmetic", "float_bits",
                              "map_sequence", "dictionary_sequence", "receive_sequence",
                              "binary_sequence", "lifecycle_sequence"})
            self.assertEqual(summary["passed"], 32)

    def test_timer_and_subtraction_observations_have_independent_oracles(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            source.write_text(json.dumps([{"expression": e} for e in [
                "differential:call(erlang,'--',[[1,2,1,3],[1,3]])",
                "differential:call(erlang,'--',[[1,1.0,1],[1.0,1]])",
                "differential:call(erlang,'--',[[{1},#{a=>1}],[{1.0},#{a=>1.0}]])",
                "differential:call(erlang,'--',[[a|tail],[]])",
                "differential:timer(deliver,0,{nested,7})",
                "differential:timer(deliver,1,<<5:3>>)",
                "differential:timer(cancel,60000,tick)",
                "differential:timer(cancel,0,tick)",
                "differential:timer(expired,0,tick)",
                "differential:timer(time_left,60000,tick)",
                "differential:timer(no_time_left,0,tick)",
                "differential:timer(false_before_delivery_waited,1,tick)",
                "differential:timer(false_before_delivery_target,1,tick)",
                "differential:timer_options(deliver,10,[{abs,true},{abs,false}])",
                "R=erlang:send_after(10,self(),tick,[abs]),{is_reference(R),erlang:cancel_timer(R)=/=false}",
            ]]))
            output = work / "results"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 15, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            # Expectations are handwritten, not derived from the helper. Both
            # cancel outcomes must preserve unrelated mailbox contents, while
            # a completed timer must be absent on repeated cancellation.
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                values = JSON.decode!(File.read!(path))["expected"]
                         |> Enum.map(&:erlang.binary_to_term(Base.decode64!(&1)))
                [
                  {:ok, [2, 1]},
                  {:ok, [1]},
                  {:ok, [{1}, %{a: 1}]},
                  {:exception, :error, :badarg},
                  {:ok, {true, {:nested, 7}, [:retained]}},
                  {:ok, {true, <<5::3>>, [:retained]}},
                  {:ok, {true, true, false, [:retained]}},
                  {:ok, {true, true, false, [:retained]}},
                  {:ok, {:tick, false, false}},
                  {:ok, {true, :remaining_timeout, false, [:retained]}},
                  {:ok, {true, :ok, false, [:retained]}},
                  {:ok, {true, :settled, false, [:retained]}},
                  {:ok, {true, :settled, :normal, false, [:retained]}},
                  {:ok, {true, {:nested, %{1 => :one, 1.0 => :float}, <<5::3>>}, true, false, false, [:retained]}},
                  {:exception, :error, :badarg}
                ] = values
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_send_after_options_have_independent_oracles(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            selected = [entry for entry in lean_runtime.cases()
                        if entry.get("otp_extension") == "send_after/4"]
            self.assertTrue(all("lean_origins" not in entry for entry in selected))
            source = work / "input.json"
            source.write_text(json.dumps(selected))
            output = work / "results"
            summary = self.invoke(output, "--replay", source)
            # Missing /4 support stays a visible failure, not an allowlisted pass.
            # This check remains valid once AtomVM implements the API.
            self.assertEqual(summary["passed"] + summary["failed"], 52)
            self.assertEqual(summary["unverified"], 0)
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                values = JSON.decode!(File.read!(path))["expected"]
                         |> Enum.map(&:erlang.binary_to_term(Base.decode64!(&1)))
                delivered = {:ok, {true, {:nested, %{1 => :one, 1.0 => :float}, <<5::3>>},
                                   true, false, false, [:retained]}}
                canceled = {:ok, {true, true, false, [:retained]}}
                # Four relative option forms; three absolute option forms.
                # Past/current deadlines deliver; cancel races have one normalized
                # valid result. Invalid entries must raise badarg, not undef.
                expected = List.duplicate([delivered, delivered, canceled], 4)
                           |> List.flatten()
                expected = expected ++ List.flatten(List.duplicate(
                  [delivered, delivered, delivered, delivered, canceled, canceled, canceled], 3))
                expected = expected ++ List.duplicate({:exception, :error, :badarg}, 19)
                true = values === expected
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_map_and_list_relation_observations_have_independent_oracles(self):
        all_cases = lean_runtime.cases()
        upstream_sort = [c for c in all_cases
                         if c.get("upstream_example") == "examples/otp/lists.exs#sort_contract_holds"]
        upstream_seq = [c for c in all_cases
                        if c.get("upstream_example") == "examples/otp/lists.exs#sequence_formula"]
        self.assertGreater(len(upstream_sort), 0)
        self.assertGreater(len(upstream_seq), 0)
        self.assertTrue(all("lean_origins" not in c and not c.get("assert_true")
                            for c in upstream_sort + upstream_seq))
        model_relations = [c for c in all_cases
                           if any(c["expression"].startswith(prefix) for prefix in (
                               "differential:map_relations(",
                               "differential:map_singleton_iteration(",
                               "differential:map_guards(",
                               "differential:list_relations(",
                           )) and "upstream_example" not in c]
        self.assertGreater(len(model_relations), 0)
        self.assertTrue(all(c.get("lean_origins") and not c.get("assert_true")
                            for c in model_relations))
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            expressions = [
                "differential:map_relations([{a,1},{a,2},{1,10}],[{a,2},{1,10.0}],[a,1,1.0,missing])",
                "differential:map_singleton_iteration(k,42)",
                "differential:map_guards(a,#{a=>wanted,b=>2})",
                "differential:map_guards(a,[])",
                "differential:list_relations([3,1,2,1],[1,4],tail,[1,2,3,4,9])",
                "differential:seq_contract(1,7,3,[0,1,2,3])",
                "differential:seq_contract(5,4,1,[0])",
                "differential:call(lists,seq,[5,2,2])",
                "differential:call(lists,member,[a,[a|improper]])",
            ]
            source.write_text(json.dumps([{"expression": e} for e in expressions]))
            output = work / "relations"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 9, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                values = JSON.decode!(File.read!(path))["expected"]
                         |> Enum.map(&:erlang.binary_to_term(Base.decode64!(&1)))
                hit = fn k, v ->
                  {k, {:lookup, {:ok, v}, {:ok, v}, {:ok, v}, v, true, true},
                   {:put_rel, true, true}, {:update_rel, {:ok, true, true}},
                   {:remove_take_rel, true, {:ok, true, true}},
                   {:merge_rel, true, true, true, true, true}}
                end
                miss = fn k ->
                  {k, {:lookup, :error, {:exception, :error, {:badkey, k}},
                       {:exception, :error, {:badkey, k}}, :"$default", false, false},
                   {:put_rel, true, true},
                   {:update_rel, {:exception, :error, {:badkey, k}}},
                   {:remove_take_rel, true, {:error, true}},
                   {:merge_rel, true, true, true, true, true}}
                end
                expected_map = {:ok, {
                  {:from_list_eq_fold_put, true, true},
                  {:sizes, 2, 2, 2, 2, 1},
                  {:equality_vs_lookup, {false, false, true, true, true, true}},
                  {:compare, {true, false, false, true, false, true, false, true}},
                  {:entries, {2, %{:a => 2, 1 => 10}}, {2, %{:a => 2, 1 => 10.0}}},
                  {:per_key, [hit.(:a, 2), hit.(1, 10), miss.(1.0), miss.(:missing)]}
                }}
                expected_list = {:ok, {
                  {:lengths, 4, 2, 6, 4},
                  {:reverse_laws, true, true, true},
                  {:subtract_laws, true, true, true, true, [4, 1]},
                  {:sort_laws, [1, 1, 2, 3], true, true, true, true},
                  {:probes, [
                    {1, true, true, true, 2, true, true, true, true, 2, true},
                    {2, true, true, true, 1, true, true, true, true, 1, true},
                    {3, true, true, true, 1, true, true, true, true, 1, true},
                    {4, false, false, true, 0, true, true, true, true, 0, true},
                    {9, false, false, true, 0, true, true, true, true, 0, true}
                  ]}
                }}
                expected = [
                  expected_map,
                  {:ok, {true, true, true, true, :none, {0, %{}}, [k: 42]}},
                  {:ok, {{:map, 2}, :has_key, :nonempty, :matched_wanted,
                         {:ok, true}, {:ok, 2}, {:ok, true}, {:ok, :wanted}}},
                  {:ok, {:not_map, :no_key, :empty_or_not_map, :rejected,
                         {:ok, false}, {:exception, :error, {:badmap, []}},
                         {:exception, :error, {:badmap, []}},
                         {:exception, :error, {:badmap, []}}}},
                  expected_list,
                  {:ok, {[1, 4, 7], 3, 3, true, :not_applicable,
                         [{0, 1, true}, {1, 4, true}, {2, 7, true},
                          {3, :out_of_range, {:exception, :error, :function_clause}}]}},
                  {:ok, {[], 0, 0, true, true,
                         [{0, :out_of_range, {:exception, :error, :function_clause}}]}},
                  {:exception, :error, :badarg},
                  {:ok, true}
                ]
                true = values === expected
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_queue_callbacks_have_independent_contracts_and_replay(self):
        selected = [case for case in lean_runtime.cases() if "queue_callback" in case]
        self.assertEqual({case["queue_callback"][0] for case in selected},
                         {"filter", "filtermap", "fold", "any", "all", "delete_with", "delete_with_r"})
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            for name, cases in [("all", selected), ("replay", selected[::-11])]:
                source.write_text(json.dumps(cases))
                output = work / name
                self.assertEqual(self.invoke(output, "--replay", source),
                                 {"laws": 0, "passed": len(cases), "failed": 0,
                                  "unverified": 0, "uncovered": 0})
                check = subprocess.run(["elixir", "-e", QUEUE_CALLBACK_MODEL, str(output)],
                                       capture_output=True, text=True, timeout=30)
                self.assertEqual(check.returncode, 0, check.stdout + check.stderr)
                saved = json.loads((output / "cases.json").read_text())
                self.assertEqual([c["queue_callback"] for c in saved],
                                 [c["queue_callback"] for c in cases])

    def test_set_callbacks_have_independent_contracts_and_replay(self):
        selected = [case for case in lean_runtime.cases() if "set_callback" in case]
        self.assertEqual({case["set_callback"][0] for case in selected},
                         {"fold", "filter", "map", "filtermap"})
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            for name, cases in [("all", selected), ("replay", selected[::-9])]:
                with self.subTest(name=name):
                    source.write_text(json.dumps(cases))
                    output = work / name
                    summary = self.invoke(output, "--replay", source)
                    saved = json.loads((output / "cases.json").read_text())
                    self.assertEqual([c["set_callback"] for c in saved],
                                     [c["set_callback"] for c in cases])
                    check = subprocess.run(["elixir", "-e", SET_CALLBACK_MODEL, str(output)],
                                           capture_output=True, text=True, timeout=30)
                    self.assertEqual(check.returncode, 0, check.stdout + check.stderr)
                    self.assertEqual(summary,
                                     {"laws": 0, "passed": len(cases), "failed": 0,
                                      "unverified": 0, "uncovered": 0})

    def test_ordered_list_folds_have_independent_contracts_and_replay(self):
        selected = [case for case in lean_runtime.cases() if "list_fold" in case]
        self.assertEqual({case["list_fold"][0] for case in selected},
                         {"foldl", "foldr", "mapfoldl", "mapfoldr"})
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            for name, cases in [("all", selected), ("replay", selected[::-9])]:
                with self.subTest(name=name):
                    source.write_text(json.dumps(cases))
                    output = work / name
                    summary = self.invoke(output, "--replay", source)
                    saved = json.loads((output / "cases.json").read_text())
                    self.assertEqual([c["list_fold"] for c in saved],
                                     [c["list_fold"] for c in cases])
                    check = subprocess.run(["elixir", "-e", LIST_FOLD_MODEL, str(output)],
                                           capture_output=True, text=True, timeout=30)
                    self.assertEqual(check.returncode, 0, check.stdout + check.stderr)
                    self.assertEqual(summary,
                                     {"laws": 0, "passed": len(cases), "failed": 0,
                                      "unverified": 0, "uncovered": 0})

    def test_unary_list_callbacks_have_independent_contracts_and_replay(self):
        selected = [case for case in lean_runtime.cases() if "list_callback" in case]
        self.assertEqual({case["list_callback"][0] for case in selected},
                         {"map", "foreach", "flatmap", "filter", "filtermap", "search", "dropwhile"})
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            for name, cases in [("all", selected), ("replay", selected[::-13])]:
                with self.subTest(name=name):
                    source.write_text(json.dumps(cases))
                    output = work / name
                    summary = self.invoke(output, "--replay", source)
                    saved = json.loads((output / "cases.json").read_text())
                    self.assertEqual([c["list_callback"] for c in saved],
                                     [c["list_callback"] for c in cases])
                    check = subprocess.run(["elixir", "-e", LIST_CALLBACK_MODEL, str(output)],
                                           capture_output=True, text=True, timeout=30)
                    self.assertEqual(check.returncode, 0, check.stdout + check.stderr)
                    self.assertEqual(summary,
                                     {"laws": 0, "passed": len(cases), "failed": 0,
                                      "unverified": 0, "uncovered": 0})

    def test_paired_list_zipwith_has_independent_contracts_and_replay(self):
        selected = [case for case in lean_runtime.cases() if "list_zipwith" in case]
        self.assertEqual(len(selected), 95)
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            for name, cases in [("all", selected), ("replay", selected[::-7])]:
                with self.subTest(name=name):
                    source.write_text(json.dumps(cases))
                    output = work / name
                    summary = self.invoke(output, "--replay", source)
                    saved = json.loads((output / "cases.json").read_text())
                    self.assertEqual([c["list_zipwith"] for c in saved],
                                     [c["list_zipwith"] for c in cases])
                    check = subprocess.run(["elixir", "-e", LIST_ZIPWITH_MODEL, str(output)],
                                           capture_output=True, text=True, timeout=30)
                    self.assertEqual(check.returncode, 0, check.stdout + check.stderr)
                    self.assertEqual(summary,
                                     {"laws": 0, "passed": len(cases), "failed": 0,
                                      "unverified": 0, "uncovered": 0})

    def test_comparator_ordering_has_independent_contracts_and_replay(self):
        selected = [case for case in lean_runtime.cases() if "list_ordering" in case]
        self.assertEqual(len(selected), 148)
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            for name, cases in [("all", selected), ("replay", selected[::-5])]:
                with self.subTest(name=name):
                    source.write_text(json.dumps(cases))
                    output = work / name
                    summary = self.invoke(output, "--replay", source)
                    saved = json.loads((output / "cases.json").read_text())
                    self.assertEqual([c["list_ordering"] for c in saved],
                                     [c["list_ordering"] for c in cases])
                    check = subprocess.run(["elixir", "-e", LIST_ORDERING_MODEL, str(output)],
                                           capture_output=True, text=True, timeout=30)
                    self.assertEqual(check.returncode, 0, check.stdout + check.stderr)
                    self.assertEqual(summary,
                                     {"laws": 0, "passed": len(cases), "failed": 0,
                                      "unverified": 0, "uncovered": 0})

    def test_keyed_lists_have_independent_contracts_and_replay(self):
        selected = [case for case in lean_runtime.cases() if "list_keys" in case]
        self.assertEqual(len(selected), 344)
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            for name, cases in [("all", selected), ("replay", selected[::-7])]:
                with self.subTest(name=name):
                    source.write_text(json.dumps(cases))
                    output = work / name
                    summary = self.invoke(output, "--replay", source)
                    saved = json.loads((output / "cases.json").read_text())
                    self.assertEqual([c["list_keys"] for c in saved],
                                     [c["list_keys"] for c in cases])
                    check = subprocess.run(["elixir", "-e", LIST_KEYS_MODEL, str(output)],
                                           capture_output=True, text=True, timeout=30)
                    self.assertEqual(check.returncode, 0, check.stdout + check.stderr)
                    self.assertEqual(summary,
                                     {"laws": 0, "passed": len(cases), "failed": 0,
                                      "unverified": 0, "uncovered": 0})

    def test_positional_lists_preserve_boundaries_and_replay(self):
        selected = [case for case in lean_runtime.cases() if "list_positions" in case]
        self.assertEqual(len(selected), 284)
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            for name, cases in [("all", selected), ("replay", selected[::-13])]:
                source.write_text(json.dumps(cases))
                output = work / name
                self.assertEqual(self.invoke(output, "--replay", source),
                                 {"laws": 0, "passed": len(cases), "failed": 0,
                                  "unverified": 0, "uncovered": 0})
                saved = json.loads((output / "cases.json").read_text())
                self.assertEqual([c["list_positions"] for c in saved],
                                 [c["list_positions"] for c in cases])
            # Handwritten boundaries distinguish validation from traversal:
            # nthtail may return an improper suffix, split may preserve one,
            # and sublist truncates but must validate its initial list shape.
            check = subprocess.run(["elixir", "-e", """
            [path] = System.argv()
            read = fn name ->
              dir = Path.join(path,name)
              cases = JSON.decode!(File.read!(Path.join(dir,"cases.json")))
              values = JSON.decode!(File.read!(Path.join(dir,"manifest.json")))["expected"]
                       |> Enum.map(&:erlang.binary_to_term(Base.decode64!(&1)))
              Map.new(Enum.zip(cases,values), fn {c,v} -> {c["list_positions"],v} end)
            end
            all = read.("all")
            fc = {:exception,:error,:function_clause}
            controls = [
              {["nth",["1","[a,b|tail]"]], {:ok,:a}},
              {["nth",["2","[a,b|tail]"]], {:ok,:b}},
              {["nth",["3","[a,b|tail]"]], fc},
              {["nth",["bad","[a,b,c]"]], fc},
              {["nthtail",["2","[a,b|tail]"]], {:ok,:tail}},
              {["nthtail",["3","[a,b|tail]"]], fc},
              {["nthtail",["0","tail"]], fc},
              {["sublist",["tail","0"]], fc},
              {["sublist",["[a,b|tail]","2"]], {:ok,[:a,:b]}},
              {["sublist",["[a,b|tail]","3"]], fc},
              {["sublist",["[a,b,c]","4"]], {:ok,[:a,:b,:c]}},
              {["sublist",["[]","4","-1"]], {:ok,[]}},
              {["split",["2","[a,b|tail]"]], {:ok,{[:a,:b],:tail}}},
              {["split",["4","[a,b,c]"]], {:exception,:error,:badarg}},
              {["split",["1.0","[a,b,c]"]], {:exception,:error,:badarg}}
            ]
            true = Enum.all?(controls, fn {key,value} -> Map.fetch!(all,key) === value end)
            true = Enum.all?(read.("replay"), fn {key,value} -> Map.fetch!(all,key) === value end)
            """, str(work)], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_deep_lists_have_structural_contracts_and_replay(self):
        selected = [case for case in lean_runtime.cases() if "deep_lists" in case]
        self.assertEqual(len(selected), 185)
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            for name, cases in [("all", selected), ("replay", selected[::-7])]:
                with self.subTest(name=name):
                    source.write_text(json.dumps(cases))
                    output = work / name
                    self.assertEqual(self.invoke(output, "--replay", source),
                                     {"laws": 0, "passed": len(cases), "failed": 0,
                                      "unverified": 0, "uncovered": 0})
                    saved = json.loads((output / "cases.json").read_text())
                    self.assertEqual([c["deep_lists"] for c in saved],
                                     [c["deep_lists"] for c in cases])
                    check = subprocess.run(["elixir", "-e", DEEP_LIST_MODEL, str(output)],
                                           capture_output=True, text=True, timeout=30)
                    self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_queue_sequences_match_list_model_and_reordered_replay(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            selected = [case for case in generate(20261126, 240)
                        if case["operation"] == "queue_sequence"]
            self.assertEqual(len(selected), 14)
            self.assertEqual(selected, [case for case in generate(20261126, 240)
                                       if case["operation"] == "queue_sequence"])
            self.assertNotEqual(selected[4:],
                                [case for case in generate(20261127, 240)
                                 if case["operation"] == "queue_sequence"][4:])
            source = work / "input.json"
            for name, cases in [("first", selected), ("replay", selected[::-3])]:
                source.write_text(json.dumps(cases))
                output = work / name
                self.assertEqual(self.invoke(output, "--replay", source),
                                 {"laws": 0, "passed": len(cases), "failed": 0,
                                  "unverified": 0, "uncovered": 0})
                check = subprocess.run(["elixir", "-e", QUEUE_MODEL, str(output)],
                                       capture_output=True, text=True, timeout=30)
                self.assertEqual(check.returncode, 0, check.stdout + check.stderr)
                selected = json.loads((output / "cases.json").read_text())
                self.assertEqual([(c["seed"], c["index"], c["args"]) for c in selected],
                                 [(c["seed"], c["index"], c["args"]) for c in cases])

    def test_binary_sequences_rebase_scopes_and_preserve_split_edges(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            short = {"bits": [0x6162, 16]}
            long = {"bits": [0x616263, 24]}
            patterns = [short, long]
            scope = {"tuple": ["scope", {"tuple": [3, 3]}]}
            negative = {"tuple": ["scope", {"tuple": [6, -3]}]}
            zero = {"tuple": ["scope", {"tuple": [0, 0]}]}
            steps = [
                ["match", patterns, []], ["match", patterns[::-1], [negative]],
                ["split", patterns, ["global", scope]],
                ["split", patterns, [negative, "global"]],
                ["match", patterns, [zero, scope]],
                ["match", patterns, [scope, zero]],
                ["match", patterns, [{"tuple": ["scope", {"tuple": [3, 2]}]}]],
                ["part", 1, 5], ["match", long, []],
                ["part", 5, -3], ["match", patterns, []],
                ["append", [97, 98]], ["split", patterns, ["global"]],
                ["split", patterns, ["global", "trim"]],
                ["part", -1, 1],
                ["match", patterns, [{"tuple": ["scope", {"tuple": [0, 6]}]}]],
                ["part", 5, 0], ["match", short, [zero]],
                ["split", short, ["global"]],
                ["split", short, ["global", "trim_all"]],
            ]
            trims = [
                ["split", {"bits": [124, 8]}, ["global", "trim"]],
                ["split", {"bits": [124, 8]}, ["global", "trim_all"]],
                ["split", {"bits": [124, 8]},
                 ["global", {"tuple": ["scope", {"tuple": [1, 4]}]}]],
            ]
            source.write_text(json.dumps([
                {"operation": "binary_sequence", "args": [list(subject),
                    [{"tuple": step} for step in operations]]}
                for subject, operations in [(b"zababcab", steps), (b"|a||b|", trims)]
            ]))
            output = work / "binary"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 2, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                [first, second] = JSON.decode!(File.read!(path))["expected"]
                  |> Enum.map(fn s -> :erlang.binary_to_term(Base.decode64!(s)) end)
                state = fn b -> {byte_size(b), b} end
                row = fn value, b -> {{:ok, value}, state.(b)} end
                original = "zababcab"
                expected = [row.({1,2}, original), row.({3,3}, original),
                  row.(["zab", "ab"], original), row.(["zab", "ab"], original),
                  row.({3,3}, original), row.(:nomatch, original), row.({3,2}, original),
                  row.("ababc", "ababc"), row.({2,3}, "ababc"),
                  row.("abc", "abc"), row.({0,3}, "abc"),
                  row.("abcab", "abcab"), row.(["", "", ""], "abcab"),
                  row.([], "abcab"),
                  {{:exception, :error, :badarg}, state.("abcab")},
                  {{:exception, :error, :badarg}, state.("abcab")},
                  row.("", ""), row.(:nomatch, ""), row.([""], ""), row.([], "")]
                true = first === {:ok, {state.(original), expected}}
                expected_trims = [row.(["", "a", "", "b"], "|a||b|"),
                  row.(["a", "b"], "|a||b|"), row.(["|a", "", "b|"], "|a||b|")]
                true = second === {:ok, {state.("|a||b|"), expected_trims}}
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_receive_sequences_preserve_order_and_resume_after_misses(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            tag_float = {"tuple": ["tag", 1.0]}
            tag_int = {"tuple": ["tag", 1]}
            tag_wrong = {"tuple": ["tag", "wrong"]}
            tag_wanted = {"tuple": ["tag", "wanted"]}
            empty_tuple, empty_map = {"tuple": []}, {"map": []}
            bits = {"bits": [5, 3]}
            inputs = [
                (["sentinel", 1.0, 1, tag_float, tag_int], [
                    ["exact", 1], ["exact", 1], ["send", 1], ["tag", "tag"],
                    ["exact", 1.0], ["exact", 1], ["any"],
                ]),
                ([0, "bad", empty_tuple, empty_map, tag_wrong, tag_wanted,
                  5, 4, "fallback", "wanted"], [
                    ["guard", "tuple"], ["guard", "division"],
                    ["clauses", "wanted"], ["clauses", "wanted"],
                    ["guard", "map"], ["any"],
                ]),
                ([], [["any"], ["exact", "missing"], ["send", bits],
                      ["exact", bits], ["any"]]),
            ]
            source.write_text(json.dumps([
                {"operation": "receive_sequence",
                 "args": [setup, [{"tuple": step} for step in steps]]}
                for setup, steps in inputs
            ]))
            output = work / "receives"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 3, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                values = JSON.decode!(File.read!(path))["expected"]
                  |> Enum.map(fn s -> :erlang.binary_to_term(Base.decode64!(s)) end)
                wrap = fn rows -> {:ok, Enum.map(rows, &{:ok, &1})} end
                a = [:sentinel, 1.0, {:tag, 1.0}, {:tag, 1}]
                b = a ++ [1]
                c = [:sentinel, 1.0, {:tag, 1}, 1]
                d = [:sentinel, {:tag, 1}, 1]
                e = [:sentinel, {:tag, 1}]
                first = wrap.([
                  {:none, [:sentinel, 1.0, 1, {:tag, 1.0}, {:tag, 1}]},
                  {{:matched, 1}, a}, {:timeout, a}, {{:sent, 1}, b},
                  {{:matched, {:tag, 1.0}}, c}, {{:matched, 1.0}, d},
                  {{:matched, 1}, e}, {{:matched, :sentinel}, [{:tag, 1}]}])
                f = [0, :bad, {}, %{}, {:tag, :wrong}, 5, 4, :fallback, :wanted]
                g = [0, :bad, {}, %{}, {:tag, :wrong}, 5, :fallback, :wanted]
                h = [0, :bad, {}, %{}, {:tag, :wrong}, 5, :wanted]
                i = [0, :bad, {}, %{}, {:tag, :wrong}, 5]
                second = wrap.([
                  {:none, [0, :bad, {}, %{}, {:tag, :wrong}, {:tag, :wanted},
                           5, 4, :fallback, :wanted]},
                  {{:matched, {:tag, :wanted}}, f}, {{:matched, 4}, g},
                  {{:matched, :fallback}, h}, {{:matched, :wanted}, i},
                  {:timeout, i}, {{:matched, 0}, [:bad, {}, %{}, {:tag, :wrong}, 5]}])
                bits = <<5::3>>
                third = wrap.([{:none, []}, {:timeout, []}, {:timeout, []},
                               {{:sent, bits}, [bits]}, {{:matched, bits}, []}, {:timeout, []}])
                true = values === [first, second, third]
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_sequences_return_intermediate_results_and_exact_state(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            setup = [{"tuple": ["sentinel", "safe"]}]
            steps = [{"tuple": s} for s in [
                ["put", 1, "first"], ["put", 1.0, "second"],
                ["put", 1, "replaced"], ["erase", 1], ["get", 1.0],
                ["put", 1, "undefined"], ["selected", "undefined"],
                ["clear"], ["get", 1],
            ]]
            map_steps = steps[:7] + [{"tuple": s} for s in [
                ["update", "missing", "bad"], ["take", 1.0],
                ["take", 1.0], ["update", 1, "updated"],
            ]] + steps[7:]
            source.write_text(json.dumps([
                {"operation": "dictionary_sequence", "args": [setup, steps]},
                {"operation": "map_sequence", "args": [setup, map_steps]},
            ]))
            output = work / "sequences"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 2, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            # Hand-written expectations catch tracing mistakes even if both
            # runtimes agree on an incorrectly implemented probe.
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                [dictionary, map] = JSON.decode!(File.read!(path))["expected"]
                  |> Enum.map(fn s -> :erlang.binary_to_term(Base.decode64!(s)) end)
                s0 = %{sentinel: :safe}
                s1 = %{1 => :first, :sentinel => :safe}
                s2 = Map.put(s1, 1.0, :second)
                s3 = %{s2 | 1 => :replaced}
                s4 = %{1.0 => :second, :sentinel => :safe}
                s5 = Map.put(s4, 1, :undefined)
                ds = fn m -> {map_size(m), m, map_size(m),
                              Map.new(Map.keys(m), &{&1, :present})} end
                ms = fn m -> {map_size(m), m} end
                expected_d = {{:ok, :undefined}, ds.(s1)}
                {:ok, {initial_d, trace_d}} = dictionary
                true = initial_d === ds.(s0)
                true = trace_d === [expected_d,
                  {{:ok, :undefined}, ds.(s2)}, {{:ok, :first}, ds.(s3)},
                  {{:ok, :replaced}, ds.(s4)}, {{:ok, :second}, ds.(s4)},
                  {{:ok, :undefined}, ds.(s5)},
                  {{:ok, {1, %{1 => :present}}}, ds.(s5)},
                  {{:ok, {3, s5}}, ds.(%{})}, {{:ok, :undefined}, ds.(%{})}]
                s6 = %{1 => :undefined, :sentinel => :safe}
                s7 = %{s6 | 1 => :updated}
                {:ok, {initial_m, trace_m}} = map
                true = initial_m === ms.(s0)
                true = trace_m === [{{:ok, s1}, ms.(s1)}, {{:ok, s2}, ms.(s2)},
                  {{:ok, s3}, ms.(s3)}, {{:ok, s4}, ms.(s4)},
                  {{:ok, {:ok, :second}}, ms.(s4)}, {{:ok, s5}, ms.(s5)},
                  {{:ok, {1, %{1 => :present}}}, ms.(s5)},
                  {{:exception, :error, {:badkey, :missing}}, ms.(s5)},
                  {{:ok, {:second, s6}}, ms.(s6)}, {{:ok, :error}, ms.(s6)},
                  {{:ok, s7}, ms.(s7)}, {{:ok, s7}, ms.(%{})},
                  {{:ok, :error}, ms.(%{})}]
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_float_encoding_smoke_cases_have_known_byte_oracles(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / "floats"
            summary = self.invoke(output, "--float-encoding", "--generated", 6)
            self.assertEqual(summary["passed"] + summary["failed"], 6)
            selected = json.loads((output / "cases.json").read_text())
            self.assertEqual({entry["operation"] for entry in selected}, {"float_bits"})
            # Independent IEEE encodings: even tie, subnormal, near tie,
            # float32 even tie, underflow tie, and odd tie. Compare bytes,
            # not round-tripped floats (which cannot represent infinity in OTP).
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                values = JSON.decode!(File.read!(path))["expected"]
                encodings = Enum.map(values, fn value ->
                  {:ok, {_bits, encoded}} = :erlang.binary_to_term(Base.decode64!(value))
                  Base.encode16(encoded)
                end)
                ["6E64", "1100", "656E", "3F800000", "00000000", "3F800002"] = encodings
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_execution_paths_preserve_results_errors_and_guard_rejection(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            expressions = [
                "differential:execution_paths('+',-7,3)",
                "differential:execution_paths('+',-7,7)",
                "differential:execution_paths('div',-7,3)",
                "differential:execution_paths('rem',-7,3)",
                "differential:execution_paths('/',0,0)",
                "differential:execution_paths('+',bad,1)",
                "differential:execution_paths('<',9007199254740993,9007199254740992.0)",
                "differential:execution_paths('<',9007199254740992.0,9007199254740993)",
                "differential:execution_paths('=:=',1,1.0)",
                "differential:execution_paths('+',-0.0,-0.0)",
                "differential:execution_paths(map_get,1,#{1=>wanted,1.0=>other})",
                "differential:execution_paths(map_get,1.0,#{1=>wanted,1.0=>other})",
                "differential:execution_paths(map_get,1.0,#{1=>wanted})",
                "differential:execution_paths(map_get,key,bad)",
            ]
            structural = [entry for entry in lean_runtime.cases() if "structural_bif" in entry]
            self.assertEqual(len(structural), 70)
            self.assertEqual({entry["structural_bif"] for entry in structural},
                             {"length", "hd", "tl", "element", "tuple_size", "byte_size", "bit_size"})
            source.write_text(json.dumps([{"expression": e} for e in expressions] + structural))
            output = work / "paths"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 84, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            saved = json.loads((output / "cases.json").read_text())
            source.write_text(json.dumps([saved[i] for i in [83, 14, 24, 30, 66, 81, 4, 1]]))
            replay = work / "replay"
            self.assertEqual(self.invoke(replay, "--replay", source),
                             {"laws": 0, "passed": 8, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                [original, replay] = System.argv()
                original_cases = JSON.decode!(File.read!(Path.join(original, "cases.json")))
                paths = fn value, guard -> {:ok,
                  [body: value, dynamic: value, guard: guard]} end
                map_paths = fn value, guard, pattern -> {:ok,
                  [body: value, dynamic: value, guard: guard, pattern: pattern]} end
                negative_zero = {:ok, -0.0, Base.decode16!("8000000000000000")}
                expected = [paths.({:ok, -4}, :rejected), paths.({:ok, 0}, :matched),
                  paths.({:ok, -2}, :rejected), paths.({:ok, -1}, :rejected),
                  paths.({:exception, :error, :badarith}, :rejected),
                  paths.({:exception, :error, :badarith}, :rejected),
                  paths.({:ok, false}, :rejected), paths.({:ok, true}, :matched),
                  paths.({:ok, false}, :rejected), paths.(negative_zero, :matched),
                  map_paths.({:ok, :wanted}, :matched, {:matched, :wanted}),
                  map_paths.({:ok, :other}, :rejected, {:matched, :other}),
                  map_paths.({:exception, :error, {:badkey, 1.0}}, :rejected, :no_match),
                  map_paths.({:exception, :error, {:badmap, :bad}}, :rejected, :no_match)]
                manual = Map.new(Enum.zip(Enum.map(Enum.take(original_cases, 14),
                                                  & &1["expression"]), expected))
                tables = %{
                  "length" => %{"empty" => 0, "three" => 3, "two" => 2, "live" => 3,
                    "runtime_list" => 257, "improper" => :badarg,
                    "nested_improper" => :badarg, "non_list" => :badarg},
                  "hd" => %{"empty" => :badarg, "proper" => :wanted, "improper" => :wanted,
                    "other" => :other, "non_list" => :badarg, "heap_head" => {:nested, <<5::3>>}},
                  "tl" => %{"empty" => :badarg, "proper" => [:wanted], "improper" => :wanted,
                    "singleton" => [], "non_list" => :badarg,
                    "heap_tail" => [{:nested, <<5::3>>} | :tail]},
                  "tuple_size" => %{"empty" => 0, "three" => 3, "two" => 2,
                    "non_tuple" => :badarg, "live" => 3},
                  "element" => %{"index_-1" => :badarg, "index_0" => :badarg,
                    "index_1" => :wanted, "index_2" => :other, "index_3" => {:nested, <<5::3>>},
                    "index_4" => :badarg, "index_1.0" => :badarg, "index_bad" => :badarg,
                    "index_1 bsl 100" => :badarg, "empty" => :badarg, "non_tuple" => :badarg,
                    "invalid_both" => :badarg, "runtime_32" => :other,
                    "runtime_33" => :wanted, "runtime_34" => :badarg}
                }
                sizes = %{"heap_binary" => 248, "refc_binary" => 2056,
                  "aligned_slice" => 520, "unaligned_slice" => 520, "partial_slice" => 515,
                  "non_bitstring_atom" => :badarg, "non_bitstring_list" => :badarg}
                targets = %{"length" => 3, "hd" => :wanted, "tl" => [:wanted],
                  "tuple_size" => 3, "element" => :wanted, "byte_size" => 3, "bit_size" => 24}
                for path <- [original, replay] do
                  cases = JSON.decode!(File.read!(Path.join(path, "cases.json")))
                  values = JSON.decode!(File.read!(Path.join(path, "manifest.json")))["expected"]
                  true = length(cases) === length(values)
                  for {entry, encoded} <- Enum.zip(cases, values) do
                    want = case entry["structural_bif"] do
                      nil -> Map.fetch!(manual, entry["expression"])
                      op ->
                        label = entry["structural_case"]
                        result = if op in ["byte_size", "bit_size"] do
                          bits = case label do
                            "bits_" <> n -> String.to_integer(n)
                            _ -> Map.fetch!(sizes, label)
                          end
                          if op == "byte_size" and bits != :badarg, do: div(bits+7, 8), else: bits
                        else
                          Map.fetch!(Map.fetch!(tables, op), label)
                        end
                        observation = if result == :badarg, do: {:exception, :error, :badarg},
                                                              else: {:ok, result}
                        guard = if result != :badarg and result === Map.fetch!(targets, op),
                          do: :matched, else: :rejected
                        {:ok, observations} = paths.(observation, guard)
                        {:ok, {observations, observations}}
                    end
                    got = :erlang.binary_to_term(Base.decode64!(encoded))
                    if got !== want, do: raise(inspect({entry, want, got}))
                  end
                end
                """, str(output), str(replay)
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_reference_failures_are_reported_without_stopping_other_cases(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            source.write_text(json.dumps([
                {"expression": "receive after infinity -> never end"},
                {"expression": "missing_lynx_module:missing()"},
                {"expression": "7-2"},
            ]))
            output = work / "references"
            summary = self.invoke(output, "--replay", source, "--timeout", 1)
            self.assertEqual(summary, {"laws": 0, "passed": 1, "failed": 0,
                                       "unverified": 2, "uncovered": 0})
            unverified = json.loads((output / "unverified.json").read_text())
            self.assertEqual([entry["message"] for entry in unverified],
                             ["OTP reference timed out", "OTP reference called an undefined function"])
            self.assertEqual(json.loads((output / "failures.json").read_text()), [])
            actuals = json.loads((output / "actuals.json").read_text())
            self.assertEqual([entry["status"] for entry in actuals], ["error", "ok", "ok"])
            self.assertIn("Timed out", actuals[0]["message"])
            self.assertEqual(unverified[0]["actual"], actuals[0])

    def test_true_assertions_reject_agreement_on_wrong_results(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            source.write_text(json.dumps([
                {"expression": expression, "assert_true": True}
                for expression in ["true", "false", "1", "{true}", "error(boom)"]
            ] + [{"expression": "false"}]))
            output = work / "assertions"
            summary = self.invoke(output, "--replay", source)
            self.assertEqual(summary, {"laws": 0, "passed": 2, "failed": 4,
                                       "unverified": 0, "uncovered": 0})
            failures = json.loads((output / "failures.json").read_text())
            self.assertEqual([f["case"]["expression"] for f in failures],
                             ["false", "1", "{true}", "error(boom)"])
            self.assertTrue(all(f["case"]["assert_true"] for f in failures))
            self.assertIn("must return true", (output / "comparison.log").read_text())
            self.assertEqual(self.invoke(work / "replay", "--replay", output / "failures.json"),
                             {"laws": 0, "passed": 0, "failed": 4,
                              "unverified": 0, "uncovered": 0})

    def test_lean_inventory_replay_reports_samples_not_proof_coverage(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            checkout = work / "lynx"
            tests = checkout / "Lean/LynxTest"
            tests.mkdir(parents=True)
            lean = tests / "Example.lean"
            lean.write_text("public theorem sampled : True := by trivial\n"
                            "private theorem unsampled : True := by trivial\n#guard true\n")
            origin = ["Example.lean#sampled"]
            source = work / "input.json"
            source.write_text(json.dumps([
                {"lean_origins": origin,
                 "expression": "differential:utf(utf8,<<16#E2,16#82,16#AC,255>>,0)"},
                {"lean_origins": origin,
                 "expression": "differential:short_circuit(or_else,true,throw)"},
                {"lean_origins": origin,
                 "expression": "differential:process_exchange({value,7})"},
                {"lean_origins": origin,
                 "expression": "differential:receive_timeout(-1,[wanted,other])"},
                {"lean_origins": origin, "expression": "missing_lynx_module:missing()"},
            ]))
            output = work / "full"
            summary = self.invoke(output, "--lean-runtime", checkout, "--replay", source)
            self.assertEqual(summary, {"laws": 0, "passed": 4, "failed": 0,
                                       "unverified": 1, "uncovered": 0})
            report = json.loads((output / "lean-coverage.json").read_text())
            self.assertEqual(report[0]["sha256"], hashlib.sha256(lean.read_bytes()).hexdigest())
            sampled, unsampled, guard = report[0]["tests"]
            self.assertEqual(sampled, {"test": "sampled", "line": 1, "status": "has_samples",
                                       "cases": 5, "passed": 4, "failed": 0, "unverified": 1})
            self.assertEqual([r["status"] for r in [unsampled, guard]], ["no_samples"] * 2)
            # Independent public-result oracles for Unicode, skipped effects,
            # child isolation, and a queued match before timeout validation.
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                values = JSON.decode!(File.read!(path))["expected"]
                [utf, short, process, timeout] = Enum.map(Enum.take(values, 4), fn value ->
                  :erlang.binary_to_term(Base.decode64!(value))
                end)
                {:ok, {8364, <<255>>}} = utf
                {:ok, {true, :seen, :undefined}} = short
                {:ok, {true, true, true, true, true, true, :undefined,
                       {:value, 7}, :normal, [:pending]}} = process
                {:ok, {:matched, [:other]}} = timeout
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)
            subset = json.loads(source.read_text())[:1]
            source.write_text(json.dumps(subset))
            partial = work / "partial"
            self.assertEqual(self.invoke(partial, "--lean-runtime", checkout, "--replay", source)["passed"], 1)
            row = json.loads((partial / "lean-coverage.json").read_text())[0]["tests"][0]
            self.assertEqual((row["cases"], row["passed"], row["unverified"]), (1, 1, 0))
            subset[0]["lean_origins"] = ["Example.lean#renamed"]
            source.write_text(json.dumps(subset))
            rejected = subprocess.run([
                sys.executable, str(SCRIPT), "--lean-runtime", str(checkout),
                "--replay", str(source), "--output", str(work / "stale")
            ], capture_output=True, text=True, timeout=30)
            self.assertNotEqual(rejected.returncode, 0)
            self.assertIn("attribution no longer exists", rejected.stderr)

    def test_lifecycle_recipes_have_independent_result_oracles(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            expressions = [
                "differential:linked_exit(trapped,kill)",
                "differential:linked_exit(untrapped,boom)",
                "differential:exit_signal(true,kill)",
                "differential:monitor_options(active_message,[flush,info])",
                "differential:monitor_options(queued_other,[flush,info])",
                "differential:alias_lifetime(reply_demonitor,demonitor)",
                "differential:receive_timeout(1 bsl 100,[])",
                "differential:suspended_receive(message)",
                "differential:termination_cascade(8,target_first)",
                "differential:missing_link(true)",
                "differential:missing_link(false)",
                "differential:process_liveness()",
                "differential:raise_exception(error,reason,[bad])",
                "differential:call(lists,keyfind,[1,1,[{1.0,float},{1,integer}]])",
                "differential:call(erlang,is_process_alive,[bad])",
            ]
            scenarios = ["{kill_query,false,false}", "{kill_query,false,true}",
                         "{kill_query,true,false}", "{kill_query,true,true}",
                         "{normal_down,false}", "{normal_down,true}",
                         "{relink_kill,linked}", "{relink_kill,unlinked}"]
            queue_expressions = [f"differential:signal_queue({scenario},{count})"
                                 for scenario in scenarios for count in [1, 16, 64]]
            by_expr = {c["expression"]: c for c in lean_runtime.cases()}
            source.write_text(json.dumps([{"expression": e} for e in expressions]
                                         + [by_expr[e] for e in queue_expressions]))
            output = work / "results"
            summary = self.invoke(output, "--replay", source)
            # Both ordering fixes must match OTP. Keep the separate unfenced
            # relink timeout diagnostic, not an allowlisted runtime success.
            self.assertEqual(summary["passed"] + summary["failed"], 39)
            self.assertEqual(summary["unverified"], 0)
            self.assertEqual(summary["laws"], 0)
            self.assertEqual(summary["uncovered"], 0)
            failures = json.loads((output / "failures.json").read_text())
            self.assertTrue(all(f["case"]["expression"].startswith(
                "differential:signal_queue({relink_kill,") for f in failures))
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                values = JSON.decode!(File.read!(path))["expected"]
                actual = Enum.map(values, &:erlang.binary_to_term(Base.decode64!(&1)))
                expected = [
                  {:ok, {:ok, {:kill, :kill}}},
                  {:ok, {:exit, :boom}},
                  {:ok, {:killed, :no_reply}},
                  {:ok, {true, [:other]}},
                  {:ok, {false, [{:down, :normal}]}},
                  {:ok, {false, false, []}},
                  {:ok, {{:exception, :error, :timeout_value}, []}},
                  {:ok, {:ok, {{:throw, :message_body}, true, :ran, [:retained]}}},
                  {:ok, {:killed, :killed, []}},
                  {:ok, {:ok, {true, :noproc, []}}},
                  {:ok, {:ok, {:exception, :error, :noproc}}},
                  {:ok, {:ok, {{true, true}, [], {true, {:registered_name, :lynx_liveness_child}},
                               {:undefined, []}, {false, :undefined}}}},
                  {:ok, {:returned, :badarg}},
                  {:ok, {1.0, :float}},
                  {:exception, :error, :badarg}
                ]
                named = {:registered_name, :lynx_signal_queue_child}
                queue_observations = [
                  {[], [], true, :normal, :undefined},
                  {[], :undefined, false, :killed, :undefined},
                  {named, named, true, :normal, :undefined},
                  {named, :undefined, false, :killed, :undefined},
                  {false, :none, [:retained]},
                  {true, {:exit, :normal}, [:retained]},
                  {:killed, {:exit, :killed}, [:retained]},
                  {:killed, :none, [:retained]}
                ]
                queue_expected = for observation <- queue_observations, count <- [1, 16, 64],
                  do: {:ok, {:ok, List.duplicate(observation, count)}}
                true = actual === expected ++ queue_expected
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_custom_iterators_have_independent_oracles_and_replay(self):
        selected = [entry for entry in lean_runtime.cases() if "iterator_scenario" in entry]
        self.assertEqual(len(selected), 240)
        self.assertEqual({entry["iterator_consumer"] for entry in selected
                          if entry["iterator_scenario"] == "consumer"},
                         {"next", "fold", "map", "filter", "filtermap", "foreach"})
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            source.write_text(json.dumps(selected))
            output = work / "results"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 240, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            saved = json.loads((output / "cases.json").read_text())
            source.write_text(json.dumps([saved[i] for i in [239, 0, 213, 129, 222, 31, 228, 232]]))
            replay = work / "replay"
            self.assertEqual(self.invoke(replay, "--replay", source),
                             {"laws": 0, "passed": 8, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                for path <- System.argv() do
                  cases = JSON.decode!(File.read!(Path.join(path, "cases.json")))
                  values = JSON.decode!(File.read!(Path.join(path, "manifest.json")))["expected"]
                  true = length(cases) === length(values)
                  for {entry, encoded} <- Enum.zip(cases, values) do
                    size = entry["iterator_size"] || 0
                    entries = if size == 0, do: [], else: (for k <- 1..size, do: {k, 7*k+3})
                    ordered = fn pairs ->
                      pairs = Enum.sort_by(pairs, fn {k, _} -> {rem(k, 3), k} end)
                      if entry["iterator_order"] == "descending", do: Enum.reverse(pairs), else: pairs
                    end
                    want = case entry["iterator_scenario"] do
                      invalid when invalid in ["invalid_order", "invalid_map"] ->
                        {:exception, :error, :badarg}
                      "comparator_exception" when size > 1 ->
                        {:exception, String.to_existing_atom(entry["iterator_exception"]), :comparator_failed}
                      "comparator_exception" -> {:ok, {entries, entries}}
                      "snapshot" ->
                        updated = [{1, :changed} | Enum.reject(entries, fn {k, _} -> k == size or k == 1 end)]
                        {:ok, {ordered.(entries), ordered.(updated)}}
                      "consumer" ->
                        visits = Enum.drop(ordered.(entries), entry["iterator_skip"])
                        result = case entry["iterator_consumer"] do
                          "next" -> visits
                          "fold" -> Enum.reverse(visits)
                          "map" -> Map.new(visits, fn {k, v} -> {k, {:mapped, k, v}} end)
                          "filter" -> Map.new(Enum.filter(visits, fn {k, _} -> rem(k, 2) == 1 end))
                          "filtermap" ->
                            Map.new(for {k, v} <- visits, rem(k, 3) != 0,
                                      do: {k, if(rem(k, 3) == 1, do: v, else: {:mapped, k, v})})
                          "foreach" -> :ok
                        end
                        {:ok, {result, visits}}
                    end
                    got = :erlang.binary_to_term(Base.decode64!(encoded))
                    if got !== want, do: raise(inspect({entry, want, got}))
                  end
                end
                """, str(output), str(replay)
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_async_exit_has_independent_oracles_and_otp_gate(self):
        otp = subprocess.check_output(["elixir", "-e", "IO.write(System.otp_release())"], text=True)
        if int(otp) < 29:
            self.skipTest("exit_signal/2 requires an OTP 29 oracle")
        selected = [entry for entry in lean_runtime.cases() if "async_exit_mode" in entry]
        self.assertEqual(len(selected), 56)
        self.assertTrue(all(entry["minimum_otp"] == 29 for entry in selected))
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            source.write_text(json.dumps(selected))
            output = work / "results"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 56, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            saved = json.loads((output / "cases.json").read_text())
            source.write_text(json.dumps([saved[i] for i in [55, 0, 42, 19]]))
            replay = work / "replay"
            self.assertEqual(self.invoke(replay, "--replay", source),
                             {"laws": 0, "passed": 4, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                for path <- System.argv() do
                  cases = JSON.decode!(File.read!(Path.join(path, "cases.json")))
                  values = JSON.decode!(File.read!(Path.join(path, "manifest.json")))["expected"]
                  true = length(cases) === length(values)
                  for {entry, encoded} <- Enum.zip(cases, values) do
                    mode = entry["async_exit_mode"]
                    trap = entry["async_exit_trap"] == true
                    reason = case entry["async_exit_reason"] do
                      "normal" -> :normal
                      "kill" -> :kill
                      _ -> {:shutdown, <<5::3>>}
                    end
                    active = mode not in ["inactive_alias", "plain_ref", "reply_then_exit"]
                    fatal = active and (reason == :kill or (not trap and reason != :normal))
                    down = if reason == :kill, do: :killed, else: reason
                    want = cond do
                      mode == "invalid" -> {:exception, :error, :badarg}
                      mode == "dead" -> {:ok, {:ok, true}}
                      mode == "self" and fatal -> {:ok, {:exit, down}}
                      mode == "self" ->
                        messages = if trap, do: [:before_signal, {:exit, reason}, :after_signal],
                                           else: [:before_signal, :after_signal]
                        {:ok, {:ok, {true, messages}}}
                      fatal -> {:ok, {:ok, {down, :no_reply}}}
                      true ->
                        messages = cond do
                          not active or not trap -> [:before_signal, :after_signal]
                          mode == "exit_then_reply" -> [{:exit, reason}, :before_signal, :after_signal]
                          true -> [:before_signal, {:exit, reason}, :after_signal]
                        end
                        alias_state = cond do
                          mode == "pid" -> :not_alias
                          mode in ["alias", "reply_alias"] -> true
                          true -> false
                        end
                        {:ok, {:ok, {:normal, {messages, alias_state}}}}
                    end
                    got = :erlang.binary_to_term(Base.decode64!(encoded))
                    if got !== want, do: raise(inspect({entry, want, got}))
                  end
                end
                """, str(output), str(replay)
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)
            # Even a nested isolated undef must not become a successful oracle
            # on an older host. Force the same gate on every supported host.
            source.write_text(json.dumps([dict(selected[0], minimum_otp=999)]))
            gated = work / "gated"
            self.assertEqual(self.invoke(gated, "--replay", source),
                             {"laws": 0, "passed": 0, "failed": 0,
                              "unverified": 1, "uncovered": 0})
            manifest = json.loads((gated / "manifest.json").read_text())
            self.assertEqual(manifest["expected"], [None])
            self.assertEqual(manifest["oracle_errors"], ["OTP reference requires OTP 999 or newer"])

    def test_exit_reception_controls_and_allowed_traces(self):
        otp = subprocess.check_output(["elixir", "-e", "IO.write(System.otp_release())"], text=True)
        if int(otp) < 29:
            self.skipTest("exit_signal/2 requires an OTP 29 oracle")
        selected = [entry for entry in lean_runtime.cases() if "exit_reception_api" in entry]
        self.assertEqual(len(selected), 54)
        self.assertTrue(all(e.get("assert_true") for e in selected))
        self.assertEqual(sum(e.get("minimum_otp") == 29 for e in selected), 36)
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            source.write_text(json.dumps(selected))
            output = work / "results"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 54, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            saved = json.loads((output / "cases.json").read_text())
            source.write_text(json.dumps([saved[i] for i in [53, 12, 39, 15, 26, 0]]))
            replay = work / "replay"
            self.assertEqual(self.invoke(replay, "--replay", source),
                             {"laws": 0, "passed": 6, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            # Handwritten outcome tables, independent of the helper's state
            # construction. Check raw traces too: two equally invalid VM
            # observations must not turn into a passing differential test.
            check = subprocess.run([
                "elixir", "-e", """
                decode = fn bytes -> bytes |> JSON.decode!() |> :erlang.list_to_binary()
                                          |> :erlang.binary_to_term() end
                traces = fn log ->
                  Regex.scan(~r/^LYNX_EXIT_TRACE (\\[.*\\])$/m, log)
                  |> Enum.map(fn [_, bytes] -> decode.(bytes) end)
                end
                for path <- System.argv() do
                  entries = JSON.decode!(File.read!(Path.join(path, "cases.json")))
                  values = JSON.decode!(File.read!(Path.join(path, "manifest.json")))["expected"]
                  otp_traces = traces.(File.read!(Path.join(path, "proofs.log")))
                  true = length(otp_traces) == length(entries)
                  for {{e, encoded}, otp_trace} <- Enum.zip(Enum.zip(entries, values), otp_traces) do
                    {:ok, true} = :erlang.binary_to_term(Base.decode64!(encoded))
                    api = e["exit_reception_api"]
                    dest = e["exit_reception_destination"]
                    timing = e["exit_reception_timing"]
                    final = e["exit_reception_final"]
                    reason = case e["exit_reception_reason"] do
                      "normal" -> :normal
                      "kill" -> :kill
                      _ -> {:shutdown, <<5::3>>}
                    end
                    # Columns: normal, structured, kill. Each cell lists the
                    # ONLY legal traces; singleton cells are exact controls.
                    table = case {dest, api, timing, final} do
                      {"alias", _, "before", _} -> [[:quiet], [:quiet], [:quiet]]
                      {_, _, "before", true} -> [[:message], [:message], [:killed]]
                      {_, "exit", "before", false} -> [[:dead], [:dead], [:killed]]
                      {_, _, "before", false} -> [[:quiet], [:dead], [:killed]]
                      {_, _, "received", false} -> [[:message], [:message], [:killed]]
                      {_, "exit", "received", true} -> [[:dead], [:dead], [:killed]]
                      {_, _, "received", true} -> [[:quiet], [:dead], [:killed]]
                      {"alias", _, "pending", _} ->
                        [[:quiet, :message], [:quiet, :message, :dead], [:quiet, :killed]]
                      {_, "exit", "pending", true} -> [[:dead], [:dead], [:killed]]
                      {_, _, "pending", _} -> [[:quiet, :message], [:dead, :message], [:killed]]
                    end
                    column = case reason do :normal -> 0; :kill -> 2; _ -> 1 end
                    removed = if dest == "alias", do: true, else: :not_alias
                    legal = for outcome <- Enum.at(table, column) do
                      case outcome do
                        :dead -> {reason, :no_report}
                        :killed -> {:killed, :no_report}
                        :quiet -> {:normal, {not final, removed, [:sentinel]}}
                        :message -> {:normal, {not final, removed, [:sentinel, {:exit, reason}]}}
                      end
                    end
                    [avm_trace] = traces.(File.read!(Path.join(path, e["module"] <> ".log")))
                    for trace <- [otp_trace, avm_trace] do
                      unless trace in legal, do: raise(inspect({e, legal, trace}))
                    end
                  end
                end
                """, str(output), str(replay)
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)
            source.write_text(json.dumps([dict(selected[18], minimum_otp=999)]))
            self.assertEqual(self.invoke(work / "gated", "--replay", source),
                             {"laws": 0, "passed": 0, "failed": 0,
                              "unverified": 1, "uncovered": 0})

    def test_unfenced_self_exit_completion_and_exact_controls(self):
        otp = subprocess.check_output(["elixir", "-e", "IO.write(System.otp_release())"], text=True)
        if int(otp) < 29:
            self.skipTest("exit_signal/2 requires an OTP 29 oracle")
        selected = [e for e in lean_runtime.cases() if "self_exit_completion_mode" in e]
        self.assertEqual(len(selected), 18)
        self.assertTrue(all(e.get("assert_true") for e in selected))
        self.assertEqual(sum(e.get("minimum_otp") == 29 for e in selected), 12)
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            source.write_text(json.dumps(selected))
            output = work / "results"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 18, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            saved = json.loads((output / "cases.json").read_text())
            source.write_text(json.dumps([saved[i] for i in [17, 7, 0, 11, 13, 4]]))
            replay = work / "replay"
            self.assertEqual(self.invoke(replay, "--replay", source),
                             {"laws": 0, "passed": 6, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                traces = fn log ->
                  Regex.scan(~r/^LYNX_COMPLETION_TRACE (\\[.*\\])$/m, log)
                  |> Enum.map(fn [_, bytes] -> bytes |> JSON.decode!()
                    |> :erlang.list_to_binary() |> :erlang.binary_to_term() end)
                end
                report = [{:returned, {true, {:continued, 17, <<5::3>>}}}]
                # Explicit normal/structured/kill columns. Only the three
                # fatal unfenced cells admit multiple outcomes. No formula
                # from the observer determines these independent expectations.
                table = %{
                  {"legacy", false} => [[:stopped], [:stopped], [:stopped]],
                  {"legacy", true} => [[:completed], [:completed], [:stopped]],
                  {"fenced", false} => [[:completed], [:stopped], [:stopped]],
                  {"fenced", true} => [[:completed], [:completed], [:stopped]],
                  {"unfenced", false} => [[:completed],
                    [:stopped, :stopped_after_return, :completed],
                    [:stopped, :stopped_after_return, :completed]],
                  {"unfenced", true} => [[:completed], [:completed],
                    [:stopped, :stopped_after_return, :completed]]
                }
                for path <- System.argv() do
                  cases = JSON.decode!(File.read!(Path.join(path, "cases.json")))
                  values = JSON.decode!(File.read!(Path.join(path, "manifest.json")))["expected"]
                  otp_traces = traces.(File.read!(Path.join(path, "proofs.log")))
                  true = length(cases) == length(values) and length(cases) == length(otp_traces)
                  for {{entry, encoded}, otp_trace} <- Enum.zip(Enum.zip(cases, values), otp_traces) do
                    {:ok, true} = :erlang.binary_to_term(Base.decode64!(encoded))
                    {column, reason} = case entry["self_exit_completion_reason"] do
                      "normal" -> {0, :normal}
                      "kill" -> {2, :killed}
                      "{shutdown,<<5:3>>}" -> {1, {:shutdown, <<5::3>>}}
                    end
                    outcomes = Map.fetch!(table,
                      {entry["self_exit_completion_mode"], entry["self_exit_completion_trap"]})
                    legal = for outcome <- Enum.at(outcomes, column) do
                      case outcome do
                        :stopped -> {reason, [], false}
                        :stopped_after_return -> {reason, report, false}
                        :completed -> {:normal, report, false}
                      end
                    end
                    [actual] = traces.(File.read!(Path.join(path, entry["module"] <> ".log")))
                    for trace <- [otp_trace, actual] do
                      unless trace in legal, do: raise(inspect({entry, legal, trace}))
                    end
                  end
                end
                """, str(output), str(replay)
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)
            source.write_text(json.dumps([dict(selected[6], minimum_otp=999)]))
            gated = work / "gated"
            self.assertEqual(self.invoke(gated, "--replay", source),
                             {"laws": 0, "passed": 0, "failed": 0,
                              "unverified": 1, "uncovered": 0})
            manifest = json.loads((gated / "manifest.json").read_text())
            self.assertEqual(manifest["expected"], [None])
            self.assertEqual(manifest["oracle_errors"], ["OTP reference requires OTP 999 or newer"])

    def test_query_notifications_have_independent_outcomes_and_controls(self):
        otp = subprocess.check_output(["elixir", "-e", "IO.write(System.otp_release())"], text=True)
        if int(otp) < 29:
            self.skipTest("pending self-signals require an OTP 29 oracle")
        selected = [e for e in lean_runtime.cases() if "query_notification_family" in e]
        self.assertEqual(len(selected), 46)
        self.assertTrue(all(e.get("assert_true") for e in selected))
        self.assertEqual(sum(e.get("minimum_otp") == 29 for e in selected), 40)
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            source.write_text(json.dumps(selected))
            output = work / "results"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 46, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            saved = json.loads((output / "cases.json").read_text())
            source.write_text(json.dumps([saved[i] for i in [45, 4, 6, 23, 38, 12]]))
            replay = work / "replay"
            self.assertEqual(self.invoke(replay, "--replay", source),
                             {"laws": 0, "passed": 6, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                traces = fn log ->
                  Regex.scan(~r/^LYNX_QUERY_TRACE (\\[.*\\])$/m, log)
                  |> Enum.map(fn [_, bytes] -> bytes |> JSON.decode!()
                    |> :erlang.list_to_binary() |> :erlang.binary_to_term() end)
                end
                for path <- System.argv() do
                  cases = JSON.decode!(File.read!(Path.join(path, "cases.json")))
                  values = JSON.decode!(File.read!(Path.join(path, "manifest.json")))["expected"]
                  otp_traces = traces.(File.read!(Path.join(path, "proofs.log")))
                  true = length(cases) == length(otp_traces)
                  for {{entry, encoded}, otp_trace} <- Enum.zip(Enum.zip(cases, values), otp_traces) do
                    {:ok, true} = :erlang.binary_to_term(Base.decode64!(encoded))
                    reason = case entry["query_notification_reason"] do
                      "normal" -> :normal
                      "kill" -> :kill
                      _ -> {:shutdown, <<5::3>>}
                    end
                    legal = case entry["query_notification_family"] do
                      "monitor" ->
                        pairs = case entry["query_notification_state"] do
                          "live" -> [{true, reason}]
                          "dead" -> [{false, :noproc}]
                          "racing" -> [{true, reason}, {false, reason}, {true, :noproc}, {false, :noproc}]
                        end
                        for {alive, down} <- pairs,
                          do: {alive, down, false, [:sentinel, {:retained, <<5::3>>}]}
                      "query" ->
                        value = case {entry["query_notification_query"], entry["query_notification_state"]} do
                          {"alive", "live"} -> true
                          {"alive", "dead"} -> false
                          {"info", "live"} -> []
                          {"info", "dead"} -> :undefined
                        end
                        queried = {:queried, value}
                        # Handwritten behavior table. A query before sending
                        # a fatal signal MUST report; a received fatal signal
                        # MUST preclude a report. Only pending allows either.
                        outcome = case {entry["query_notification_trap"], reason} do
                          {false, :normal} -> {:survived, [:sentinel]}
                          {true, :normal} -> {:survived, [:sentinel, {:exit, :normal}]}
                          {_, :kill} -> {:died, :killed}
                          {false, _} -> {:died, {:shutdown, <<5::3>>}}
                          {true, _} -> {:survived, [:sentinel, {:exit, {:shutdown, <<5::3>>}}]}
                        end
                        case outcome do
                          {:survived, messages} -> [{:normal, [queried, {:completed, messages}]}]
                          {:died, down} ->
                            case entry["query_notification_timing"] do
                              "before" -> [{down, [queried]}]
                              "received" -> [{down, []}]
                              "pending" -> [{down, []}, {down, [queried]}]
                            end
                        end
                    end
                    [actual] = traces.(File.read!(Path.join(path, entry["module"] <> ".log")))
                    for trace <- [otp_trace, actual] do
                      unless trace in legal, do: raise(inspect({entry, legal, trace}))
                    end
                  end
                end
                """, str(output), str(replay)
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)
            source.write_text(json.dumps([dict(selected[6], minimum_otp=999)]))
            self.assertEqual(self.invoke(work / "gated", "--replay", source),
                             {"laws": 0, "passed": 0, "failed": 0,
                              "unverified": 1, "uncovered": 0})

    def test_fanout_and_ending_requests_have_independent_oracles(self):
        selected = [entry for entry in lean_runtime.cases() if "termination_scenario" in entry]
        self.assertEqual([(e["termination_scenario"], e["termination_mode"], e["peers"])
                          for e in selected],
                         [(kind, mode, peers) for peers in [1, 4, 16]
                          for kind, modes in [("fanout", ["normal", "shutdown", "kill"]),
                                              ("requests", ["live", "dead", "racing"])]
                          for mode in modes])
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            source.write_text(json.dumps(selected))
            output = work / "results"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 18, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            saved = json.loads((output / "cases.json").read_text())
            source.write_text(json.dumps([saved[i] for i in [17, 0, 13, 4]]))
            replay = work / "replay"
            self.assertEqual(self.invoke(replay, "--replay", source),
                             {"laws": 0, "passed": 4, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                for path <- System.argv() do
                  cases = JSON.decode!(File.read!(Path.join(path, "cases.json")))
                  values = JSON.decode!(File.read!(Path.join(path, "manifest.json")))["expected"]
                  true = length(cases) === length(values)
                  for {entry, encoded} <- Enum.zip(cases, values) do
                    peers = entry["peers"]
                    want = case entry["termination_scenario"] do
                      "fanout" ->
                        reason = case entry["termination_mode"] do
                          "normal" -> :normal
                          "shutdown" -> {:shutdown, :fanout}
                          "kill" -> :killed
                        end
                        events = fn n -> [{:before_exit, {n, n * 17 + 3}},
                                          {:exit, reason}, {:down, reason}] end
                        reports = for n <- 1..peers,
                          do: {n, events.(n), false, :undefined, [:retained]}
                        {events.(0), reports, [:retained]}
                      "requests" ->
                        reports = for n <- 1..peers,
                          do: {n, :allowed, :allowed, false, :undefined, [:retained]}
                        {reports, [:retained]}
                    end
                    got = :erlang.binary_to_term(Base.decode64!(encoded))
                    if got !== {:ok, {:ok, want}}, do: raise(inspect({entry, want, got}))
                  end
                end
                """, str(output), str(replay)
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_map_callbacks_preserve_arguments_effects_and_exceptions(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            expressions = [
                "differential:map_callback(merge_with,[#{a=>3},#{a=>11,b=>7}],valid)",
                "differential:map_callback(merge_with,[#{1=>left},#{1.0=>right}],valid)",
                "differential:map_callback(merge_with,[#{a=>3},#{b=>11}],throw)",
                "differential:map_callback(merge_with,[#{a=>3},#{a=>11}],error)",
                "differential:map_callback(filter,[#{keep=>ordinary,drop=>selected,skip=>ordinary}],valid)",
                "differential:map_callback(filter,[#{1=>selected,1.0=>ordinary}],valid)",
                "differential:map_callback(filter,[#{}],wrong_arity)",
                "differential:map_callback(filter,[#{keep=>selected}],exit)",
                "differential:map_callback(intersect_with,[#{a=>3},#{a=>11,b=>7}],valid)",
                "differential:map_callback(filtermap,[#{keep=>7,drop=>11,change=>9}],transform)",
                "differential:map_callback(update_with,[missing,initial,#{a=>3}],throw)",
                "differential:map_callback(update_with,[a,#{a=>3}],error)",
                "differential:map_callback(filter,[#{keep=>selected}],nonboolean)",
                "differential:map_callback(merge_with,[#{},#{}],wrong_arity)",
            ]
            source.write_text(json.dumps([{"expression": e} for e in expressions]))
            output = work / "results"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 14, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                actual = JSON.decode!(File.read!(path))["expected"]
                         |> Enum.map(&:erlang.binary_to_term(Base.decode64!(&1)))
                [
                  {:ok, {{:ok, %{a: {:a, 3, 11}, b: 7}}, [[:a, 3, 11]]}},
                  {:ok, {{:ok, %{1 => :left, 1.0 => :right}}, []}},
                  {:ok, {{:ok, %{a: 3, b: 11}}, []}},
                  {:ok, {{:exception, :error, :callback_failed}, [[:a, 3, 11]]}},
                  {:ok, {{:ok, %{keep: :ordinary, drop: :selected}},
                         [[:drop, :selected], [:keep, :ordinary], [:skip, :ordinary]]}},
                  {:ok, {{:ok, %{1 => :selected}}, [[1.0, :ordinary], [1, :selected]]}},
                  {:ok, {{:exception, :error, :badarg}, []}},
                  {:ok, {{:exception, :exit, :callback_failed}, [[:keep, :selected]]}},
                  {:ok, {{:ok, %{a: {:a, 3, 11}}}, [[:a, 3, 11]]}},
                  {:ok, {{:ok, %{keep: 7, change: {:change, 9}}},
                         [[:change, 9], [:drop, 11], [:keep, 7]]}},
                  {:ok, {{:ok, %{a: 3, missing: :initial}}, []}},
                  {:ok, {{:exception, :error, :callback_failed}, [[3]]}},
                  {:ok, {{:exception, :error, {:case_clause, :not_boolean}}, [[:keep, :selected]]}},
                  {:ok, {{:exception, :error, :badarg}, []}}
                ] = actual
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_set_recipes_preserve_exact_elements_and_operand_direction(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            expressions = [
                "differential:set_algebra([a,b],[b,c,d])",
                "differential:set_algebra([1],[1.0])",
                "differential:set_algebra([a],[a,b,c])",
                "differential:set_elements(sets:from_list([1,1.0,1],[{version,2}]))",
                "begin S=sets:from_list([1],[{version,2}]),"
                "{sets:is_element(1,S),sets:is_element(1.0,S),"
                "differential:set_elements(sets:del_element(1.0,S))} end",
                "differential:set_elements(sets:intersection([]))",
            ]
            source.write_text(json.dumps([{"expression": e} for e in expressions]))
            output = work / "results"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 6, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                [overlap, numeric, subset, duplicates, membership, invalid] =
                  JSON.decode!(File.read!(path))["expected"]
                  |> Enum.map(&:erlang.binary_to_term(Base.decode64!(&1)))
                {:ok, %{
                  left: {2, false, 2, %{a: :present, b: :present}},
                  right: {3, false, 3, %{b: :present, c: :present, d: :present}},
                  union: {4, false, 4, %{a: :present, b: :present, c: :present, d: :present}},
                  intersection: {1, false, 1, %{b: :present}},
                  left_only: {1, false, 1, %{a: :present}},
                  right_only: {2, false, 2, %{c: :present, d: :present}},
                  subset: {false, false}, disjoint: {false, false}, equal: {false, false},
                  relations: {true, true, true, true, true, true}
                }} = overlap
                {:ok, %{
                  union: {2, false, 2, %{1 => :present, 1.0 => :present}},
                  intersection: {0, true, 0, empty},
                  left_only: {1, false, 1, %{1 => :present}},
                  right_only: {1, false, 1, %{1.0 => :present}},
                  subset: {false, false}, disjoint: {true, true}, equal: {false, false},
                  relations: {true, true, true, true, true, true}
                }} = numeric
                0 = map_size(empty)
                {:ok, %{subset: {true, false}, left_only: {0, true, 0, empty},
                        right_only: {2, false, 2, %{b: :present, c: :present}}}} = subset
                {:ok, {2, false, 2, %{1 => :present, 1.0 => :present}}} = duplicates
                {:ok, {true, false, {1, false, 1, %{1 => :present}}}} = membership
                {:exception, :error, :function_clause} = invalid
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_function_recipes_have_independent_result_oracles(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            expressions = [
                "differential:function_apply(local_one,captured,[7])",
                "differential:function_apply(bif_zero,captured,[7])",
                "differential:function_apply(missing_export,captured,[7])",
                "differential:function_make(differential,function_one,1,[7])",
                "differential:function_make(differential,function_one,256,[7])",
                "differential:function_named(differential,missing,[])",
                "differential:function_spawn(external,linked,payload)",
                "differential:function_spawn(local_one,monitored,payload)",
            ]
            source.write_text(json.dumps([{"expression": e} for e in expressions]))
            output = work / "results"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 8, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                actual = JSON.decode!(File.read!(path))["expected"]
                         |> Enum.map(&:erlang.binary_to_term(Base.decode64!(&1)))
                [
                  {:ok, {:ok, {:captured, 7}}},
                  {:ok, {:error, :badarity, true, true}},
                  {:ok, {:error, :badarity, true, true}},
                  {:ok, {true, {:module, :differential}, {:name, :function_one},
                         {:arity, 1}, {:ok, {:one, 7}}}},
                  {:exception, :error, :badarg},
                  {:ok, {:error, :undef}},
                  {:ok, {:ok, {:child, :normal, [{:reply, :payload}, {:exit, :normal}]}}},
                  {:ok, {:ok, {:child, {:badarity, true, true, true}, []}}}
                ] = actual
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_nested_fallthrough_has_independent_oracles(self):
        selected = [entry for entry in lean_runtime.cases() if "fallthrough_scenario" in entry]
        self.assertEqual(len(selected), 32)
        self.assertEqual(sum("core_body" in entry for entry in selected), 16)
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "fallthrough.json"
            source.write_text(json.dumps(selected))
            output = work / "results"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 32, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            saved = json.loads((output / "cases.json").read_text())
            source.write_text(json.dumps([saved[i] for i in [31, 4, 16, 12]]))
            replay = work / "replay"
            self.assertEqual(self.invoke(replay, "--replay", source),
                             {"laws": 0, "passed": 4, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                keep = {:anchor, 13}
                saved = {keep, 97}
                prefix = [:capture, :input, :saved]
                ok = fn value, rest -> {{:ok, {keep, saved, value}}, prefix ++ rest ++ [:join]} end
                err = fn reason, rest -> {{:exception, :error, reason}, prefix ++ rest} end
                # Expected branch payloads and traces are handwritten. Guard
                # errors fall through; selected-body errors must never do so.
                expected = %{
                  "first_match" => ok.({:first, {:probe, :yes}, 41}, [:body_first]),
                  "guard_false" => ok.({:fallback, {:group, {:tag, {:probe, :no}}, 43}}, [:inner, :outer]),
                  "guard_error" => ok.({:fallback, {:group, {:tag, {:short}}, 47}}, [:inner, :outer]),
                  "later_match" => ok.({:second, 7, 53}, [:body_second]),
                  "inner_pattern_miss" => ok.({:fallback, {:group, {:other, 99}, 59}}, [:inner, :outer]),
                  "direct_match" => ok.({:direct, 61}, [:body_direct]),
                  "outer_pattern_miss" => ok.({:fallback, :not_group}, [:outer]),
                  "first_body_error" => err.(:body_first, [:body_first]),
                  "later_body_error" => err.(:body_second, [:body_second]),
                  "direct_body_error" => err.(:body_direct, [:body_direct]),
                  "inner_error" => err.(:inner, [:inner]),
                  "outer_error" => err.(:outer, [:inner, :outer]),
                  "outer_direct_error" => err.(:outer, [:outer]),
                  "input_error" => {{:exception, :error, :input}, [:capture, :input]},
                  "saved_error" => err.(:saved, []),
                  "join_error" => err.(:join, [:body_second, :join])
                }
                for path <- System.argv() do
                  cases = JSON.decode!(File.read!(Path.join(path, "cases.json")))
                  values = JSON.decode!(File.read!(Path.join(path, "manifest.json")))["expected"]
                  true = length(cases) === length(values)
                  for {entry, encoded} <- Enum.zip(cases, values) do
                    want = {:ok, Map.fetch!(expected, entry["fallthrough_scenario"])}
                    got = :erlang.binary_to_term(Base.decode64!(encoded))
                    if got !== want, do: raise(inspect({entry, want, got}))
                  end
                end
                """, str(output), str(replay)
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_escaping_recursive_closures_have_independent_oracles(self):
        selected = [entry for entry in lean_runtime.cases() if "recursive_closure_kind" in entry]
        self.assertEqual([(e["recursive_closure_kind"], e["recursive_closure_lifetime"],
                           e["recursive_closure_profile"]) for e in selected],
                         [(kind, lifetime, profile) for kind in ["tail", "non_tail"]
                          for lifetime in ["local", "gc", "message_gc", "creator_exit"]
                          for profile in ["empty", "numeric", "compound"]])
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "closures.json"
            source.write_text(json.dumps(selected))
            output = work / "results"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 24, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            saved = json.loads((output / "cases.json").read_text())
            source.write_text(json.dumps([saved[i] for i in [23, 0, 14]]))
            replay = work / "replay"
            self.assertEqual(self.invoke(replay, "--replay", source),
                             {"laws": 0, "passed": 3, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                for path <- System.argv() do
                  cases = JSON.decode!(File.read!(Path.join(path, "cases.json")))
                  values = JSON.decode!(File.read!(Path.join(path, "manifest.json")))["expected"]
                  true = length(cases) === length(values)
                  for {entry, encoded} <- Enum.zip(cases, values) do
                    # Explicit orders distinguish tail accumulation from unwind
                    # construction, rather than deriving expectations from OTP.
                    {left, right, forward, backward} = case entry["recursive_closure_profile"] do
                      "empty" -> {:a, :b, [], []}
                      "numeric" -> {1, 1.0, [4], [4]}
                      "compound" -> {%{k: [1, <<5::size(3)>>]}, {:right, [-7 | :tail]},
                                     [2, 5, 9], [9, 5, 2]}
                    end
                    a = {:left, left, <<17, 34, 51>>}
                    b = {:right, right, <<5::size(3)>>}
                    tail? = entry["recursive_closure_kind"] === "tail"
                    rows = fn xs, seed -> Enum.map(xs, &{a, &1, b}) ++ [seed] end
                    first = rows.(if(tail?, do: backward, else: forward), :first)
                    second = rows.(if(tail?, do: [3, 7], else: [7, 3]), :second)
                    events = fn tag, xs -> Enum.map(xs, &{tag, &1}) end
                    first_trace = [:first] ++ events.(:down, forward) ++ [:base] ++
                                  if(tail?, do: [], else: events.(:up, backward))
                    second_trace = [:second, {:down, 7}, {:down, 3}, :base] ++
                                   if(tail?, do: [], else: [{:up, 3}, {:up, 7}])
                    result = {{a, first, b}, {a, second, b}, {true, true}}
                    want = {:ok, {:ok, {{:ok, result}, first_trace ++ second_trace}}}
                    got = :erlang.binary_to_term(Base.decode64!(encoded))
                    if got !== want, do: raise(inspect({entry, want, got}))
                  end
                end
                """, str(output), str(replay)
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_core_try_results_and_boundaries_have_independent_oracles(self):
        selected = [entry for entry in lean_runtime.cases() if "try_result_scenario" in entry]
        self.assertEqual(len(selected), 56)
        self.assertEqual(sum("core_body" in entry for entry in selected), 36)
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "try.json"
            source.write_text(json.dumps(selected))
            output = work / "results"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 56, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            # Reindex mixed zero/pair Core and source entries from saved input.
            saved = json.loads((output / "cases.json").read_text())
            source.write_text(json.dumps([saved[i] for i in [55, 0, 23, 47, 50]]))
            replay = work / "replay"
            self.assertEqual(self.invoke(replay, "--replay", source),
                             {"laws": 0, "passed": 5, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                for path <- System.argv() do
                  cases = JSON.decode!(File.read!(Path.join(path, "cases.json")))
                  values = JSON.decode!(File.read!(Path.join(path, "manifest.json")))["expected"]
                  true = length(cases) === length(values)
                  for {entry, encoded} <- Enum.zip(cases, values) do
                    pair? = entry["try_result_arity"] === 2
                    result = if pair?, do: {17, :left}, else: :empty
                    caught = if pair?, do: {:caught, {:protected, 23}}, else: :empty
                    prefix = if pair?, do: [:protected, :left, :right], else: [:protected]
                    ok = fn value, rest -> {{:ok, value}, prefix ++ rest} end
                    err = fn class, reason, rest -> {{:exception, class, reason}, prefix ++ rest} end
                    # Distinct payloads locate the exception; traces detect a
                    # wrongly entered handler/body or duplicated/missing effect.
                    expected = %{
                      "success" => ok.(result, [:body]),
                      "protected_error" => ok.({:caught, :error, {:protected, 23}}, [:handler]),
                      "protected_throw" => ok.({:caught, :throw, {:protected, 23}}, [:handler]),
                      "protected_exit" => ok.({:caught, :exit, {:protected, 23}}, [:handler]),
                      "body_error" => err.(:error, {:body, 31}, [:body]),
                      "body_throw" => err.(:throw, {:body, 31}, [:body]),
                      "body_exit" => err.(:exit, {:body, 31}, [:body]),
                      "handler_error" => err.(:error, {:handler, 41}, [:handler]),
                      "handler_throw" => err.(:throw, {:handler, 41}, [:handler]),
                      "handler_exit" => err.(:exit, {:handler, 41}, [:handler]),
                      "let_success" => ok.(result, [:body, :after]),
                      "let_protected_throw" => ok.(caught, [:handler, :after]),
                      "case_success" => ok.(result, [:body, :match, :after]),
                      "case_protected_throw" => ok.(caught, [:handler, :match, :after]),
                      "nested_success" => ok.(result, [:body, :outer]),
                      "nested_protected_throw" => ok.(caught, [:handler, :outer]),
                      "nested_body_error" => ok.({:escaped, :error, {:body, 31}}, [:body, :outer_handler]),
                      "nested_handler_exit" => ok.({:escaped, :exit, {:handler, 41}}, [:handler, :outer_handler])
                    }
                    want = {:ok, Map.fetch!(expected, entry["try_result_scenario"])}
                    got = :erlang.binary_to_term(Base.decode64!(encoded))
                    if got !== want, do: raise(inspect({entry, want, got}))
                  end
                end
                """, str(output), str(replay)
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_lowered_receive_continuations_have_independent_oracles(self):
        selected = [e for e in lean_runtime.cases() if "receive_continuation_family" in e]
        self.assertEqual(len(selected), 32)
        self.assertEqual(sum("core_body" in e for e in selected), 16)
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            source.write_text(json.dumps(selected))
            output = work / "results"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 32, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            saved = json.loads((output / "cases.json").read_text())
            source.write_text(json.dumps([saved[i] for i in [31, 2, 24, 7, 27, 6]]))
            replay = work / "replay"
            self.assertEqual(self.invoke(replay, "--replay", source),
                             {"laws": 0, "passed": 6, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                full = [:sentinel, {:take, 7.0}, {:take, 7}, {:take, 9}, :tail]
                unmatched = [:sentinel, {:take, 7.0}, :tail]
                remaining_first = [:sentinel, {:take, 7.0}, {:take, 9}, :tail]
                remaining_second = [:sentinel, {:take, 7.0}, {:take, 7}, :tail]
                ok = fn value, effects, mailbox -> {{:ok, value}, effects, mailbox} end
                err = fn reason, effects, mailbox -> {{:exception, :error, reason}, effects, mailbox} end
                sequence = [:capture, :first, :second, :join, :cleanup]
                matched = [:capture, :resumed, :join, :cleanup]
                timed = [:capture, :timeout, :resumed, :join, :cleanup]
                expected = %{
                  {"sequence", "success"} => ok.({{:anchor, 13}, :two}, sequence, full),
                  {"sequence", "first_error"} => ok.({:caught, :error, :first}, [:capture, :first, :handler, :cleanup], full),
                  {"sequence", "second_error"} => ok.({:caught, :error, :second}, [:capture, :first, :second, :handler, :cleanup], full),
                  {"sequence", "join_error"} => err.(:join, sequence, full),
                  {"sequence", "handler_error"} => err.(:handler, [:capture, :first, :handler, :cleanup], full),
                  {"sequence", "cleanup_error"} => err.(:cleanup, sequence, full),
                  {"sequence", "cleanup_overrides_error"} => err.(:cleanup, sequence, full),
                  {"discarded", "first"} => ok.({{:anchor, 13}, :selected}, matched, remaining_first),
                  {"discarded", "second"} => ok.({{:anchor, 13}, :selected}, matched, remaining_second),
                  {"discarded", "timeout"} => ok.({{:anchor, 13}, :selected}, timed, unmatched),
                  {"discarded", "join_error"} => err.(:join, matched, remaining_first),
                  {"discarded", "timeout_error"} => ok.({:caught, :error, :timeout}, [:capture, :timeout, :handler, :cleanup], unmatched),
                  {"discarded", "timeout_join_error"} => err.(:join, timed, unmatched),
                  {"discarded", "handler_error"} => err.(:handler, [:capture, :timeout, :handler, :cleanup], unmatched),
                  {"discarded", "cleanup_error"} => err.(:cleanup, matched, remaining_first),
                  {"rescan", "success"} => ok.({{:anchor, 13}, :selected}, [:capture, :first, :resumed, :join, :cleanup], remaining_first)
                }
                for path <- System.argv() do
                  cases = JSON.decode!(File.read!(Path.join(path, "cases.json")))
                  values = JSON.decode!(File.read!(Path.join(path, "manifest.json")))["expected"]
                  true = length(cases) === length(values)
                  for {entry, encoded} <- Enum.zip(cases, values) do
                    {outcome, trace, mailbox} = Map.fetch!(expected,
                      {entry["receive_continuation_family"], entry["receive_continuation_scenario"]})
                    want = {:ok, {:ok, {{outcome, trace}, mailbox}}}
                    # Check each VM independently: equal but wrong traces must
                    # not pass. Mailbox order and numeric types stay exact.
                    got = :erlang.binary_to_term(Base.decode64!(encoded))
                    log = File.read!(Path.join(path, entry["module"] <> ".log"))
                    [[_, bytes]] = Regex.scan(~r/^LYNX_RESULT (\\[.*\\])$/m, log)
                    actual = bytes |> JSON.decode!() |> :erlang.list_to_binary() |> :erlang.binary_to_term()
                    if got !== want or actual !== want,
                      do: raise(inspect({entry, want, got, actual}))
                  end
                end
                """, str(output), str(replay)
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_binary_construction_order_has_independent_oracles(self):
        selected = [entry for entry in lean_runtime.cases() if "binary_order_scenario" in entry]
        self.assertEqual(len(selected), 56)
        self.assertEqual(sum("core_body" in entry for entry in selected), 28)
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "binary.json"
            source.write_text(json.dumps(selected))
            output = work / "results"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 56, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                ok = fn value, trace -> {{:ok, value}, trace} end
                err = fn reason, trace -> {{:exception, :error, reason}, trace} end
                all = [:v1, :s1, :v2, :s2]
                # 101 followed by 10011 gives 10110011, not two full bytes.
                expected = %{
                  "segments_success" => ok.(<<179>>, all),
                  "value_error" => err.(:v1, [:v1]),
                  "size_error" => err.(:s1, [:v1, :s1]),
                  "later_value_error" => err.(:v2, [:v1, :s1, :v2]),
                  "later_size_error" => err.(:s2, all),
                  "bad_value" => err.(:badarg, all),
                  "bad_size" => err.(:badarg, all),
                  "bad_value_later_error" => err.(:v2, [:v1, :s1, :v2]),
                  "bad_size_later_error" => err.(:s2, all),
                  "zero_bad_value" => err.(:badarg, all),
                  "zero_width" => ok.(<<19::size(5)>>, all),
                  "binary_short" => err.(:badarg, [:v1, :s1, :v2]),
                  "binary_short_later_error" => err.(:v2, [:v1, :s1, :v2]),
                  "utf_bad" => err.(:badarg, [:v1, :v2]),
                  "utf_bad_later_error" => err.(:v2, [:v1, :v2]),
                  "append_success" => ok.(<<18, 52, 19>>, [:p, :v]),
                  "append_bad_prefix" => err.(:badarg, [:p, :v]),
                  "append_bad_prefix_later_error" => err.(:v, [:p, :v]),
                  "append_unaligned_binary" => err.(:badarg, [:p, :v]),
                  "append_unaligned_bitstring" => ok.(<<179>>, [:p, :v]),
                  "append_shared_prefix" => ok.({<<18, 52>>, <<18, 52, 19>>}, [:p, :v]),
                  "nested_call_success" => ok.(<<5>>, [:v, :s, :call]),
                  "nested_call_bad_value" => err.(:badarg, [:v, :s]),
                  "tuple_success" => ok.({<<5>>, [:tail]}, [:v, :s, :tail]),
                  "tuple_bad_value" => err.(:badarg, [:v, :s]),
                  "cons_success" => ok.([<<5>>, :tail], [:v, :s, :tail]),
                  "cons_bad_value" => err.(:badarg, [:v, :s]),
                  "cons_bad_value_tail_error" => err.(:badarg, [:v, :s])
                }
                cases = JSON.decode!(File.read!(Path.join(path, "cases.json")))
                values = JSON.decode!(File.read!(Path.join(path, "manifest.json")))["expected"]
                # Raw Core leaves the final head construction after the tail.
                # Ordinary source lowering binds that construction beforehand.
                for {entry, encoded} <- Enum.zip(cases, values) do
                  scenario = entry["binary_order_scenario"]
                  want = case {entry["binary_order_path"], scenario} do
                    {"core", "cons_bad_value"} -> err.(:badarg, [:v, :s, :tail])
                    {"core", "cons_bad_value_tail_error"} -> err.(:tail, [:v, :s, :tail])
                    _ -> Map.fetch!(expected, scenario)
                  end
                  got = :erlang.binary_to_term(Base.decode64!(encoded))
                  if got !== {:ok, want}, do: raise(inspect({entry, want, got}))
                end
                """, str(output)
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_core_evaluation_order_and_replay_have_independent_oracles(self):
        selected = [entry for entry in lean_runtime.cases() if "core_order_scenario" in entry]
        self.assertEqual(len(selected), 34)
        self.assertEqual(sum("core_body" in entry for entry in selected), 32)
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "core.json"
            source.write_text(json.dumps(selected))
            output = work / "results"
            summary = self.invoke(output, "--replay", source)
            self.assertEqual(summary["passed"] + summary["failed"], 34)
            self.assertEqual(summary["unverified"], 0)
            self.assertEqual(summary["laws"], 0)
            self.assertEqual(summary["uncovered"], 0)
            # Missing-key reason remains a visible differential failure, not an
            # allowlisted pass. All ordering scenarios must match; this check
            # also stays valid once the missing-key opcode reason is corrected.
            failures = json.loads((output / "failures.json").read_text())
            self.assertTrue(all(f["case"]["core_order_scenario"] == "map_missing_key"
                                for f in failures))
            # Reindex a mixed Core/source subset from the saved cases, not the
            # recipes: companions and macros must follow the replay's modules.
            saved = json.loads((output / "cases.json").read_text())
            source.write_text(json.dumps([saved[-1], saved[4], saved[28]]))
            replay = work / "replay"
            self.assertEqual(self.invoke(replay, "--replay", source),
                             {"laws": 0, "passed": 3, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                ok = fn trace -> {{:ok, [:seed, :tail]}, trace} end
                err = fn reason, trace -> {{:exception, :error, reason}, trace} end
                expected = %{
                  "direct_success" => ok.([:h, :f]),
                  "direct_head_error" => err.(:f, [:h, :f]),
                  "direct_tail_error" => err.(:h, [:h]),
                  "direct_both_error" => err.(:h, [:h]),
                  "nested_call_success" => ok.([:g, :h, :f]),
                  "nested_call_head_error" => err.(:f, [:g, :h, :f]),
                  "nested_call_tail_error" => err.(:h, [:g, :h]),
                  "nested_call_both_error" => err.(:h, [:g, :h]),
                  "nested_call_prefix_error" => err.(:g, [:g]),
                  "let_success" => ok.([:g, :h, :f]),
                  "let_both_error" => err.(:h, [:g, :h]),
                  "let_prefix_error" => err.(:g, [:g]),
                  "sequence_success" => ok.([:g, :h, :f]),
                  "sequence_both_error" => err.(:h, [:g, :h]),
                  "case_success" => ok.([:g, :h, :f]),
                  "case_both_error" => err.(:h, [:g, :h]),
                  "shadowed_let_success" => ok.([:g, :h, :f]),
                  "shadowed_let_both_error" => err.(:h, [:g, :h]),
                  "shadowed_let_prefix_error" => err.(:g, [:g]),
                  "local_function_success" => ok.([:g, :h, :f]),
                  "local_function_both_error" => err.(:h, [:g, :h]),
                  "try_head_success" => ok.([:h, :g, :f]),
                  "try_head_both_error" => err.(:h, [:h]),
                  "case_miss" => err.({:case_clause, :seed}, [:g, :h]),
                  "case_miss_tail_error" => err.(:h, [:g, :h]),
                  "let_capture" => {{:ok, [:seed, :outside]}, [:g, :h, :f]},
                  "tuple_arguments" => {{:ok, [{7, 9}, :tail]}, [:g1, :g2, :h, :f]},
                  "tuple_argument_error" => err.(:g2, [:g1, :g2]),
                  "map_update" => {{:ok, [%{a: 1, b: 2}, :tail]}, [:v, :h]},
                  "map_missing_key" => err.({:badkey, :a}, [:v, :h]),
                  "map_missing_key_tail_error" => err.(:h, [:v, :h]),
                  "map_bad_base" => err.({:badmap, :bad}, [:v]),
                  "source_success" => ok.([:g, :f, :h]),
                  "source_both_error" => err.(:f, [:g, :f])
                }
                for path <- System.argv() do
                  cases = JSON.decode!(File.read!(Path.join(path, "cases.json")))
                  values = JSON.decode!(File.read!(Path.join(path, "manifest.json")))["expected"]
                  for {entry, encoded} <- Enum.zip(cases, values) do
                    value = :erlang.binary_to_term(Base.decode64!(encoded))
                    true = value === {:ok, Map.fetch!(expected, entry["core_order_scenario"])}
                  end
                end
                """, str(output), str(replay)
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_compiler_folded_and_runtime_exceptions_have_independent_oracles(self):
        selected = [entry for entry in lean_runtime.cases() if "exception_path" in entry]
        scenarios = ["atom_match", "atom_miss", "numeric_miss", "two_arg_miss",
                     "map_match", "map_miss", "guard_match", "guard_false", "guard_error",
                     "later_clause", "badfun", "badarity", "body_error", "body_throw", "body_exit"]
        self.assertEqual([(entry["exception_scenario"], entry["exception_path"])
                          for entry in selected],
                         [(scenario, path) for scenario in scenarios
                          for path in ["literal", "effectful_literal", "runtime"]])
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "exceptions.json"
            source.write_text(json.dumps(selected))
            output = work / "results"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 45, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            # Handwritten outcomes distinguish atom vs tuple reasons, swallowed
            # guard errors, argument order/count, and entering a rejected body.
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                actual = JSON.decode!(File.read!(path))["expected"]
                         |> Enum.map(&:erlang.binary_to_term(Base.decode64!(&1)))
                success = {{:ok, :matched}, [:before, :arg1, :body]}
                miss = {{:exception, :error, :function_clause}, [:before, :arg1]}
                expected = [
                  success, miss, miss,
                  {{:exception, :error, :function_clause}, [:before, :arg1, :arg2]},
                  success, miss, success, miss, miss, success,
                  {{:exception, :error, {:badfun, :not_fun}}, [:before, :arg1]},
                  {{:exception, :error, {:badarity, true, [7, 9]}}, [:before, :arg1, :arg2]},
                  {{:exception, :error, :body_failed}, [:before, :arg1, :body]},
                  {{:exception, :throw, :body_failed}, [:before, :arg1, :body]},
                  {{:exception, :exit, :body_failed}, [:before, :arg1, :body]}
                ] |> Enum.flat_map(fn value -> [{:ok, value}, {:ok, value}, {:ok, value}] end)
                true = actual === expected
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_function_identity_preserves_exact_keys_across_copy_and_gc(self):
        kinds = ["named_zero", "named_one", "named_two", "captured_named", "numeric_capture"]
        expressions = [f"differential:function_identity({kind},{padding})"
                       for kind in kinds for padding in [0, 29, 30, 31]]
        by_expr = {c["expression"]: c for c in lean_runtime.cases()}
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            source.write_text(json.dumps([by_expr[e] for e in expressions]))
            output = work / "identity"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 20, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                actual = JSON.decode!(File.read!(path))["expected"]
                         |> Enum.map(&:erlang.binary_to_term(Base.decode64!(&1)))
                expected = for kind <- [:named_zero, :named_one, :named_two,
                                        :captured_named, :numeric_capture], padding <- [0, 29, 30, 31] do
                  numeric = kind == :numeric_capture
                  types = if kind in [:captured_named, :numeric_capture],
                    do: [type: :local, type: :local, type: :local],
                    else: [type: :local, type: :external, type: :external]
                  # Exact membership/keys differ even when numeric captures are ==.
                  found_a = if numeric, do: :external, else: :local
                  found_c = if numeric, do: :local, else: :external
                  {:ok, {:ok, {
                    {:equality, {numeric, not numeric, false, true, true, true}},
                    {:types, types},
                    {:calls, [true, true, true]},
                    {:lists, {false, true, found_a, found_c}},
                    {:maps, {padding + 2, padding + 2, {:left, :right},
                             {:left, :replaced, :replaced},
                             {padding + 1, true, false},
                             {:left, padding + 1, false, :replaced}}},
                    {:dictionary, {{:undefined, :undefined, :right},
                                   {:left, :replaced, :replaced, 2},
                                   :replaced, :left, :undefined}}
                  }}}
                end
                true = actual === expected
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_portable_hashing_observations_have_independent_oracles(self):
        all_cases = lean_runtime.cases()
        hashing_cases = [
            c for c in all_cases
            if any(o.startswith("Modules/Hashing.lean#") for o in c.get("lean_origins", []))
        ]
        self.assertEqual(len(hashing_cases), 112)
        self.assertTrue(all(not c.get("assert_true") for c in hashing_cases))
        origins = {o for c in hashing_cases for o in c["lean_origins"]}
        self.assertEqual(
            origins,
            {
                "Modules/Hashing.lean#guard:1",
                "Modules/Hashing.lean#guard:2",
                "Modules/Hashing.lean#guard:3",
                "Modules/Hashing.lean#guard:4",
                "Modules/Hashing.lean#guard:5",
                "Modules/Hashing.lean#shadowed_bindings_hash",
                "Modules/Hashing.lean#capture_hash",
            },
        )
        selected_expressions = [
            "differential:phash2_probe(0)",
            "differential:phash2_probe(-1)",
            "differential:phash2_probe(134217728)",
            "differential:phash2_probe(-1267650600228229401496703205369)",
            "differential:phash2_probe({float_bits,0,0,0})",
            "differential:phash2_probe({float_bits,1,0,0})",
            "differential:phash2_probe({float_bits,0,0,1})",
            "differential:phash2_probe(a)",
            "differential:phash2_probe({utf8_atom,<<99,97,102,195,169>>})",
            "differential:phash2_probe({utf8_atom,<<230,151,165,230,156,172,232,170,158>>})",
            "differential:phash2_probe(<<>>)",
            "differential:phash2_probe(<<97,98,99>>)",
            "differential:phash2_probe(<<1,2,3:4>>)",
            "differential:phash2_probe([])",
            "differential:phash2_probe([104,101,108,108,111])",
            "differential:phash2_probe([1,2|3])",
            "differential:phash2_probe({a,{b,c},[1,2]})",
            "differential:phash2_probe(#{1=>a,[x]=>{y}})",
            "differential:phash2_probe({map_seq_40})",
            "differential:phash2_ranges(0)",
            "differential:phash2_range(a,7)",
            "differential:phash2_range(a,0)",
            "differential:phash2_range(a,(1 bsl 32)+1)",
            "differential:phash2_map_relations(shadowed_single)",
            "differential:phash2_live_identities(closures_shadowed_map)",
            "differential:phash2_live_identities(pids_and_refs)",
        ]
        by_expr = {c["expression"]: c for c in hashing_cases}
        self.assertTrue(all(expr in by_expr for expr in selected_expressions))
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            source.write_text(json.dumps([by_expr[expr] for expr in selected_expressions]))
            output = work / "hashing"
            summary = self.invoke(output, "--replay", source)
            # Absent phash2/1,2 support stays an explicit target failure, never
            # allowlisted or marked unverified; remains valid once implemented.
            self.assertEqual(summary["passed"] + summary["failed"], 26)
            self.assertEqual(summary["unverified"], 0)
            check = subprocess.run([
                "elixir", "-e", """
                import Bitwise
                [path] = System.argv()
                values = JSON.decode!(File.read!(path))["expected"]
                         |> Enum.map(&:erlang.binary_to_term(Base.decode64!(&1)))
                probe = fn short, full ->
                  {:ok, {short, full, true, true, 0, rem(full, 7), true, rem(full, 1_000_003), true}}
                end
                full0 = 3_175_731_469
                ranges0 = Enum.map([1, 2, 7, 13, 255, 256, 65_537,
                                    (1 <<< 27) - 1, 1 <<< 27, (1 <<< 32) - 1, 1 <<< 32],
                                   &rem(full0, &1))
                expected = [
                  probe.(88_723_725, 3_175_731_469),
                  probe.(44_071_773, 1_117_813_597),
                  probe.(12_354_923, 2_562_491_755),
                  probe.(17_713_402, 3_373_156_602),
                  probe.(20_875_736, 423_528_920),
                  probe.(20_875_736, 423_528_920),
                  probe.(107_647_591, 3_731_526_247),
                  probe.(97, 97),
                  probe.(432_201, 432_201),
                  probe.(67_592_990, 67_592_990),
                  probe.(13_708_901, 147_926_629),
                  probe.(98_228_475, 1_306_188_027),
                  probe.(11_286_013, 2_292_987_389),
                  probe.(113_427_502, 3_468_870_702),
                  probe.(81_920_127, 753_008_767),
                  probe.(8_851_781, 4_169_601_349),
                  probe.(93_309_890, 1_569_704_898),
                  probe.(87_590_499, 2_637_727_331),
                  probe.(55_048_502, 2_202_532_150),
                  {:ok, {ranges0, true}},
                  {:ok, 6},
                  {:exception, :error, :badarg},
                  {:exception, :error, :badarg},
                  {:ok, {59_617_982, 1_401_795_262, true, true, true, true, true}},
                  {:ok, {true, true, true, true, true, true, true}},
                  {:ok, {true, true, true, true, true, true, true}}
                ]
                true = values === expected
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_await_many_and_many_sends_observations_have_independent_oracles(self):
        all_cases = lean_runtime.cases()
        upstream_await_many = [
            c for c in all_cases
            if c.get("upstream_example") == "examples/elixir/task.exs#await_many_drains_replies"
        ]
        self.assertEqual(len(upstream_await_many), 6)
        self.assertTrue(all("lean_origins" not in c and not c.get("assert_true")
                            for c in upstream_await_many))
        many_sends_30 = [
            c for c in all_cases
            if c["expression"].startswith("differential:many_sends(") and c["expression"].endswith(",30)")
        ]
        self.assertEqual(len(many_sends_30), 4)
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            source.write_text(json.dumps(upstream_await_many + many_sends_30))
            output = work / "await_many"
            summary = self.invoke(output, "--replay", source)
            self.assertEqual(summary, {"laws": 0, "passed": 10, "failed": 0,
                                       "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                values = JSON.decode!(File.read!(path))["expected"]
                         |> Enum.map(&:erlang.binary_to_term(Base.decode64!(&1)))
                post = {false, false, false, false}
                expected = [
                  {:ok, {:ok, {{:ok, [first: 10, second: 20]}, :normal, :normal, post, [:retained]}}},
                  {:ok, {:ok, {{:ok, [first: 10, second: 20]}, :normal, :normal, post, [:retained]}}},
                  {:ok, {:ok, {{:ok, [first: 10, second: 20]}, :normal, :normal, post, [:retained]}}},
                  {:ok, {:ok, {:timeout, :normal, :normal, post, [:retained]}}},
                  {:ok, {:ok, {:timeout, :normal, :normal, post, [:retained]}}},
                  {:ok, {:ok, {{:down, :first, :boom}, :boom, :normal, post, [:retained]}}},
                  {:ok, {:ok, {true, true, 0, [:retained]}}},
                  {:ok, {:ok, {true, true, 30, [:retained]}}},
                  {:ok, {:ok, {true, true, 30, [:retained]}}},
                  {:ok, {:ok, {true, true, 0, [:retained]}}}
                ]
                true = values === expected
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_lifecycle_sequences_preserve_fenced_state_and_allowed_races(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            case_a = "differential:lifecycle_sequence(" + ",".join([
                "{false,true,none,none}",
                "[" + ",".join([
                    "{register_slot,reg_a,alpha}",
                    "{register_slot,reg_b,alpha}",
                    "{register_slot,reg_a,beta}",
                    "{whereis_slot,reg_a}",
                    "{unregister_slot,reg_a}",
                    "{peer_link_action,alpha,unlink}",
                    "{link_slot,missing}",
                    "{trap_exit,true}",
                    "{link_slot,missing}",
                    "{fenced_exit_signal,custom_signal}",
                    "{trap_exit,false}",
                    "{drain_mailbox}",
                ]) + "]",
            ]) + ")"
            case_b = "differential:lifecycle_sequence(" + ",".join([
                "{true,false,reg_a,explicit_unalias}",
                "[" + ",".join([
                    "{alias_send,a1,alpha,hello_a1}",
                    "{monitor_slot,m1,reg_a,reply_demonitor}",
                    "{alias_send,m1,alpha,one_shot}",
                    "{demonitor_slot,m1,info}",
                    "{monitor_slot,m2,beta,demonitor}",
                    "{monitor_slot,m1,alpha,bad_opt}",
                    "{kill_child,beta,shutdown}",
                    "{monitor_dead_queued,m1,explicit_unalias}",
                    "{demonitor_slot,m1,flush_info}",
                    "{alias_unalias,m1}",
                    "{spawn_slot,beta}",
                    "{link_slot,alpha}",
                    "{trap_exit,false}",
                    "{kill_child,alpha,boom}",
                    "{race_unlink_exit,beta,boom}",
                    "{race_demonitor_exit,explicit_unalias,flush,shutdown}",
                    "{drain_mailbox}",
                ]) + "]",
            ]) + ")"
            source.write_text(json.dumps([{"expression": case_a}, {"expression": case_b}]))
            output = work / "results"
            self.assertEqual(self.invoke(output, "--replay", source),
                             {"laws": 0, "passed": 2, "failed": 0,
                              "unverified": 0, "uncovered": 0})
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                [a, b] = JSON.decode!(File.read!(path))["expected"]
                         |> Enum.map(&:erlang.binary_to_term(Base.decode64!(&1)))
                s_a0 = {false, [:alpha], {:undefined, :undefined}, {{:alive, true}, {:alive, true}}, {:none, :none}, {:none, :none}}
                s_a1 = {false, [:alpha], {:alpha, :undefined}, {{:alive, true}, {:alive, true}}, {:none, :none}, {:none, :none}}
                s_a5 = {false, [:alpha], {:undefined, :undefined}, {{:alive, true}, {:alive, true}}, {:none, :none}, {:none, :none}}
                s_a6 = {false, [], {:undefined, :undefined}, {{:alive, true}, {:alive, true}}, {:none, :none}, {:none, :none}}
                s_a8 = {true, [], {:undefined, :undefined}, {{:alive, true}, {:alive, true}}, {:none, :none}, {:none, :none}}
                {:ok, {:ok, {^s_a0, [
                  {{:register_slot, :reg_a, :alpha, {:ok, true}, :alpha}, ^s_a1},
                  {{:register_slot, :reg_b, :alpha, {:exception, :error, :badarg}, :undefined}, ^s_a1},
                  {{:register_slot, :reg_a, :beta, {:exception, :error, :badarg}, :alpha}, ^s_a1},
                  {{:whereis_slot, :reg_a, :alpha}, ^s_a1},
                  {{:unregister_slot, :reg_a, {:ok, true}, :undefined}, ^s_a5},
                  {{:peer_link_action, :alpha, :unlink, true, false, false, false}, ^s_a6},
                  {{:link_slot, :missing, {:exception, :error, :noproc}}, ^s_a6},
                  {{:trap_exit, true, {:ok, false}, true}, ^s_a8},
                  {{:link_slot, :missing, {:ok, :noproc}}, ^s_a8},
                  {{:fenced_exit_signal, :custom_signal, :queued}, ^s_a8},
                  {{:trap_exit, false, {:ok, true}, false}, ^s_a6},
                  {{:drain_mailbox, [{:exit_msg, {:helper, 2}, :custom_signal}]}, ^s_a6}
                ]}}} = a
                s_b0 = {true, [], {:alpha, :undefined}, {{:alive, true}, {:alive, true}}, {:none, :none}, {{:explicit_unalias, true}, :none}}
                s_b2 = {true, [], {:alpha, :undefined}, {{:alive, true}, {:alive, true}}, {{:reply_demonitor, true, true}, :none}, {{:explicit_unalias, true}, :none}}
                s_b3 = {true, [], {:alpha, :undefined}, {{:alive, true}, {:alive, true}}, {{:reply_demonitor, false, false}, :none}, {{:explicit_unalias, true}, :none}}
                s_b5 = {true, [], {:alpha, :undefined}, {{:alive, true}, {:alive, true}}, {{:reply_demonitor, false, false}, {:demonitor, true, true}}, {{:explicit_unalias, true}, :none}}
                s_b6 = {true, [], {:alpha, :undefined}, {{:alive, true}, {:alive, true}}, {:none, {:demonitor, true, true}}, {{:explicit_unalias, true}, :none}}
                s_b7 = {true, [], {:alpha, :undefined}, {{:alive, true}, {:dead, false}}, {:none, {:demonitor, false, false}}, {{:explicit_unalias, true}, :none}}
                s_b8 = {true, [], {:alpha, :undefined}, {{:alive, true}, {:dead, false}}, {{:explicit_unalias, false, true}, {:demonitor, false, false}}, {{:explicit_unalias, true}, :none}}
                s_b10 = {true, [], {:alpha, :undefined}, {{:alive, true}, {:dead, false}}, {{:explicit_unalias, false, false}, {:demonitor, false, false}}, {{:explicit_unalias, true}, :none}}
                s_b11 = {true, [], {:alpha, :undefined}, {{:alive, true}, {:alive, true}}, {{:explicit_unalias, false, false}, {:demonitor, false, false}}, {{:explicit_unalias, true}, :none}}
                s_b12 = {true, [:alpha], {:alpha, :undefined}, {{:alive, true}, {:alive, true}}, {{:explicit_unalias, false, false}, {:demonitor, false, false}}, {{:explicit_unalias, true}, :none}}
                s_b13 = {false, [:alpha], {:alpha, :undefined}, {{:alive, true}, {:alive, true}}, {{:explicit_unalias, false, false}, {:demonitor, false, false}}, {{:explicit_unalias, true}, :none}}
                s_b14 = {false, [], {:undefined, :undefined}, {{:dead, false}, {:alive, true}}, {{:explicit_unalias, false, false}, {:demonitor, false, false}}, {{:explicit_unalias, true}, :none}}
                s_b15 = {false, [], {:undefined, :undefined}, {{:dead, false}, {:dead, false}}, {{:explicit_unalias, false, false}, {:demonitor, false, false}}, {{:explicit_unalias, true}, :none}}
                {:ok, {:ok, {^s_b0, [
                  {{:alias_send, :a1, :alpha, {:delivered, :hello_a1}}, ^s_b0},
                  {{:monitor_slot, :m1, :reg_a, :reply_demonitor, :ok}, ^s_b2},
                  {{:alias_send, :m1, :alpha, {:delivered, :one_shot}}, ^s_b3},
                  {{:demonitor_slot, :m1, :info, {:ok, false}}, ^s_b3},
                  {{:monitor_slot, :m2, :beta, :demonitor, :ok}, ^s_b5},
                  {{:monitor_slot, :m1, :alpha, :bad_opt, {:exception, :error, :badarg}}, ^s_b6},
                  {{:kill_child, :beta, :shutdown, {:ok, :shutdown, :none, [{:m2, {true, :shutdown}}]}}, ^s_b7},
                  {{:monitor_dead_queued, :m1, :explicit_unalias, :queued}, ^s_b8},
                  {{:demonitor_slot, :m1, :flush_info, {:ok, false}}, ^s_b8},
                  {{:alias_unalias, :m1, {:ok, true}}, ^s_b10},
                  {{:spawn_slot, :beta, :spawned, []}, ^s_b11},
                  {{:link_slot, :alpha, {:ok, true}, true, true, true}, ^s_b12},
                  {{:trap_exit, false, {:ok, true}, false}, ^s_b13},
                  {{:kill_child, :alpha, :boom, {:ok, :boom, {:observer_linked_exit, :boom}, []}}, ^s_b14},
                  {{:race_unlink_exit, :beta, :boom, true}, ^s_b15},
                  {{:race_demonitor_exit, :explicit_unalias, :flush, :shutdown, true}, ^s_b15},
                  {{:drain_mailbox, []}, ^s_b15}
                ]}}} = b
                """, str(output / "manifest.json")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    @unittest.skipUnless(os.getenv("LYNX_CHECKOUT"), "Set LYNX_CHECKOUT to test fixture coverage")
    def test_lynx_exports_have_cases_and_partial_replay_is_scoped(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            full = work / "full"
            summary = self.invoke(full, "--lynx", os.environ["LYNX_CHECKOUT"],
                                  "--generated", 0, "--timeout", 1)
            self.assertGreater(summary["laws"], 0)
            self.assertEqual(summary["uncovered"], 0)
            self.assertFalse((full / "lists.beam").exists())
            coverage = json.loads((full / "coverage.json").read_text())
            self.assertGreater(len(coverage), 100)
            self.assertTrue(all(row["cases"] > 0 for row in coverage))
            selected = json.loads((full / "cases.json").read_text())
            law_samples = [c for c in selected if c.get("assert_true")]
            self.assertEqual(len(law_samples), 19)
            self.assertEqual(sum(c["fixture"] == ["sum", "sum_singleton_ensures", 1]
                                 for c in law_samples), 5)
            self.assertEqual(sum(c["fixture"] == ["sum", "sum_empty_ensures", 0]
                                 for c in law_samples), 1)
            self.assertEqual(sum(c["fixture"][0] == "protocols" for c in law_samples), 13)
            manifest = json.loads((full / "manifest.json").read_text())
            self.assertTrue(manifest["support"])
            self.assertTrue(all(name.startswith("Elixir.Lynx.Support.")
                                for name in manifest["support"]))
            self.assertTrue(all((full / f"{name}.beam").is_file()
                                for name in manifest["support"]))
            protocol_calls = [c for c in selected if c["fixture"][0] == "protocols"]
            self.assertEqual(len(protocol_calls), 78)
            failures = json.loads((full / "failures.json").read_text())
            self.assertFalse(any(f["case"].get("assert_true") for f in failures))
            # A partial replay must not claim full fixture coverage merely
            # because its one selected call matched.
            source = work / "one.json"
            source.write_text(json.dumps([{"fixture": ["literal", "integers", 0],
                                          "expression": "literal:integers()"}]))
            partial = work / "partial"
            subset = self.invoke(partial, "--lynx", os.environ["LYNX_CHECKOUT"], "--replay", source)
            self.assertEqual(subset["passed"], 1)
            self.assertEqual(subset["failed"], 0)
            self.assertEqual(subset["uncovered"], len(coverage) - 1)
            self.assertEqual(subset["unverified"], 0)

    def test_wrong_target_result_is_saved_shrunk_and_replayable(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            build = work / "build"
            (build / "src").mkdir(parents=True)
            (build / "tools/packbeam").mkdir(parents=True)
            (build / "libs").mkdir()
            (build / "CMakeCache.txt").write_text("AVM_DISABLE_JIT:BOOL=ON\nAVM_ENABLE_PRECOMPILED:BOOL=OFF\n")
            (build / "libs/atomvmlib.avm").touch()
            (build / "tools/packbeam/packbeam").symlink_to(ROOT / "build/tools/packbeam/packbeam")
            # A successful process emitting the wrong serialized term must still fail.
            runtime = build / "src/AtomVM"
            runtime.write_text("#!/bin/sh\necho 'LYNX_RESULT [131,100,0,4,110,111,112,101]'\n")
            runtime.chmod(0o755)
            source = work / "input.json"
            source.write_text(json.dumps([{"operation": "compare", "args": [100, -7]}]))
            output = work / "failure"
            summary = self.invoke(output, "--build", build, "--replay", source, "--shrink")
            self.assertEqual(summary, {"laws": 0, "passed": 0, "failed": 1,
                                       "unverified": 0, "uncovered": 0})
            failures = json.loads((output / "failures.json").read_text())
            self.assertEqual(failures[0]["case"]["args"], [100, -7])
            reduced = json.loads((output / "minimized.json").read_text())
            self.assertEqual(reduced[0]["args"], [0, 0])
            self.assertEqual(
                self.invoke(work / "replayed", "--build", build, "--replay", output / "minimized.json"),
                summary,
            )
            setup = [{"tuple": ["sentinel", 17]}, {"tuple": [1.0, "second"]}]
            steps = [{"tuple": s} for s in [
                ["put", 1, "first"], ["put", 1, "replaced"],
                ["erase", 1], ["get", 1.0], ["put", 1, "reinserted"],
            ]]
            for operation in ["map_sequence", "dictionary_sequence", "receive_sequence", "binary_sequence", "lifecycle_sequence", "queue_sequence"]:
                if operation == "receive_sequence":
                    setup = ["sentinel", 1.0, {"tuple": ["tag", 1]}]
                    steps = [{"tuple": s} for s in [
                        ["exact", 1], ["send", 1], ["tag", "tag"], ["exact", 1], ["any"],
                    ]]
                elif operation == "binary_sequence":
                    setup = list(b"zababcab")
                    steps = [{"tuple": s} for s in [
                        ["part", 1, 5], ["part", 5, -3], ["append", [97, 98]],
                        ["match", {"bits": [0x6162, 16]}, []],
                    ]]
                elif operation == "lifecycle_sequence":
                    setup = {"tuple": ["false", "true", "none", "none"]}
                    steps = [{"tuple": s} for s in [
                        ["register_slot", "reg_a", "alpha"],
                        ["whereis_slot", "reg_a"],
                        ["peer_link_action", "alpha", "unlink"],
                        ["kill_child", "alpha", "shutdown"],
                    ]]
                elif operation == "queue_sequence":
                    setup = [1, 1.0, "sentinel"]
                    steps = [{"tuple": s} for s in [
                        ["save"], ["out"], ["in_r", 1], ["split", 2],
                        ["join", "right", [1.0, "tail"]], ["restore"],
                    ]]
                source.write_text(json.dumps([
                    {"seed": 123, "index": 7, "operation": operation, "args": [setup, steps]}
                ]))
                output = work / operation
                self.assertEqual(self.invoke(output, "--build", build, "--replay", source, "--shrink"),
                                 summary)
                reduced = json.loads((output / "minimized.json").read_text())[0]
                self.assertEqual(reduced["args"][0], setup)
                self.assertEqual(len(reduced["args"][1]), 1)
                self.assertIn(reduced["args"][1][0], steps)
                self.assertEqual((reduced["seed"], reduced["index"]), (123, 7))
                self.assertEqual(self.invoke(work / (operation + "-replay"), "--build", build,
                                             "--replay", output / "minimized.json"), summary)
            runtime.write_text("#!/bin/sh\necho 'LYNX_RESULT [131]'\n")
            malformed = work / "malformed"
            self.assertEqual(self.invoke(malformed, "--build", build, "--replay", source), summary)
            failure = json.loads((malformed / "failures.json").read_text())[0]
            self.assertEqual(failure["actual"]["status"], "error")
            self.assertIn("Invalid serialized result", failure["actual"]["message"])
            sys.path.insert(0, str(Path(__file__).resolve().parent))
            import base64
            import validate
            ok_encoded = base64.b64encode(bytes([131, 104, 2, 119, 2, *b"ok", 97, 1])).decode("ascii")
            exc_encoded = base64.b64encode(bytes([131, 104, 3, 119, 9, *b"exception", 119, 5, *b"error", 119, 6, *b"badarg"])).decode("ascii")
            orig = {"case": {"operation": "lifecycle_sequence"}, "expected": ok_encoded, "actual": {"status": "ok", "value": ok_encoded}}
            self.assertTrue(validate.same_failure_contract(orig, dict(orig)))
            self.assertFalse(validate.same_failure_contract(orig, {**orig, "expected": exc_encoded}))
            self.assertFalse(validate.same_failure_contract(orig, {**orig, "case": {"operation": "compare"}}))

    @unittest.skipUnless(os.getenv("ATOMVM_JIT_BUILD"), "Set ATOMVM_JIT_BUILD to test on-demand JIT compilation")
    def test_jit_build_compiles_bytecode_probes_on_demand(self):
        jit_build = Path(os.environ["ATOMVM_JIT_BUILD"]).resolve()
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "input.json"
            source.write_text(json.dumps([
                {"expression": "1 + 2"},
                {"expression": "differential:execution_paths('<',9007199254740993,9007199254740992.0)"},
                {"expression": "lists:keyfind(b,2,[{a,1},{prefix,b,ok}])"},
            ]))
            output = work / "jit"
            result = subprocess.run(
                [sys.executable, str(SCRIPT), "--build", str(jit_build),
                 "--replay", str(source), "--timeout", "120", "--output", str(output)],
                capture_output=True, text=True, timeout=600,
            )
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            summary = json.loads((output / "summary.json").read_text())
            self.assertEqual(summary, {"laws": 0, "passed": 3, "failed": 0,
                                       "unverified": 0, "uncovered": 0})
            run = json.loads((output / "run.json").read_text())
            self.assertEqual(run["execution_mode"], "on_demand_jit")
            self.assertFalse(run["target"]["avm_disable_jit"])
            self.assertFalse(run["target"]["avm_enable_precompiled"])
            self.assertEqual(len(run["runtime_sha256"]), 64)
            self.assertEqual(len(run["library_sha256"]), 64)
            self.assertFalse((output / "native").exists())
            check = subprocess.run([
                "elixir", "-e", """
                [beam] = System.argv()
                {:ok, _, chunks} = :beam_lib.all_chunks(String.to_charlist(beam))
                names = Enum.map(chunks, fn {name, _} -> List.to_string(name) end)
                true = "Code" in names
                false = Enum.any?(names, &String.starts_with?(&1, "avm"))
                """, str(output / "lynx_probe_0.beam")
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)
            self.assertIn("Compilation of ", (output / "lynx_probe_0.log").read_text())

    @unittest.skipUnless(
        os.getenv("ATOMVM_JIT_PRECOMPILE_BUILD"),
        "Set ATOMVM_JIT_PRECOMPILE_BUILD to test jit_precompile across all targets",
    )
    def test_jit_precompile_all_targets(self):
        jit_build = Path(os.environ["ATOMVM_JIT_PRECOMPILE_BUILD"]).resolve()
        jit_beams = jit_build / "libs" / "jit" / "src" / "beams"
        self.assertTrue(jit_beams.is_dir(), f"Missing JIT beam directory: {jit_beams}")
        targets = [
            "x86_64", "x86_64+float32",
            "aarch64", "aarch64+float32",
            "arm32", "arm32+float32",
            "armv6m", "armv6m+float32",
            "armv6m+thumb2", "armv6m+thumb2+float32",
            "riscv32", "riscv32+float32",
            "riscv64", "riscv64+float32",
            "wasm32", "wasm32+float32",
            "xtensa", "xtensa+float32",
        ]
        diff_erl = SCRIPT.parent / "differential.erl"
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            compile_beam = subprocess.run(
                ["erlc", "-o", str(work), str(diff_erl)],
                capture_output=True, text=True, timeout=60,
            )
            self.assertEqual(compile_beam.returncode, 0, compile_beam.stdout + compile_beam.stderr)
            diff_beam = work / "differential.beam"
            targets_erl = "[" + ",".join(f'"{t}"' for t in targets) + "]"
            script = (
                f'Work = "{work}", Beam = "{diff_beam}", '
                f"lists:foreach(fun(T) -> "
                f'Dir = Work ++ "/" ++ T, '
                f'ok = filelib:ensure_dir(Dir ++ "/x"), '
                f"ok = jit_precompile:compile(T, Dir, false, Beam), "
                f'Out = Dir ++ "/differential.beam", '
                f"{{ok, Bin}} = file:read_file(Out), "
                f"{{ok, differential, Chunks}} = beam_lib:all_chunks(Bin), "
                f'{{"avmN", Native}} = lists:keyfind("avmN", 1, Chunks), '
                f"true = byte_size(Native) > 0 "
                f"end, {targets_erl}), init:stop()."
            )
            res = subprocess.run(
                ["erl", "-pa", str(jit_beams), "-noshell", "-eval", script],
                capture_output=True, text=True, timeout=120,
            )
            self.assertEqual(res.returncode, 0, res.stdout + res.stderr)

    def test_timeout_and_failure_stages_distinguish_oracle_semantic_and_transport(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            # 1. Oracle timeout (OTP times out before AtomVM executes).
            oracle_src = work / "oracle.json"
            oracle_src.write_text(json.dumps([{"expression": "timer:sleep(1500), ok"}]))
            oracle_out = work / "oracle"
            summary = self.invoke(oracle_out, "--replay", oracle_src, "--timeout", "0.2")
            self.assertEqual(summary, {"laws": 0, "passed": 0, "failed": 0,
                                       "unverified": 1, "uncovered": 0})
            unverified = json.loads((oracle_out / "unverified.json").read_text())
            self.assertEqual(unverified[0]["stage"], "oracle")
            self.assertTrue(unverified[0]["timeout"])

            # Fake build to test semantic/transport timeout and error classification deterministically.
            build = work / "build"
            (build / "src").mkdir(parents=True)
            (build / "tools/packbeam").mkdir(parents=True)
            (build / "libs").mkdir()
            (build / "CMakeCache.txt").write_text("AVM_DISABLE_JIT:BOOL=ON\nAVM_ENABLE_PRECOMPILED:BOOL=OFF\n")
            (build / "libs/atomvmlib.avm").touch()
            (build / "tools/packbeam/packbeam").symlink_to(ROOT / "build/tools/packbeam/packbeam")
            runtime = build / "src/AtomVM"
            source = work / "expr.json"
            source.write_text(json.dumps([{"expression": "1 + 2"}]))

            # 2. Semantic timeout (no LYNX_SEMANTIC_DONE emitted before deadline).
            runtime.write_text("#!/bin/sh\nexec sleep 2\n")
            runtime.chmod(0o755)
            sem_to = work / "sem-timeout"
            result = subprocess.run(
                [sys.executable, str(SCRIPT), "--build", str(build),
                 "--replay", str(source), "--timeout", "0.2", "--output", str(sem_to)],
                capture_output=True, text=True, timeout=60,
            )
            self.assertNotEqual(result.returncode, 0)
            failures = json.loads((sem_to / "failures.json").read_text())
            self.assertEqual(failures[0]["stage"], "semantic")
            self.assertTrue(failures[0]["timeout"])

            # 3. Transport timeout (LYNX_SEMANTIC_DONE emitted before deadline).
            runtime.write_text("#!/bin/sh\necho 'LYNX_SEMANTIC_DONE'\nexec sleep 2\n")
            runtime.chmod(0o755)
            trans_to = work / "trans-timeout"
            result = subprocess.run(
                [sys.executable, str(SCRIPT), "--build", str(build),
                 "--replay", str(source), "--timeout", "0.2", "--output", str(trans_to)],
                capture_output=True, text=True, timeout=60,
            )
            self.assertNotEqual(result.returncode, 0)
            failures = json.loads((trans_to / "failures.json").read_text())
            self.assertEqual(failures[0]["stage"], "transport")
            self.assertTrue(failures[0]["timeout"])

            # 4. Semantic mismatch (cleanly decoded wrong value).
            runtime.write_text("#!/bin/sh\necho 'LYNX_SEMANTIC_DONE'\necho 'LYNX_RESULT [131,100,0,4,110,111,112,101]'\n")
            runtime.chmod(0o755)
            sem_val = work / "sem-value"
            result = subprocess.run(
                [sys.executable, str(SCRIPT), "--build", str(build),
                 "--replay", str(source), "--output", str(sem_val)],
                capture_output=True, text=True, timeout=60,
            )
            self.assertNotEqual(result.returncode, 0)
            failures = json.loads((sem_val / "failures.json").read_text())
            self.assertEqual(failures[0]["stage"], "semantic")
            self.assertFalse(failures[0]["timeout"])

            # 5. Transport malformed ETF.
            runtime.write_text("#!/bin/sh\necho 'LYNX_SEMANTIC_DONE'\necho 'LYNX_RESULT [131]'\n")
            runtime.chmod(0o755)
            trans_bad = work / "trans-bad"
            result = subprocess.run(
                [sys.executable, str(SCRIPT), "--build", str(build),
                 "--replay", str(source), "--output", str(trans_bad)],
                capture_output=True, text=True, timeout=60,
            )
            self.assertNotEqual(result.returncode, 0)
            failures = json.loads((trans_bad / "failures.json").read_text())
            self.assertEqual(failures[0]["stage"], "transport")
            self.assertFalse(failures[0]["timeout"])

    def test_word_size_and_float32_expectations_are_representation_aware(self):
        word_cases = [c for c in lean_runtime.cases() if c.get("word_size_dependent")]
        self.assertEqual(len(word_cases), 24)
        stack_cases = [c for c in lean_runtime.cases() if c.get("stacktrace_dependent")]
        self.assertEqual(len(stack_cases), 172)
        self.assertGreaterEqual(len(lean_runtime.float32_cases()), 8)

        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            build32 = work / "build32"
            (build32 / "src").mkdir(parents=True)
            (build32 / "tools/packbeam").mkdir(parents=True)
            (build32 / "libs").mkdir()
            (build32 / "CMakeCache.txt").write_text(
                "AVM_DISABLE_JIT:BOOL=ON\n"
                "AVM_ENABLE_PRECOMPILED:BOOL=OFF\n"
                "AVM_USE_32BIT_FLOAT:BOOL=ON\n"
                "AVM_CREATE_STACKTRACES:BOOL=OFF\n"
                "CMAKE_SIZEOF_VOID_P:INTERNAL=4\n"
            )
            (build32 / "libs/atomvmlib.avm").touch()
            (build32 / "tools/packbeam/packbeam").symlink_to(ROOT / "build/tools/packbeam/packbeam")
            runtime = build32 / "src/AtomVM"

            # Unannotated float64 expectation, word-size-dependent keyfind, and
            # stacktrace-dependent probe on 32-bit/float32/no-stacktraces -> oracle unverified.
            import base64
            import struct
            f32_16777216 = bytes([131, 104, 2, 119, 2, *b"ok", 70]) + struct.pack(">d", 16777216.0)
            runtime.write_text(f"#!/bin/sh\necho 'LYNX_SEMANTIC_DONE'\necho 'LYNX_RESULT {list(f32_16777216)}'\n")
            runtime.chmod(0o755)
            mixed = work / "mixed.json"
            mixed.write_text(json.dumps([
                {"expression": "differential:call(erlang,'+',[16777216.0,1.0])"},
                word_cases[0],
                stack_cases[0],
                {"expression": "differential:call(erlang,'+',[16777216.0,1.0])",
                 "float32_expected": base64.b64encode(f32_16777216).decode("ascii")},
            ]))
            out = work / "out32"
            result = subprocess.run(
                [sys.executable, str(SCRIPT), "--build", str(build32),
                 "--replay", str(mixed), "--output", str(out)],
                capture_output=True, text=True, timeout=60,
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertIs(json.loads((out / "run.json").read_text())["target"]["avm_create_stacktraces"], False)
            summary = json.loads((out / "summary.json").read_text())
            self.assertEqual(summary, {"laws": 0, "passed": 1, "failed": 0,
                                       "unverified": 3, "uncovered": 0})
            unverified = json.loads((out / "unverified.json").read_text())
            self.assertTrue(all(item["stage"] == "oracle" and not item["timeout"] for item in unverified))
            self.assertTrue(any("AVM_CREATE_STACKTRACES=OFF" in item["message"] for item in unverified))

            # Annotated float32 expectation passes when AtomVM returns binary32 result.
            f32_only = work / "f32.json"
            f32_only.write_text(json.dumps([
                {"expression": "differential:call(erlang,'+',[16777216.0,1.0])",
                 "float32_expected": base64.b64encode(f32_16777216).decode("ascii")},
            ]))
            f32_out = work / "out-f32"
            result = subprocess.run(
                [sys.executable, str(SCRIPT), "--build", str(build32),
                 "--replay", str(f32_only), "--output", str(f32_out)],
                capture_output=True, text=True, timeout=60,
            )
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertEqual(json.loads((f32_out / "summary.json").read_text())["passed"], 1)

            # Emscripten target metadata resolves --host-build PackBEAM/library and records wasm hash.
            wasm_build = work / "wasm"
            (wasm_build / "src").mkdir(parents=True)
            (wasm_build / "CMakeCache.txt").write_text(
                "CMAKE_SYSTEM_NAME:STRING=Emscripten\n"
                "AVM_DISABLE_JIT:BOOL=ON\n"
                "AVM_ENABLE_PRECOMPILED:BOOL=OFF\n"
                "AVM_USE_32BIT_FLOAT:BOOL=OFF\n"
            )
            (wasm_build / "src/AtomVM.wasm").write_bytes(b"wasm-bytes")
            (wasm_build / "src/AtomVM.mjs").write_text("// fake module")
            import validate as vmod
            target = vmod.resolve_target(wasm_build, ROOT / "build")
            self.assertEqual(target["platform"], "emscripten")
            self.assertEqual(target["arch"], "wasm32")
            self.assertEqual(target["word_size"], 4)
            self.assertIs(target["stacktraces"], True)
            self.assertEqual(target["execution_mode"], "interpreter")
            self.assertEqual(target["packbeam"], (ROOT / "build/tools/packbeam/packbeam").resolve())

    def test_expected_failures_manifest_and_strict_checking(self):
        import validate as vmod
        manifest_path = Path(__file__).with_name("expected_failures.json")
        areas = vmod.load_expected_failures(manifest_path)
        all_entries = [{**c, "module": f"m_{i}"} for i, c in enumerate(lean_runtime.cases())]
        _, _, area_counts, total_expected = vmod.classify_expected_failures(all_entries, [], areas)
        self.assertEqual(
            area_counts,
            {
                "phash2": 112,
                "float_text_short": 4,
                "lists_reverse_invalid": 2,
                "bitstring_size_system_limit": 3,
                "binary_encode_unsigned": 13,
                "fun_info_mfa": 13,
                "base64_invalid_error_reason": 15,
                "base64_iolist_whitespace_and_byte_range": 4,
                "calendar_date_range_guards": 6,
                "calendar_negative_subsecond_system_time": 4,
                "calendar_unimplemented_functions": 8,
                "array_module_unimplemented": 3,
                "gb_trees_module_unimplemented": 3,
                "rand_module_unimplemented": 3,
            },
        )
        self.assertEqual(total_expected, 193)
        all_exprs = {c["expression"] for c in all_entries}
        for area in areas:
            for expr in area["expressions"]:
                self.assertIn(expr, all_exprs)

        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            # 1. Sampled expected failures across unimplemented/known areas + passing
            # neighbors exit 0, while still recording true failures in summary.json and failures.json.
            sample = work / "sample.json"
            sample.write_text(json.dumps([
                {"expression": "1 + 2"},
                {"expression": "differential:phash2_probe(0)"},
                {"expression": "differential:float_text(100.0,[short])"},
                {"expression": "differential:float_text(12300.0,[short])"},
                {"expression": "differential:call(lists,'reverse',[[a,b]])"},
                {"expression": "differential:call(lists,'reverse',[bad])"},
                {"expression": "differential:call(binary,'encode_unsigned',[(1 bsl 64)+5])"},
                {"expression": "differential:fun_info_mfa(local_capture)"},
            ]))
            ok_out = work / "ok"
            result = subprocess.run(
                [sys.executable, str(SCRIPT), "--build", str(ROOT / "build"),
                 "--replay", str(sample), "--expected-failures", str(manifest_path),
                 "--output", str(ok_out)],
                capture_output=True, text=True, timeout=120,
            )
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertEqual(
                json.loads((ok_out / "summary.json").read_text()),
                {"laws": 0, "passed": 3, "failed": 5, "unverified": 0, "uncovered": 0},
            )
            self.assertEqual(len(json.loads((ok_out / "failures.json").read_text())), 5)

            # 2. Any unexpected failure still causes a nonzero exit.
            fake_build = work / "fake_build"
            (fake_build / "src").mkdir(parents=True)
            (fake_build / "tools/packbeam").mkdir(parents=True)
            (fake_build / "libs").mkdir()
            (fake_build / "CMakeCache.txt").write_text("AVM_DISABLE_JIT:BOOL=ON\nAVM_ENABLE_PRECOMPILED:BOOL=OFF\n")
            (fake_build / "libs/atomvmlib.avm").touch()
            (fake_build / "tools/packbeam/packbeam").symlink_to(ROOT / "build/tools/packbeam/packbeam")
            wrong_etf = bytes([131, 104, 2, 119, 2, *b"ok", 97, 99])
            fake_runtime = fake_build / "src/AtomVM"
            fake_runtime.write_text(f"#!/bin/sh\necho 'LYNX_SEMANTIC_DONE'\necho 'LYNX_RESULT {list(wrong_etf)}'\n")
            fake_runtime.chmod(0o755)
            unexpected_fail = work / "unexpected_fail.json"
            unexpected_fail.write_text(json.dumps([
                {"expression": "differential:phash2_probe(0)"},
                {"expression": "1 + 2"},
            ]))
            fail_out = work / "unexpected_fail"
            result = subprocess.run(
                [sys.executable, str(SCRIPT), "--build", str(fake_build),
                 "--replay", str(unexpected_fail), "--expected-failures", str(manifest_path),
                 "--output", str(fail_out)],
                capture_output=True, text=True, timeout=120,
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("1 unexpected differential failures", result.stderr)

            # 3. An expected failure that unexpectedly passes causes a nonzero exit.
            xfail_pass = work / "xfail_pass.json"
            xfail_pass.write_text(json.dumps({
                "areas": [{
                    "name": "unexpected_pass_check",
                    "reason": "test strict xfail",
                    "expressions": ["1 + 2"],
                }]
            }))
            pass_input = work / "pass_input.json"
            pass_input.write_text(json.dumps([{"expression": "1 + 2"}]))
            pass_out = work / "unexpected_pass"
            result = subprocess.run(
                [sys.executable, str(SCRIPT), "--build", str(ROOT / "build"),
                 "--replay", str(pass_input), "--expected-failures", str(xfail_pass),
                 "--output", str(pass_out)],
                capture_output=True, text=True, timeout=120,
            )
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("1 unexpected passes", result.stderr)

    def test_portable_only_and_parallel_jobs(self):
        import validate as vmod
        sample = [
            {"expression": "1 + 2"},
            {"expression": "lists:keyfind(1,1,[{1,a}])", "word_size_dependent": True},
            {"expression": "try error(boom) catch _:_:S -> S end", "stacktrace_dependent": True},
        ]
        kept_same, ow, os_cnt = vmod.filter_portable_cases(
            sample, {"word_size": 8, "stacktraces": True}, oracle_word_size=8
        )
        self.assertEqual((len(kept_same), ow, os_cnt), (3, 0, 0))
        kept_diff, ow, os_cnt = vmod.filter_portable_cases(
            sample, {"word_size": 4, "stacktraces": False}, oracle_word_size=8
        )
        self.assertEqual((kept_diff, ow, os_cnt), ([sample[0]], 1, 1))

        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            build32 = work / "build32"
            (build32 / "src").mkdir(parents=True)
            (build32 / "tools/packbeam").mkdir(parents=True)
            (build32 / "libs").mkdir()
            (build32 / "CMakeCache.txt").write_text(
                "AVM_DISABLE_JIT:BOOL=ON\n"
                "AVM_ENABLE_PRECOMPILED:BOOL=OFF\n"
                "AVM_CREATE_STACKTRACES:BOOL=OFF\n"
                "CMAKE_SIZEOF_VOID_P:INTERNAL=4\n"
            )
            (build32 / "libs/atomvmlib.avm").touch()
            (build32 / "tools/packbeam/packbeam").symlink_to(ROOT / "build/tools/packbeam/packbeam")
            ok_3 = bytes([131, 104, 2, 119, 2, *b"ok", 97, 3])
            runtime = build32 / "src/AtomVM"
            runtime.write_text(f"#!/bin/sh\necho 'LYNX_SEMANTIC_DONE'\necho 'LYNX_RESULT {list(ok_3)}'\n")
            runtime.chmod(0o755)
            mixed = work / "mixed.json"
            mixed.write_text(json.dumps(sample))
            out_portable = work / "out-portable"
            result = subprocess.run(
                [sys.executable, str(SCRIPT), "--build", str(build32),
                 "--replay", str(mixed), "--portable-only", "--output", str(out_portable)],
                capture_output=True, text=True, timeout=120,
            )
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertIn("Omitted 2 target-inapplicable cases (word_size=1, stacktraces=1).", result.stdout)
            self.assertEqual(
                json.loads((out_portable / "summary.json").read_text()),
                {"laws": 0, "passed": 1, "failed": 0, "unverified": 0, "uncovered": 0},
            )

            out_parallel = work / "out-parallel"
            result = subprocess.run(
                [sys.executable, str(SCRIPT), "--build", str(ROOT / "build"),
                 "--seed", "20261114", "--generated", "24", "--jobs", "4",
                 "--output", str(out_parallel)],
                capture_output=True, text=True, timeout=120,
            )
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertEqual(
                json.loads((out_parallel / "summary.json").read_text()),
                {"laws": 0, "passed": 24, "failed": 0, "unverified": 0, "uncovered": 0},
            )
            self.assertEqual(list(out_parallel.glob("*.avm")), [])
            self.assertEqual(list(out_parallel.glob("*.pack.log")), [])

    def test_bitstring_recipes_have_independent_oracles(self):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "bitstring_oracles.json"
            expressions = [
                "differential:bit_construct_binary(whole_binary_unit16,<<1,2,3,4>>,0)",
                "differential:bit_construct_binary(whole_binary_unit16,<<1,2,3>>,0)",
                "differential:bit_construct_binary(sized_bitstring,<<16#AB,5:3>>,5)",
                "differential:bit_construct_binary(concat_assoc,{<<5:3>>,<<16#AB:8>>,<<3:2>>},0)",
                "differential:bit_construct_int(unit3_unsigned_big,16#DEAD,5,3)",
                "differential:float_construct(big,{ieee64,1,0,0},64,3)",
                "differential:bit_match_int(unaligned_signed_little_rest,<<5:3,(-7):11/signed-little,1:1>>,11)",
                "differential:bit_match_int(size_from_prev_signed,<<15:4,1,2>>,0)",
                "differential:bit_match_binary(unaligned_binary2_rest,<<5:3,16#AB,16#CD,3:2>>,0)",
                "differential:bit_match_binary(unaligned_tail_binary,<<5:3,16#AB,1:1>>,0)",
                "differential:bit_match_float(big,<<128,0,0,0,0,0,0,0>>,64)",
                "differential:bit_match_float(big,<<124,0>>,16)",
                "differential:bit_match_literal(full_partial_5_3,<<5:3>>)",
                "differential:bit_match_literal(full_partial_5_3,<<16#A0:4>>)",
                "differential:utf_match_term(utf8,not_bits)",
                "differential:utf_roundtrip(utf16le,16#10FFFF)",
            ]
            source.write_text(json.dumps([{"expression": e} for e in expressions]))
            output = work / "results"
            self.assertEqual(
                self.invoke(output, "--replay", source),
                {"laws": 0, "passed": len(expressions), "failed": 0,
                 "unverified": 0, "uncovered": 0},
            )
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                values = JSON.decode!(File.read!(path))["expected"]
                         |> Enum.map(&:erlang.binary_to_term(Base.decode64!(&1)))
                [
                  {:ok, {32, <<1, 2, 3, 4>>}},
                  {:exception, :error, :badarg},
                  {:ok, {5, <<21::size(5)>>}},
                  {:ok, {true, true, <<181, 15::size(5)>>}},
                  {:ok, {15, <<189, 45::size(7)>>, <<183, 171, 7::size(4)>>}},
                  {:ok, {64, <<128, 0, 0, 0, 0, 0, 0, 0>>, <<176, 0, 0, 0, 0, 0, 0, 0, 3>>}},
                  {:ok, {-7, <<1::size(1)>>}},
                  {:ok, :no},
                  {:ok, {<<0xAB, 0xCD>>, <<3::size(2)>>}},
                  {:ok, :no},
                  {:ok, {true, -0.0, <<128, 0, 0, 0, 0, 0, 0, 0>>, <<>>}},
                  {:ok, :no},
                  {:ok, :matched},
                  {:ok, :no},
                  {:ok, :no},
                  {:ok, {32, <<255, 219, 255, 223>>, {0x10FFFF, <<>>}}},
                ] = values
                """, str(output / "manifest.json"),
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_remaining_behavioral_probes_have_independent_oracles(self):
        self.assertEqual(len(lean_runtime.cases()), 8674)
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "behavioral_oracles.json"
            expressions = [
                "differential:short_circuit(or_else,lhs_throw,value)",
                "differential:short_circuit(or_else,lhs_error,throw)",
                "differential:short_circuit(strict_and,1,value)",
                "differential:short_circuit(strict_or,false,throw)",
                "differential:short_circuit_chain(left_mixed,{true,bad,throw})",
                "differential:short_circuit_chain(right_guard,5)",
                "differential:demonitor_oldest_message(tag_mismatch)",
                "differential:demonitor_oldest_message(forged_then_real_down)",
                "differential:killed_linked_root(exit_atom)",
                "differential:word_mask_check((1 bsl 64)+7,64)",
                "differential:bnot_involution(-(1 bsl 63))",
                "differential:integer_range_check(5,0,10)",
                "differential:integer_range_check(5,0.0,10)",
                "differential:local_node_check(local,0)",
                "differential:function_apply(local_one,#{capture=><<1,2,3>>},[a,b,c])",
                "differential:spawn_behavior(parent_child_sync)",
                "differential:spawn_behavior(invalid_not_fun)",
                "differential:spawn_behavior(invalid_arity1)",
                "differential:spawn_behavior(unknown_export)",
                "differential:nested_spawn_cleanup()",
                "differential:is_number_paths(1 bsl 64)",
                "differential:is_number_paths(true)",
                "differential:call(maps,'from_keys',[[1,1.0],tag])",
                "differential:call(maps,'from_keys',[[a|b],v])",
                "differential:error_pair(custom_reason,[1,<<\"a\">>])",
                "differential:is_reference_paths(live_ref)",
                "differential:is_reference_paths(live_pid)",
                "differential:process_probe({refs,state_and_effect})",
                "differential:receive_selective(clause_priority_mailbox)",
                "differential:receive_selective(receive_body_exception)",
                "differential:process_probe({child_timeout,2})",
                "differential:process_probe(infinity_alive)",
                "differential:process_probe(guard_child_self)",
                "differential:try_boundaries(success_body)",
                "differential:try_boundaries(handler_propagates)",
                "differential:process_probe(blocking_not_exception)",
                "differential:process_probe({handshake,delay_child})",
                "differential:process_probe(two_senders)",
                "differential:process_probe(terminated_child)",
                "differential:process_probe(terminated_root)",
                "differential:process_probe({child_isolated,error})",
            ]
            source.write_text(json.dumps([{"expression": e} for e in expressions]))
            output = work / "results"
            self.assertEqual(
                self.invoke(output, "--replay", source),
                {"laws": 0, "passed": len(expressions), "failed": 0,
                 "unverified": 0, "uncovered": 0},
            )
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                values = JSON.decode!(File.read!(path))["expected"]
                         |> Enum.map(&:erlang.binary_to_term(Base.decode64!(&1)))
                [
                  {:ok, {{:exception, :throw, :left_boom}, :seen, :undefined}},
                  {:ok, {{:exception, :error, :left_boom}, :seen, :undefined}},
                  {:ok, {{:exception, :error, :badarg}, :seen, :seen}},
                  {:ok, {{:exception, :throw, :boom}, :seen, :seen}},
                  {:ok, {{:exception, :error, {:badarg, :bad}}, [:left, :mid]}},
                  {:ok, :rejected},
                  {:ok, {:ok, {true, [{:DOWN, true, :process, :child_pid, :normal}]}}},
                  {:ok, {:ok, {true, [{:DOWN, true, :process, :child_pid, :normal}]}}},
                  {:ok, {:ok, {1, :boom, false}}},
                  {:ok, {7, 7, true}},
                  {:ok, {:ok, 9223372036854775807, -9223372036854775808, true, true, true}},
                  {:ok, {:matched, {:ok, true}, {:ok, true}}},
                  {:ok, {:rejected, {:exception, :error, :badarg}, {:exception, :error, :badarg}}},
                  {:ok, {true, true, true, true, {false, false, false}, {false, false, false}, {false, false, false}}},
                  {:ok, {:error, :badarity, true, true}},
                  {:ok, {:ok, {true, true, true, :normal, false}}},
                  {:ok, {:ok, {{:error, :badarg}, {:error, :badarg}, {:error, :badarg}}}},
                  {:ok, {:ok, {{:unexpected, true}, {:error, :badarg}, {:error, :badarg}}}},
                  {:ok, {:ok, {true, {:undef, :stack}, false, true, :exited_expected, false}}},
                  {:ok, {:ok, {{false, :normal}, :normal, false, false}}},
                  {:ok, {true, true, true, true}},
                  {:ok, {false, false, false, false}},
                  {:ok, from_keys_ok},
                  {:exception, :error, :badarg},
                  {:ok, {{:exception, :error, :custom_reason}, {:caught, :error, :custom_reason}}},
                  {:ok, {true, true, true, true}},
                  {:ok, {false, false, false, false}},
                  {:ok, {:ok, {true, true, true, true, %{a: 1}, [:queued]}}},
                  {:ok, {:ok, {{:atom_first, :first}, [val: 7]}}},
                  {:ok, {:ok, {:caught, [:keep]}}},
                  {:ok, {:ok, {{:timeout, [:noise]}, :normal}}},
                  {:ok, {:ok, {true, :killed}}},
                  {:ok, {:ok, {{true, [true]}, :normal}}},
                  {:ok, :outer_caught},
                  {:ok, :outer_caught},
                  {:ok, {:ok, {true, {:ok, 7}, :normal}}},
                  {:ok, {:ok, {:parent, {:child, [:child_mail]}, :normal, [:parent_mail]}}},
                  {:ok, {:ok, {[sender: :left, sender: :right], :normal, :normal, []}}},
                  {:ok, {:ok, {:normal, false, {:ignored, <<1::size(3)>>}, false, []}}},
                  {:ok, {:ok, {:normal, {:normal, true, false}, :normal, false, []}}},
                  {:ok, {:ok, {{:failure, :stack}, false, :kept, [:parent_mail]}}},
                ] = values
                true = (from_keys_ok === %{1 => :tag, 1.0 => :tag})
                """, str(output / "manifest.json"),
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)

    def test_upstream_stdlib_example_observations_have_independent_oracles(self):
        all_cases = lean_runtime.cases()
        expected_groups = {
            "examples/otp/base64.exs#encoding_roundtrip": 50,
            "examples/otp/binary.exs#hex_roundtrip": 16,
            "examples/otp/calendar.exs#date_to_gregorian_days_cycle": 28,
            "examples/otp/calendar.exs#seconds_to_daystime_roundtrips": 2,
            "examples/otp/calendar.exs#seconds_to_time_roundtrips": 1,
            "examples/otp/calendar.exs#is_leap_year_cycle": 2,
            "examples/otp/calendar.exs#gregorian_days_roundtrip": 2,
            "examples/otp/calendar.exs#iso_week_number_valid_date": 1,
            "examples/otp/json.exs#json_roundtrip": 42,
            "examples/otp/queue.exs#reverse_involution": 6,
            "examples/otp/queue.exs#reverse_exchanges_ends": 6,
            "examples/otp/queue.exs#front_insert_peek": 6,
            "examples/otp/queue.exs#split_preserves_ordered_partition": 26,
            "examples/otp/array.exs#set_obeys_update_contract": 3,
            "examples/otp/gb_trees.exs#balance_preserves_observations": 3,
            "examples/otp/rand.exs#shuffle_permutes": 2,
            "examples/otp/rand.exs#shuffle_short_consumes_nothing": 1,
        }
        for tag, min_count in expected_groups.items():
            matched = [c for c in all_cases if c.get("upstream_example") == tag]
            self.assertEqual(len(matched), min_count, tag)
            self.assertTrue(all("lean_origins" not in c and not c.get("assert_true")
                                for c in matched), tag)
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            source = work / "upstream_stdlib.json"
            expressions = [
                "B = <<251,255>>, Opts = #{mode => urlsafe, padding => false}, "
                "E = base64:encode(B, Opts), D = base64:decode(E, Opts), {E, D, D =:= B}",
                "B = <<15,16,255>>, {binary:encode_hex(B), "
                "binary:encode_hex(B, lowercase), binary:decode_hex(<<\"0aA0fF\">>)}",
                "differential:call(binary,decode_hex,[<<\"0\">>])",
                "Y = 2000, D1 = calendar:date_to_gregorian_days({Y,1,1}), "
                "D2 = calendar:date_to_gregorian_days({Y+400,1,1}), {D1, D2, D2 =:= D1 + 146097}",
                "differential:call(calendar,datetime_to_gregorian_seconds,[{{2024,1,1},{24,0,0}}])",
                "V = #{<<\"a\">> => 1, <<\"b\">> => [false, null]}, "
                "E = iolist_to_binary(json:encode(V)), {E, json:decode(E) =:= V}",
                r'differential:call(json,decode,[<<"\"\\uD800\\uD800\"">>])',
                r'differential:call(json,decode,[<<"\"\\uD800\"">>])',
                "differential:call(json,decode,[<<\"1 2\">>])",
                "differential:call(json,decode,[<<\"1e1000\">>])",
                "Q = {[3,2],[1]}, R1 = queue:reverse(Q), R2 = queue:reverse(R1), "
                "{queue:to_list(R1), queue:peek(Q), queue:peek_r(Q), queue:to_list(R2) =:= queue:to_list(Q)}",
                "Q = queue:from_list([1,2,3,4]), {Q1,Q2} = queue:split(2,Q), "
                "{queue:to_list(Q1), queue:to_list(Q2), queue:to_list(queue:join(Q1,Q2))}",
                "differential:call(queue,out,[queue:new()])",
            ]
            source.write_text(json.dumps([{"expression": e} for e in expressions]))
            output = work / "results"
            self.assertEqual(
                self.invoke(output, "--replay", source),
                {"laws": 0, "passed": len(expressions), "failed": 0,
                 "unverified": 0, "uncovered": 0},
            )
            check = subprocess.run([
                "elixir", "-e", """
                [path] = System.argv()
                values = JSON.decode!(File.read!(path))["expected"]
                         |> Enum.map(&:erlang.binary_to_term(Base.decode64!(&1)))
                [
                  {:ok, {"-_8", <<251, 255>>, true}},
                  {:ok, {"0F10FF", "0f10ff", <<10, 160, 255>>}},
                  {:exception, :error, :badarg},
                  {:ok, {730485, 876582, true}},
                  {:exception, :error, :function_clause},
                  {:ok, {json_bin, true}},
                  {:exception, :error, {:unexpected_sequence, "\\\\uD800\\\\uD800"}},
                  {:exception, :error, :unexpected_end},
                  {:exception, :error, {:invalid_byte, 32}},
                  {:exception, :error, {:unexpected_sequence, "1.0e1000"}},
                  {:ok, {[3, 2, 1], {:value, 1}, {:value, 3}, true}},
                  {:ok, {[1, 2], [3, 4], [1, 2, 3, 4]}},
                  {:ok, {:empty, {[], []}}},
                ] = values
                true = is_binary(json_bin)
                true = (:json.decode(json_bin) === %{"a" => 1, "b" => [false, :null]})
                """, str(output / "manifest.json"),
            ], capture_output=True, text=True, timeout=30)
            self.assertEqual(check.returncode, 0, check.stdout + check.stderr)


if __name__ == "__main__":
    unittest.main()
