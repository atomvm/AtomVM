//
// This file is part of AtomVM.
//
// Copyright 2025 Mikael Karlsson <mikael.karlsson@creado.se>
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
//

import gleam/erlang/process.{type Subject}
import gleam/io
import gleam/otp/actor
import gleam/otp/static_supervisor as supervisor
import gleam/otp/supervision

pub type RequestMessage {
  SendPid
  Kill
}

pub type ReplyMessage {
  MyPidIs(process.Pid)
  Aaarghhh
}

// AtomVM currently requires a start/0 function instead of main/0
pub fn start() -> Nil {
  io.println("Hello from AtomVM!")
  main()
}

pub fn main() -> Nil {
  io.println("Hello from Gleam !")
  io.println("Hello from supervise_actor!")
  let actor_name1 = process.new_name("actor1")
  let actor_name2 = process.new_name("actor2")
  let subject1: Subject(RequestMessage) = process.named_subject(actor_name1)
  let subject2: Subject(RequestMessage) = process.named_subject(actor_name2)
  let my_subject: Subject(ReplyMessage) = process.new_subject()

  let child_spec1 =
    supervision.worker(fn() { init(my_subject, actor_name1) })
    |> supervision.restart(supervision.Permanent)

  let child_spec2 =
    supervision.worker(fn() { init(my_subject, actor_name2) })
    |> supervision.restart(supervision.Permanent)

  io.println("Start a supervisor with two permanent actor children.")
  let assert Ok(actor.Started(_pid, _supervisor)) =
    supervisor.new(supervisor.OneForOne)
    |> supervisor.add(child_spec1)
    |> supervisor.add(child_spec2)
    |> supervisor.start()

  io.println("Get child pids.")
  let #(pid1, pid2) = echo get_pids(subject1, subject2, my_subject)
  assert pid1 != pid2

  io.println("Kill first actor, it shall be restarted with new pid.")
  process.send(subject1, Kill)
  let assert Ok(Aaarghhh) = process.receive(my_subject, 100)
  // Give the supervisor some time to restart actor 1,
  process.sleep(200)
  io.println("Get child pids again, first shall be new.")
  let #(pid3, pid4) = echo get_pids(subject1, subject2, my_subject)
  assert pid1 != pid3
  assert pid2 == pid4

  // Now add a new OneForAll supervisor since PR 1958 is done

  io.println("Start a OneForAll supervisor with two permanent children.")
  let actor_name1 = process.new_name("actor3")
  let actor_name2 = process.new_name("actor4")
  let subject1: Subject(RequestMessage) = process.named_subject(actor_name1)
  let subject2: Subject(RequestMessage) = process.named_subject(actor_name2)

  let child_spec1 =
    supervision.worker(fn() { init(my_subject, actor_name1) })
    |> supervision.restart(supervision.Permanent)

  let child_spec2 =
    supervision.worker(fn() { init(my_subject, actor_name2) })
    |> supervision.restart(supervision.Permanent)

  let assert Ok(actor.Started(_pid, _supervisor)) =
    supervisor.new(supervisor.OneForAll)
    |> supervisor.add(child_spec1)
    |> supervisor.add(child_spec2)
    |> supervisor.start()

  io.println("Get child pids.")
  let #(pid1, pid2) = echo get_pids(subject1, subject2, my_subject)
  assert pid1 != pid2

  io.println("Kill second actor, both shall be restarted with new pid.")
  process.send(subject2, Kill)
  let assert Ok(Aaarghhh) = process.receive(my_subject, 100)
  // Give the supervisor some time to restart actor 2,
  process.sleep(200)
  let #(pid3, pid4) = echo get_pids(subject1, subject2, my_subject)
  assert pid1 != pid3
  assert pid2 != pid4
  Nil
}

// --------- Actor stanza--------
type State {
  State(client: Subject(ReplyMessage))
}

fn init(
  client: Subject(ReplyMessage),
  name: process.Name(RequestMessage),
) -> actor.StartResult(Subject(RequestMessage)) {
  let state = State(client: client)
  actor.new(state)
  |> actor.named(name)
  |> actor.on_message(handle_message)
  |> actor.start()
}

fn handle_message(state: State, message: RequestMessage) {
  case message {
    SendPid -> {
      actor.send(state.client, MyPidIs(process.self()))
      actor.continue(state)
    }
    Kill -> {
      actor.send(state.client, Aaarghhh)
      actor.stop_abnormal("My manager killed me!")
    }
  }
}

fn get_pids(
  subject1: Subject(RequestMessage),
  subject2: Subject(RequestMessage),
  my_subject: Subject(ReplyMessage),
) {
  process.send(subject1, SendPid)
  let assert Ok(MyPidIs(pid1)) = process.receive(my_subject, 300)
  process.send(subject2, SendPid)
  let assert Ok(MyPidIs(pid2)) = process.receive(my_subject, 300)
  #(pid1, pid2)
}
