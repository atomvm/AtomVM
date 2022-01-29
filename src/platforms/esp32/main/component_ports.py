#!/usr/bin/env python
#
# This file is part of AtomVM.
#
# Copyright 2020 Fred Dushin <fred@dushin.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

import os
import sys

def load(path) :
    with open(path) as f:
        return f.read()

def insert(component_port, component_ports_txt) :

    prev_pos = component_ports_txt.find("// DECLARATIONS")
    buf = component_ports_txt[:prev_pos]
    buf += "Context *%s_create_port(GlobalContext *global, term opts);\n" % component_port
    buf += "void %s_init(GlobalContext *global);\n" % component_port

    next_pos = component_ports_txt.find("// INIT")
    buf += component_ports_txt[prev_pos:next_pos]
    buf += "%s_init(global);\n    " % component_port
    prev_pos = next_pos

    next_pos = component_ports_txt.find("// CREATE_PORT")
    buf += component_ports_txt[prev_pos:next_pos]
    buf += "if (strcmp(port_name, \"%s\") == 0) { return %s_create_port(global, opts); }\n    " % (port_name(component_port), component_port)
    prev_pos = next_pos

    buf += component_ports_txt[prev_pos:]
    return buf

def port_name(component_port) :
    if component_port.endswith("_driver") :
        idx = component_port.find("_driver")
        return component_port[:idx]
    else :
        return component_port

def strip_line(line) :
    idx = line.find('#')
    if idx != -1 :
        line = line[:idx]
    return line.strip()

def main(argv):
    if len(argv) != 3 :
        print("Syntax: %s <component_ports_in> <component_ports_txt>" % argv[0])
        return -1
    try :
        component_ports_in = load(argv[1])
        component_ports_txt = load(argv[2]) if os.path.exists(argv[2]) else ""
        work = component_ports_in
        for component_port in component_ports_txt.splitlines() :
            component_port = strip_line(component_port)
            if len(component_port) > 0 :
                work = insert(component_port, work)
        print(work)
        return 0
    except Exception as e :
        print("An error occurred: {}".format(e))
        return -1

if __name__ == "__main__":
    sys.exit(main(sys.argv))
