#!/usr/bin/env python
##
## Copyright (c) 2020 fred@dushin.net
## All rights reserved.
##
## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
##     http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.
##

import os
import sys

def load(path) :
    with open(path) as f:
        return f.read()

def insert(component_nif, component_nifs_txt) :

    prev_pos = component_nifs_txt.find("// DECLARATIONS")
    buf = component_nifs_txt[:prev_pos]
    buf += "void %s_init(GlobalContext *global);\n" % component_nif
    buf += "const struct Nif *%s_get_nif(const char *nifname);\n" % component_nif

    next_pos = component_nifs_txt.find("// INIT")
    buf += component_nifs_txt[prev_pos:next_pos]
    buf += "%s_init(global);\n    " % component_nif
    prev_pos = next_pos

    next_pos = component_nifs_txt.find("// GET_NIF")
    buf += component_nifs_txt[prev_pos:next_pos]
    buf += "if ((nif = %s_get_nif(nifname)) != NULL) { return nif; }\n    " % component_nif
    prev_pos = next_pos

    buf += component_nifs_txt[prev_pos:]
    return buf

def strip_line(line) :
    idx = line.find('#')
    if idx != -1 :
        line = line[:idx]
    return line.strip()

def main(argv):
    if len(argv) != 3 :
        print("Syntax: %s <component_nifs_in> <component_nifs_txt>" % argv[0])
        return -1
    try :
        component_nifs_in = load(argv[1])
        component_nifs_txt = load(argv[2]) if os.path.exists(argv[2]) else ""
        work = component_nifs_in
        for component_nif in component_nifs_txt.splitlines() :
            component_nif = strip_line(component_nif)
            if len(component_nif) > 0 :
                work = insert(component_nif, work)
        print(work)
        return 0
    except Exception as e :
        print("An error occurred: {}".format(e))
        return -1

if __name__ == "__main__":
    sys.exit(main(sys.argv))
