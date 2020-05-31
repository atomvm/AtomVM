##
## Copyright (c) dushin.net
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

import sys
import json
from optparse import OptionParser

def create_option_parser():
    parser = OptionParser()
    parser.add_option(
        "--root_dir",
        dest="root_dir",
        help="Root directory",
        type="string",
    )
    parser.add_option(
        "--config",
        dest="config",
        help="Config file (default: ./mkimage.json)",
        type="string",
        default="mkimage.json"
    )
    parser.add_option(
        "--out",
        dest="out",
        help="Output file (default: ./atomvm.img)",
        type="string",
        default="atomvm.img"
    )
    return parser

def from_hex(str) :
    return int(str, 0)

def replace(pattern, root_dir, s) :
    return s.replace("${%s}" % pattern, root_dir)

def mkimage(root_dir, output_file, segments) :
    offset = -1
    print("Writing output to {}".format(output_file))
    print("=============================================")
    with open(output_file, "wb") as fout :
        for segment in segments :
            segment_offset = from_hex(segment["offset"])
            if offset == -1 :
                offset = segment_offset
            else :
                padding = b"\xff" * (segment_offset - offset)
                fout.write(padding)
                offset = segment_offset
            path = replace("ROOT_DIR", root_dir, segment['path'])
            with open(path, "rb") as fin:
                data = fin.read()
                fout.write(data)
                print("Wrote {} at offset {} ({})".format(segment["name"], segment["offset"], offset))
                offset += len(data)
    return offset

def load_config(config_file):
    with open(config_file) as f:
        return json.loads(f.read())

def print_help(parser, message) :
    print("##")
    print("## {}".format(message))
    print("##")
    parser.print_help()

def main(argv):
    parser = create_option_parser()
    (options, _args) = parser.parse_args(argv)
    if not options.root_dir :
        print_help(parser, "root_dir option is required and should be the root directory of the AtomVM checkout")
        return -1
    try :
        config = load_config(options.config)
        return mkimage(options.root_dir, options.out, config['segments']) > 0
    except Exception as e :
        print("An error occurred: {}".format(e))
        return -1

if __name__ == "__main__":
    sys.exit(main(sys.argv))
