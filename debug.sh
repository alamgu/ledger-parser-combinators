#!/bin/bash

set -e

function get_text_addr()
{
    local elf="$1"

    armv6l-unknown-none-eabi-readelf -WS "$elf" \
        | grep '\.text' \
        | awk '{ print "0x"$5 }'
}

function main()
{
    local script_dir
    local launcher_text_addr
    local app="$1"
    # script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
    script_dir="$( dirname $(which speculos) )"
    local launcher_path="$script_dir/../libexec/launcher"
    launcher_text_addr=$(get_text_addr "$launcher_path")
    local gdbinit="./gdbinit"

#source ${gdbinit}
    cat >/tmp/x.gdb<<EOF
define connect
set architecture arm
target remote 127.0.0.1:1234
handle SIGILL nostop pass noprint
catch signal SIGSEGV
add-symbol-file "${launcher_path}" ${launcher_text_addr}
add-symbol-file "${app}" 0x40000000
set substitute-path library "${rustLibSrc}"
set substitute-path /rustc/53cb7b09b00cbea8754ffb78e7e3cb521cb8af4b/library "${rustLibSrc}"
b *0x40000000
c
end
set history save on
set history size unlimited
set print asm-demangle
define incrementalDisassemble
define hook-stop
disassemble /s \$pc-8,\$pc+8
end
end
define restart
kill
shell sleep 1 
connect
end
connect
EOF

    gdb -q -nh -x /tmp/x.gdb
}

if [ $# -ne 1 ]; then
    echo "Usage: $0 <app.elf>"
    exit 1
fi

main "$1"
