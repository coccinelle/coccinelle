#!/usr/bin/env bash
set -e
for module in \
    Atomic \
    Arg Array ArrayLabels Bool Buffer Bytes BytesLabels Callback Char \
    Complex Digest Either Ephemeron Filename Float Format Fun Gc Hashtbl Int32 \
    Int64 Lazy Lexing List ListLabels Map Marshal MoreLabels Nativeint Obj Oo \
    Option Parsing Printexc Printf Queue Random Result Scanf Seq Set \
    Stack StdLabels String StringLabels Sys Uchar Weak In_channel Out_channel \
    Unit
 do
    echo $module
    target=../stdcompat__${module,}_s.mli.in
    echo $target
    ./interface_generator $module 5.0 4.14 4.13 4.12 4.11 4.10 4.09 4.08 4.07 4.06 4.05 4.04 4.03 4.02 4.01 4.00 3.12 \
       3.11 3.10 3.09 3.08 3.07 >$target
done
