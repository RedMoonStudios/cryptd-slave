#!/usr/bin/env python
import os
import sys
import subprocess

CC = 'gcc'
STATIC_LIBS = ['gmp', 'ffi']


def get_library_paths():
    process = subprocess.Popen(
        [CC, '-Xlinker', '-v'],
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
    )
    stderr = process.stderr.read()
    process.wait()
    return [p[2:] for p in stderr.split() if p.startswith('-L')]


def find_lib(lib):
    for try_path in get_library_paths():
        path = os.path.join(try_path, 'lib%s.a' % lib)
        if os.path.exists(path):
            return path


def mkstatic(arg):
    if arg.startswith('-l') and arg[2:] in STATIC_LIBS:
        return find_lib(arg[2:])

    return arg


if __name__ == '__main__':
    subprocess.call([CC] + map(mkstatic, sys.argv)[1:])
