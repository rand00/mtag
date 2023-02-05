# `mtag`, the static filesystem tagger / media-tagger

*My good friend Valdemar has shown interest in opensourcing this program some 
time ago - happy birthday (: Historically this application was called `n_tag` and 
is part of the suite of [`niseq`](https://r7p5.earth/) CLI-programs, in this 
case to manage and query my `niseq`-recordings and source material.*

## Description

Filesystems are tree-structures, which are extremely limited for structuring
your data. Also, much data is not textbased, so you can't easily do a
filesystem search on these files.
Tagging allows a many-to-one relation (many tags to one file), vs the
filesystems one-to-one relation. And tagging your files (e.g. videos) is an
act of mapping textual data to these files, which allow them to be queried.

`mtag` tags any file within an immutable part of the filesystem hierarchy,
whose base directory contains an `_mtags` directory. Immutable in this sense
means that files are added, but not moved or deleted.

When you query for files matching a set of tags, `mtag` outputs each found
file on separate lines. This allows for composing CLI applications,
e.g. for loading all `mtag`s query-results in your favorite application. E.g.:
```bash
$ mpv $(mtag query type/video,color/green | head -n5)
```

Note that the `bash` shell has problems with files containing spaces when
expanding the previous `mtag` command (you can set `IFS=$'\n'`).
The `fish` shell e.g. doesn't have this problem.

By default `mtag` searches upwards from the current working directory to find
an `_mtags` directory, which contains all the tags for files within that
directory (as relative symlinks).
You have the responsibility to create the `_mtags` directory yourself, as you
are the one who knows what part of the filesystem will be kept static. 

All `mtag` commands optionally supports being given `--root=<root-dir>`
explicitly as the first argument, which overrides the recursive search for
the immutable root dir containing the `_mtags` directory.

The directory structure could look as follows:
```bash
~/my_static_data
|-- _mtags/
|-- videos/
    |-- 2001_01_01
        |--video0.mp4
        |--video1.mp4
```

.. in this example, to run an `mtag` command, you would either `cd` into a
directory within `~/my_static_data` or pass `--root=~/my_static_data` to `mtag`.
E.g.:
```bash
# Initializing the static root
$ mkdir ~/my_static_data/_mtags
# Ordinary workflow
$ cd ~/my_static_data/videos/2001_01_01/
$ mtag score/5,animal/whale video0.mp4
$ mtag query animal/whale
> ~/my_static_data/videos/2001_01_01/video0.mp4
```

Another useful thing is e.g. to explore the `mtags_` directory with:
```bash
$ tree -d _mtags | less
```
.. which shows all the tags you have already created.

For composing `mtag` commands, it is advised to use the special `-`
path-argument, which represents `stdin`. This circumvents the mentioned shell
problems with whitespace, and allows very big sets of paths to be input, as
the OS have a limit on the size of the argument-list (see POSIX
`getconf ARG_MAX`). This allows you to e.g.:
  $ mtag query mytag | mtag tags_union -

## Commands

### `mtag <tags> <paths..>`

Tag all the paths given, where `<tags>` is of the format:
```
<tag>[,<tag>]*
```
where `<tag>` is of the format:
```
[<tag-dir>/]*<tag-dir>
```
e.g.:
```bash
$ mtag score/5,color/nice/black file0 file1 file3
```

If the first and single file is `-`, then `mtag` will read a newline separated
list of files from stdin instead.

`--root=<root-dir>` can be passed to override the search for the root
directory containing an `_mtags` dir, starting from the current working
directory.

### `mtag query <query>`

Query for all files that match the query, where `<query>` is of the format:
```
<[!][>]tag0>[,<[!][>]tag1>]*
```
where
  * `!` means 'not'. A not-expression in a query won't do anything if it stands alone
    (for performance reasons); it only filters other query expressions.
  * `>` means 'contained within', i.e. including within any sub-tag.

All files are printed newline separated. The output can be used as argument
to other CLI applications, e.g. in the `bash` shell:
```bash
$ my_app $(mtag query mytag,!mytag2,>mytag3,!>mytag4/mytag5)
```

.. note the earlier mentioned problems with whitespace in filenames, and the
limited argument-list on POSIX systems.

### `mtag rm <tags> <paths..>`

Remove all the given tags from the given paths. This means removing the
relative symlinks from within the `_mtags` directory, but not removing their
containing directories.

See `mtag <tags> <paths..>` for special arguments.

### `mtag tags_union <paths..>`

Output the tags that all the given paths have in common, i.e. the mathematical
set union.

The printed string has the same format as the `<tags>` parameter of the other
commands - this can be used to e.g. do (in bash):
```bash
$ mtag query $(mtag tags_union file0 file1)
```

See `mtag <tags> <paths..>` for special arguments.

### `mtag tags_intersection <paths..>`

Works the same as `mtag tags_union` but using mathematical set intersection.

## Installation

[Install](https://opam.ocaml.org/doc/Install.html) `opam`, the OCaml package manager.
E.g. on Arch Linux:
```bash
pacman -S opam
opam init
```

Install OCaml 4.14.1:
```bash
opam switch install 4.14.1
```

Then clone this repository and install dependencies and `mtag`:
```bash
git clone https://github.com/rand00/mtag
cd mtag
opam install rresult bos containers fmt digestif ppx_deriving dune 
dune build
cp bin/main.exe ~/bin/mtag
```



