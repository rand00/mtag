# `mtag`, the static filesystem tagger / media-tagger

*My good friend Valdemar has shown interest in opensourcing this program some 
time ago - happy birthday (: Historically this application was called `n_tag` and 
is part of the suite of [`niseq`](https://r7p5.earth/) CLI-programs, in this 
case to manage and query my `niseq`-recordings and source material.*

## Description

Filesystems are tree-structures, which are extremely limited for querying
for all the data you have lying around. `mtag` allows you to tag any file
within a static part of the filesystem hierarchy. Static in this sense means
that files are added, but not moved or deleted.

Files can have many tags, which `mtag` lets you query, and which outputs
each found file on separate lines. This allows for composing CLI applications,
e.g. for loading all `mtag`s query-results in your favorite application. E.g.:
```bash
$ mpv $(mtag query type/video,color/green | head -n5)
```

By default `mtag` searches upwards from the current working directory to find
an `_mtags` directory, which contains all the tags for files within that
directory (as relative symlinks).
You have the responsibility to create the `_mtags` directory yourself, as you
are the one who knows what part of the filesystem will be kept static. 

All `mtag` commands optionally supports being given `--root=<root-dir>`
explicitly as the first argument, which overrides the recursive search for
the `_mtags` directory.

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

## Commands

### `mtag <tags> <paths..>`

Tag all the paths given, where <tags> is of the format:
  <tag>[,<tag>]*
where <tag> is of the format:
  [<tag-dir>/]*<tag-dir>
e.g.:
```bash
$ mtag score/5,color/nice/black file0 file1 file3
```

### `mtag query <query>`

Query for all files that match the query, where <query> is of the format:
  <[!]tag0>[,<[!]tag1>]*
where '!' means 'not'.

All files are printed newline separated. The output can be used as argument
to other CLI applications, e.g. in the `bash` shell:
  $ my_app $(mtag query mytag,!mytag2)

### `mtag rm <tags> <paths..>`

Remove all the given tags from the given paths. This means removing the
relative symlinks from within the `_mtags` directory, but not removing their
containing directories.

### `mtag tags_union <paths..>`

Output the tags that all the given paths have in common, i.e. the mathematical
set union.

The printed string has the same format as the <tags> parameter of the other
commands - this can be used to e.g. do (in bash):
  $ mtag query $(mtag tags_union file0 file1)

### `mtag tags_intersection <paths..>`

Works the same as `mtag tags_union` but using mathematical set intersection.





