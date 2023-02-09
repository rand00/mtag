# Roadmap

* test tag/rm/.. of symlinks (search for Path.resolve_and_normalize in Mtag)
  * should symlinks be able to be tagged?
  * if should be resolved, shouldn't it be recursive resolving?
* avoid depending on filesystem for paths given? (if can avoid resolve symlink)
  * 'to_absolute' could maybe just be an Fpath operation
  * (? if do) make `mtag rm` be able to remove tags from nonexistant paths
  * use `tree _mtags` to find broken symlinks
    * and show how to fix them
  * show how to use tail + head to 'paginate' through list of files
  * show how to use xargs to iteratively call program with files
  * show intresting real-life queries; e.g. using '>/,!>score'
* docs; write more interesting examples
  * use `>/,!foo` to allow standalone 'not'-expressions (explain, for performance)
  * use `mtag mytag (readlink -f tag)` inside `_mtags` to tag what symlink links to
    * note: it does so recursively
* `mtag daemon`
  * < daemon-mode that listens to filesystem to map symlinks automatically
    * > becomes a filesystem tagger for dynamic filesystems!
    * @nice; should be easy to implement
  * using: `inotifywait` (can listen to 'move' + 'delete')
  * perf-note: a single 'mv' can become very expensive
    * all tags need to be looked at
      * all files that match need to get symlinks mapped
        * e.g. all children of some major dir
    * @idea; could keep whole _mtags in memory and listen to FS changes to this too
      * fixes:
        * symlinks matching moved file are quickly found
        * all tags are cached in mem, so no major FS access 
* ? rename title (after prev features): "the universal file tagger"
  * don't mention 'media' to open up peoples minds to what can be tagged
  * 'universal' => any kind of file
  * @idea; rename binary?
    * e.g. `mtag` => `utag` 
      * I like `mtag`.. feels more unique, and like hint to original purpose
        related to media
    * e.g. `mtag` => `ftag` 
* add more interesting/complex examples to docs
  * interesting CLI combinations of mtag
  * `xargs` + other programs ++
* add section to README.md about other related tools like `tmsu`
  * .. didn't know about this before making `mtag` - they are very alike
* mtag query 'score/{<=4}'
  * where
    * `{}` encapsulates expressions on strings
    * `<=` means LTE for strings (which works for numbers too)
* `mtag query --long` ..
  * output a human-readable + script-parseable view of files and all their tags
* `mtag list-unique`
  * < see `code:projects:art:niseq:subtodos:20200713 how to use n tag tagging`
    * .. some sort of analysis for uniqueness of queried files
* `mtag export <query> <dir>`
  * exports all resulting files with original filenames + unique-postfix to `<dir>`
    as symlinks
    * creates the dir if doesn't exist
    * asks to override dir if does exist with files inside
