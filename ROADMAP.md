# Roadmap in ~ prioritized order

* `mtag daemon`
  * < daemon-mode that listens to filesystem to map symlinks automatically
    * \> becomes a filesystem tagger for dynamic filesystems!
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
* `mtag query --long` ..
  * output a human-readable + script-parseable view of files and all their tags
* check if it's at all useful to be more cleanly in the result-monad in the codebase
  * .. could be a help for the error messages?
* make a more correct description of query language; 
  [BNF](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form)? 
* come up with place to put (potentially a lot) of `mtag` examples
  * if being a lot - this should be @ dir @ git repo
  * though CLI usage-text should also contain a set of nice examples...
  * => 
    * do both, and refer from usage-text that more examples can be found in repo
    * make a link from example-section @ README.md to `examples` dir @ repo
      * .. note that this README then becomes more inconsistent with usage-text :/
* docs; write more interesting examples
  * show how to use `mtag export` to zip a group of files 
    * e.g. for sending to someone else
    * \< `zip` on linux follows symlinks by default
    * \< see `tar` `--dereference` option
  * show how to export `_mtags` via small `find`/`mtag` script to any tag manager 
    (here just reimport via `mtag` in other mtag-root)
    * and re-importing to other `mtag`-root is useful in itself!
  * valdemar scripts to tag files via thunar (:
  * use `find -execdir` to tag files of a certain type 
  * use `find -execdir` to tag files that is matched by some script
  * use `shuf` to shuffle the output of `mtag query`
  * use `tree _mtags` to find broken symlinks
    * and show how to fix them
  * show how to use `tail` + `head` to 'paginate' through list of files
  * show how to use `xargs` to iteratively call program with files
  * show interesting real-life queries; e.g. using `>/,!>score`
    * explain how this allows 'not'-expressions + why not allowed without
  * use `mtag mytag (readlink -f tag)` inside `_mtags` to tag what symlink links to
    * note: it does so recursively
  * interesting CLI combinations of mtag
* ? rename title (after prev features): "the universal file tagger"
  * don't mention 'media' to open up peoples minds to what can be tagged
  * 'universal' => any kind of file
  * @idea; rename binary?
    * e.g. `mtag` => `utag` 
      * I like `mtag`.. feels more unique, and like hint to original purpose
        related to media
    * e.g. `mtag` => `ftag` 
* add section to README.md about other related tools like `tmsu`
  * .. didn't know about this before making `mtag` - they are very alike
* `mtag query` expressions 
  * e.g. 
    * `score/(<=4)` 
    * could also be done on result of `mtag` by other programs:
      * `(mtime<1d2h)`
      * `(name=*.jpg)`
  * where
    * `()` encapsulates expressions on strings
    * `<=` means LTE for strings (which works for numbers too)
* `mtag list-unique`
  * < see `code:projects:art:niseq:subtodos:20200713 how to use n tag tagging`
    * .. some sort of analysis for uniqueness of queried files
* better errors 
  * make std-err warnings flush before printing to stderr?
    * OR maybe they _should_ be the last thing printed, for user to see the warnings 
      after long list on stdout?
  * define exit-codes with specific semantics 
    * \> better for scripts using `mtag` that need to error-handle
        
