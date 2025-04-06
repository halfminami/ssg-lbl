# DIY minimal SSG

- Markdown->HTML
  - basic Markdown only, but hopefully just enough to write some blog posts
- config
  - arbitrary Scheme
- simple templating
  - template per directory (HTML only)

## Markdown parser

The parser processes text line by line.
It doesn't validate input.

Below syntax can be used in `.md`.
(In `.html` files, only variables are replaced.)
- block element
  - heading starts with `r/#+ /`
    - id is generated with its content (variables won't get expanded!)
    - collected for table of contents
      - not available in frontmatter because it needs scanning the whole file
        might prepend it
  - ul starts with `- `, `+ ` or `* ` and can be nested
  - ol starts with `r/\d+\. /` and can be nested
  - codeblock starts with triple backticks (not indent) and characters inside are escaped
    - `lang-` class (for [highlight.js](https://highlightjs.org/))
  - hr is `r/---*/`
  - blockquote starts with `>` (no space) and can be nested (`>>`, `>>>`, ...)
  - a line `;;;off` turns off the parser until another `;;;on`
    - any element can be output by raw HTML
  - otherwise it's p
- inline element
  - code starts with a single backtick
  - link is `[text](url)`
  - emph starts with `_` or `*`
  - strong starts with `__` or `**`
  - strikethrough starts with `~~`
  - variable in frontmatter is `{{name}}` (no leading/trailing spaces)
Note that `<table>` and `<img>` are not supported.

### Frontmatter

Frontmatter starts with a line `---scm` and ends with a line `---`.
It is only allowed at the very top of the file.

like so:
```
---scm
(define tomorrow "2025-03-20")
`((created . "2025-03-19")
  (modified . ,tomorrow))
---

This page was last modified on {{modified}}.
```
The last expression must produce an association list (metadata). The keys in
the association list are converted to strings and can be accessed using the
variable syntax.

If no metadata is needed for a page, just omit the frontmatter.

---

Inside frontmatter you can access `env` hash-table which contains information
about the current file and metadata in subdirectory. It is merged into current
file's metadata and become available for files in parent directory.

#### Templating

A template (`+template.html`) can be placed per directory and parsed files in
the same directory will use it.
[`+template.html` in `template_child_frontmatter` test](./testdata/template_child_frontmatter/content/posts/+template.html)
shows how to access metadata of each `.md` files.

(Currently only `.html` files can access `{{content}}` plus using it outside
templates can break the program! No template inheritance.)

#### Subdirectory and Special definitions

Files are traversed from bottom to top.
Metadata of files in a subdirectory is grouped by its path and put in an
association list.
[`posts.html` in `subdir_frontmatter` test](./testdata/subdir_frontmatter/content/posts.html)
shows how to access metadata of files in a subdirectory.

### Output paths

If `.html` or `.md` files are not named `index.html` or `index.md`, they will
be put under the corresponding directory and named `index.html`.
The case where there're two files for a directory (e.g. `hello.md` and
`hello/index.md`) is not handled.

If input is:
```
index.md
hello.md
posts.md
posts/
  one.md
  two/
    index.md
```
Then the output would be:
```
index.html
hello/
  index.html
posts/
  index.html
  one/
    index.html
  two/
    index.html
```
(Please see [`path_index` test](./testdata/path_index/) for this example.)

Files that cannot be parsed are copied.

### Config

[config.scm](./config.scm) is loaded and `config` hash-table is used. You can
also change part of it via command-line.

Notably you can customize HTML elements of Markdown to some extent.
You can do so by setting `'custom-tag` key in `config`.
Each element is represented by an association list which may contain
`'tagname`, `'classname` and `'raw` and formatted as `<TAGNAME class=CLASSNAME RAW></TAGNAME>`.
Please see [`custom_elements` test](./testdata/custom_elements/content/README.md)
and [unit tests](./lib/markdown/test.scm) for examples.
(Codeblock and link url are not customizable. Self-closing tags cannot be
customized to produce a closing tag.)

#### Command line

At `./`:
```
./gen.scm
```

## Uses

[Gauche](https://practical-scheme.net/gauche)
```
$ gosh -V
Gauche scheme shell, version 0.9.15 [utf-8,pthreads], x86_64-pc-linux-gnu
```

### Tests

At `./`:
```
make test
```
will run all the tests.
