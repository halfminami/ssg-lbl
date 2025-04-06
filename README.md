# DIY minimal SSG

- Markdown->HTML
  - basic Markdown only, but hopefully just enough to write some blog posts
- config
  - arbitrary Scheme
  - replace elements
- simple templating
  - master template (HTML only)

## Markdown parser

The parser tries to parse text line by line.
It doesn't validate input.

Below syntax can be used in `.md`.
(In `.html` files, only variables are replaced.)
- block element
  - heading starts with `r/#+ /`
    - id is generated with its content (template won't get expanded)
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
  - variable in frontmatter is `{{name}}`
Note that `<table>` and `<img>` are not supported.

Codeblock and link url are not customizable.
Self-closing tags cannot be customized to produce a closing tag.

## Frontmatter

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
The last expression must produce alist (metadata). The keys in alist are
converted to strings and can be accessed using variable syntax.

If no Frontmatter needed for the page, just omit it.

### Subdirectory

Files are traversed from bottom to top. A file can access metadata of files
in its subdirectory.

A file can access frontmatter of files in its subdirectory. 
They are grouped by subdirectory and put in alist.
Please see [`subdir_frontmatter` test](./testdata/subdir_frontmatter/) for an example.

## Output paths

If `.html` or `.md` files are not named `index.html` or `index.md`, they will
be put under the corresponding directory and named `index.html`.

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
