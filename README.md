# DIY minimal SSG

- Markdown->HTML
  - not going to support all of the markdown syntaxes out there, but just
    enough to write something
- config
- simple templating

## Markdown parsing
- source [`markdown.scm`](./lib/markdown.scm)
- test [`markdown_test.scm`](./lib/markdown_test.scm)

I will support Markdown syntaxes that I remember only.
- block element
  - heading starts with `r/#+ /`
  - ul starts with `- `, `+ ` or `* ` and can be nested
  - ol starts with `r/\d+\. /` and can be nested
  - codeblock starts with triple backticks (not indent) and characters inside are escaped
  - hr is `r/---*/`
  - blockquote starts with `> ` and can be nested (only with `>> `, `>>> `, ...)
  - a line containing only `;;;off` turns off parser until another `;;;on`
    - any element can be output by raw HTML
  - otherwise it's p
- inline element
  - code starts with a single backtick
  - link is `[text](url)`
  - emph starts with `_` or `*`
  - strong starts with `__` or `**`
  - strikethrough starts with `~~`
  - variable in frontmatter is `{name}`
Note that `<table>` and `<img>` are not supported.

The parser tries to parse text line by line.

Codeblock and link url are not customizable.
Self-closing tag cannot be customized to produce a closing tag.
