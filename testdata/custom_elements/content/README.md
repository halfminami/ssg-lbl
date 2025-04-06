# Customizing elements in Markdown
This is to show how to change elements in Markdown~~ and HTML~~. This
configuration is **global** and has *limitations*.

- `p`, `h1-6`, `blockquote`, `ul`, `ol`, `li`, `strong`, `em`, `s`, `a`, `code`
  can be customized and the key is the same as the name
- codeblock (`<pre><code>`)
  is a two-element syntax and `'tagname` makes no sense
- `hr`
  is a self-closing element and setting `'tagname` to any other non
  self-closing element makes no sense

---

You can customize elements by:

1. update an association list of key `'custom-tag` in `config`
2. each entry consists of `'tagname`, `'classname` and `'raw`
  - `'classname` is a string
  - `'raw` is put as-is
>it performs no validation, don't put `class="..."` in `'raw`

The `config` object for this file can be found in `run.scm`.

[back to h1](#customizing-elements-in-markdown)

I think I've used all the elements.
