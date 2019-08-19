# CodeMirror

## Basic usage

Three imports:

- The core library
- A CodeMirror mode (e.g. `import 'codemirror/lib/codemirror.js'`)
- The CodeMirror stylesheet: 'codemirror/lib/codemirror.css';

Create a CodeMirror instance:

```js
const myCodeMirror = CodeMirror(<DOM Element to bind to>);
```

Configure the CodeMirror instance:

```javascript
const myCodeMirror = CodeMirror(editorParent, {
  value: '(x) => x', // initial value
  mode: 'javascript', // language mode
})
```

Configuration options:

- `value`: the starting value of the editor
- `mode`: the mode to use, defaults to the first mode loaded
- `lineSeparator`: specify which characters are line breaks
- `theme`: this maps classnames to a theme stylesheet using the name of the 
  theme. `.cm-s-[name]`.
- `indentUnit`: how many spaces to indent. defaults to 2.
- `smartIndent`: uses context-sensitive indentation. if `false`, indents to the
  the same level as the previous line.
- `tabSize`: the width of a tab character. defaults to 4.
- `indentWithTabs`: when indenting, the first N* `tabSize` spaces should be 
  replaced by N tabs. Defaults to false.
