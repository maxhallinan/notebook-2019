import {schema} from "prosemirror-schema-basic";
import {EditorState} from "prosemirror-state";
import {EditorView} from "prosemirror-view";
import {baseKeymap} from "prosemirror-commands";
import {keymap} from "prosemirror-keymap";
import "prosemirror-view/style/prosemirror.css";

const root = document.getElementById('root');

const state = new EditorState.create({
  schema,
  plugins: [keymap(baseKeymap)],
});
const view = new EditorView(document.body, {state});
