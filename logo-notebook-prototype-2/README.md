# logo-notebook-prototype-2

This prototypes a markdown-rendering text cell for the Logo interactive 
notebook.
I wanted to test the suitability of Elm for this project.
I decided that Elm is not a suitable option because I need access to DOM 
properties like `selectionStart` and `selectionEnd` that Elm does not expose.
