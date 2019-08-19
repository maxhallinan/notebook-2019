import React, { useState } from 'react';
import './App.css';

const cell = (id) => ({
  id,
  content: '',
  result: {
    type: 'success',
    value: '',
  },
  type: 'code',
});

const defaultState = {
  cells: {
    '0': cell(0),
  },
};

function App() {
  const [state, setState] = useState(defaultState);

  const newCell = () => {
    const nextId = Object.values(state.cells).length;

    if (state.cells[nextId - 1].content !== '') {
      setState({
        ...state,
        cells: {
          ...state.cells,
          [nextId]: cell(nextId),
        }
      });
    }
  };

  const updateSource = (id, source) => {
    setState({
        cells: {
          ...state.cells,
          [id]: {
            ...state.cells[id],
            content: source,
          },
        }
    });
  };

  const runSource = (id, contents) => {
    console.log(`Evaling: `, contents);
    /*global PS */
    const results = PS.Main.runFromJs(contents);

    console.log(PS["Data.Either"]);
    console.log('results', results, results[0] instanceof PS['Data.Either'].Right);
    const cells = Object.values(state.cells).reduce((cells, cell) => {
      const r1 = results[cell.id];
      const r2 = {};

      if (r1 instanceof PS['Data.Either'].Left) {
        r2.type = 'error';
        r2.value = r1.value0;
      }

      if (r1 instanceof PS['Data.Either'].Right) {
        r2.type = 'success';
        r2.value = r1.value0;
      }

      const update = { 
        ...cell,
        result: r2,
      }; 
      cells[cell.id] = update;
      return cells;
    }, {});

    setState({ ...state, cells, });
  };

  console.log(state.cells);

  const cellContents = Object.values(state.cells).map(({ content }) => content);

  return (
    <div className="App">
      <div className="doc">
        {Object.values(state.cells).map((cell, index) => {
          return (
            <div key={index}>
              <div className="cell">
                <div>
                  <p>{cell.result.type === 'success' ? cell.result.value : 'error'}</p>
                </div>
                <textarea 
                  onChange={(e) => updateSource(cell.id, e.target.value)} 
                  value={cell.content} 
                />
                <button 
                  onClick={() => runSource(cell.id, cellContents)}
                >
                  Run
                </button>
              </div>
            </div>
          );
        })}
        <button onClick={newCell}>New cell</button>
      </div>
    </div>
  );
}

export default App;
