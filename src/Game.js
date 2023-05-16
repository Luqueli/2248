import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import { joinResult, numberToColor, valueInPos} from './util';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [numOfColumns, setNumOfColumns] = useState(null);
  const [score, setScore] = useState(0);
  const [path, setPath] = useState([]);
  const [waiting, setWaiting] = useState(false);
  const [PossiblePathAdd, setPossiblePathAdd] = useState(0);

  useEffect(() => {
    // This is executed just once, after the first render.
    PengineClient.init(onServerReady);
  }, []);

  /**
   * Called when the server was successfully initialized
   */
  function onServerReady(instance) {
    pengine = instance;
    const queryS = 'init(Grid, NumOfColumns)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setNumOfColumns(response['NumOfColumns']);
      }
    });
  }

  /**
   * Called while the user is drawing a path in the grid, each time the path changes.
   */
  function onPathChange(newPath) {
    // No effect if waiting.
    if (waiting) {
      return;
    }
    setPossiblePathAdd(redondeo((addPath(newPath))));
    console.log("Score:" + PossiblePathAdd);
    setPath(newPath);
    console.log(JSON.stringify(newPath));
  }

  function addPath(newPath) {
    var sum = 0;
    for (var j = 0; j < newPath.length; j++) {
    sum += valueInPos(newPath[j], grid, numOfColumns);
    }
    return sum;
    }

 
  
  /**
   * Called when the user finished drawing a path in the grid.
   */
  function onPathDone() {
    /*
    Build Prolog query, which will be like:
    join([
          64,4,64,32,16,
          64,8,16,2,32,
          2,4,64,64,2,
          2,4,32,16,4,
          16,4,16,16,16,
          16,64,2,32,32,
          64,2,64,32,64,
          32,2,64,32,4
          ], 
          5, 
          [[2, 0], [3, 0], [4, 1], [3, 1], [2, 1], [1, 1], [1, 2], [0, 3]],
          RGrids
        ).
    */
    setPossiblePathAdd(0);    
    const gridS = JSON.stringify(grid);
    const pathS = JSON.stringify(path);
    const queryS = "join(" + gridS + "," + numOfColumns + "," + pathS + ", RGrids)";
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setScore(score + joinResult(path, grid, numOfColumns));
        setPath([]);
        animateEffect(response['RGrids']);
      } else {
        setWaiting(false);
      }
    });
  }

  /**
   * Displays each grid of the sequence as the current grid in 1sec intervals.
   * @param {number[][]} rGrids a sequence of grids.
   */
  function animateEffect(rGrids) {
    setGrid(rGrids[0]);
    const restRGrids = rGrids.slice(1);
    if (restRGrids.length > 0) {
      setTimeout(() => {
        animateEffect(restRGrids);
      }, 1000);
    } else {
      setWaiting(false);
    }
  }

  if (grid === null) {
    return null;
  }
 
  function redondeo(num){
    const log2 = Math.floor(Math.log2(num));
    return Math.pow(2,log2) === num ? num : Math.pow(2,log2+1);
  }

  function handleButtonClick() {
    const gridS = JSON.stringify(grid);
    const queryS = "booster(" + gridS + "," + numOfColumns + ", RGrids)";
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        animateEffect(response['RGrids']);
      } else {
        setWaiting(false);
      }
    });
  }
 
  return (
    <div className="game">
       <div className="header">
        <div className="contenedor">
         <div className="score">{score}</div>
           <div className="marco" id='marco' style={{
            backgroundColor : numberToColor(PossiblePathAdd),
            visibility : path.length>0
           }}>
        <div className="sum">{PossiblePathAdd}</div>
      </div>
    </div>
  </div>
      <Board
        grid={grid}
        numOfColumns={numOfColumns}
        path={path}
        onPathChange={onPathChange}
        onDone={onPathDone}
      />
      <button class="boton-booster" onClick={handleButtonClick}>Booster</button>
    </div>
  );
}

export default Game;