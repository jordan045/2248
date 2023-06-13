import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import { joinResult } from './util';
import MyButton from './Button';
import { numberToColor } from './util';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [numOfColumns, setNumOfColumns] = useState(null);
  const [score, setScore] = useState(0);
  const [tempScore, setTempScore] = useState(0);
  const [path, setPath] = useState([]);
  const [waiting, setWaiting] = useState(false);

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
    setPath(newPath);
    setTempScore(joinResult(newPath, grid, numOfColumns));
    console.log(JSON.stringify(newPath));
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
    const gridS = JSON.stringify(grid);
    const pathS = JSON.stringify(path);
    const queryS = "join(" + gridS + "," + numOfColumns + "," + pathS + ", RGrids)";
    console.log(queryS);
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
    setTempScore(0);
  }

  function booster(){
    if (waiting) {
      return;
    }
    const gridS = JSON.stringify(grid);
    const queryS = "booster(" + gridS + "," + numOfColumns + ", RGrids)";
    console.log(queryS);
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        console.log('done');
        animateEffect(response['RGrids']);
        console.log(response['RGrids']);
      } else {
        setWaiting(false);
      }
    });
  }

  function maxmove(){
    //maxmove(Grid,MaxList)
    if(waiting){
      return;
    }
    const gridS = JSON.stringify(grid);
    const queryS = "maxmove(" + gridS + ", MaxMove," + numOfColumns + ")";
    console.log(queryS);
/*     setWaiting(true); */
    pengine.query(queryS, (success, response) => {
      if (success) {
        console.log('done');
        onPathChange(response['MaxMove']);
        console.log(response['MaxMove']);
        console.log(path);
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
      }, 400);
    } else {
      setWaiting(false);
    }
  }

  if (grid === null) {
    return null;
  }
  return (
    <div className="game">
      <div className="header">
        <div className='dashscore'>
          <div className='header_text'>Puntaje total</div>
          <div className="score">{score}</div>
        </div>
        <div className='dashscore'>
          <div 
            className="square"
            style={tempScore === 0 ? undefined :{ width:'60px',height:'60px',backgroundColor: numberToColor(tempScore)}}
          >{tempScore}</div>
          <div className='header_text'>Jugada actual</div>
        </div>
      </div>
      <Board
        grid={grid}
        numOfColumns={numOfColumns}
        path={path}
        onPathChange={onPathChange}
        onDone={onPathDone}
      />
      <div className='buttons'>
      <MyButton
        className='square'
        onClick= {booster}
      />
      <MyButton
        className='square'
        onClick= {maxmove}
      />
      </div>
    </div>
  );
}

export default Game;