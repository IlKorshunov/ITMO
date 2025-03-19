package info.kgeorgiy.ja.korshunov.chess.game;

import net.java.quickcheck.collection.Pair;

public class Figure {
    private Cell curCell;
    private Cell.TYPE curType;

    public enum Move {UP, DOWN, LEFT, RIGHT}

    public void setCell(Cell cell) {
        this.curCell = cell;
    }

    public Cell getCell() {
        return curCell;
    }

    public Figure(Cell cell, Cell.TYPE type) {
        this.curType = type;
        this.curCell = cell;
    }

    public void Move(Move move) {
        Pair<Integer, Integer> curCoord = curCell.getCoord();
        int x = curCoord.getFirst();
        int y = curCoord.getSecond();
        switch (move) {
            case UP -> y++;
            case DOWN -> y--;
            case LEFT -> x--;
            case RIGHT -> x++;
        }
        Cell newCell = Desk.getCell(x, y);
        setCell(newCell);
    }
}
