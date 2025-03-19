package info.kgeorgiy.ja.korshunov.chess.game;

import java.util.ArrayList;
import java.util.List;

public class Desk {

    protected static int row, col;
    private static List<List<Cell>> desk;

    public Desk(int row, int col) {
        Desk.row = row;
        Desk.col = col;
        desk = new ArrayList<>(row);
        for (int i = 0; i < row; i++) {
            List<Cell> column = new ArrayList<>(col);
            for (int j = 0; j < col; j++) {
                column.add(new Cell(i + 1, j + 1, Cell.TYPE.EMPTY));
            }
            desk.add(column);
        }
    }

    public Desk() {
        this(8, 8);
    }

    public static Cell getCell(int x, int y) {
        if (x < 1 || x > row || y < 1 || y > col) {
            return null;
        }
        return desk.get(x - 1).get(y - 1);
    }

    public void showDesk() {
        for (int i = 0; i < row; i++) {
            System.out.print(i + 1 + " ");
            for (int j = 0; j < col; j++) {
                System.out.print(getCell(i + 1, j + 1).toString());
            }
            System.out.println();
        }

        System.out.print("  ");
        for (int j = 0; j < col; j++) {
            System.out.print(" " + (char) ('A' + j) + " ");
        }
        System.out.println();
    }

    public static void main(String[] args) {
        Desk desk = new Desk();
        desk.showDesk();
    }
}
