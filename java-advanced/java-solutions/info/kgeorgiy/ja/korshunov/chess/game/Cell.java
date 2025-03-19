package info.kgeorgiy.ja.korshunov.chess.game;

import net.java.quickcheck.collection.Pair;

public class Cell {
    protected static final String whiteKing = "♔";
    protected static final String whiteRook = "♖";
    protected static final String blackKing = "♚";

    public enum TYPE {EMPTY, WK, WR, BK}

    private final int x, y; // x >= 1, y >= 1
    private TYPE cellType;

    private boolean isCorrectCell() {
        return x <= Desk.row && y <= Desk.col;
    }

    public Cell(int x, int y) { // отсчет ведется с левого нижнего угла
        this(x, y, TYPE.EMPTY);
    }

    public Cell(int x, int y, TYPE cellType) {
        this.x = x;
        this.y = y;
        this.cellType = cellType;
    }

    public Cell(String input, TYPE cellType) {
        this.y = Character.toLowerCase(input.charAt(0)) - 'a' + 1;
        this.x = Integer.parseInt(String.valueOf(input.charAt(1)));
        this.cellType = cellType;
    }

    public String getPos() {
        return "" + (char) ('a' + y - 1) + x;
    }

    public Pair<Integer, Integer> getCoord() {
        return new Pair<>(x, y);
    }

    public TYPE getType() {
        return cellType;
    }

    public void setType(TYPE cellType) {
        this.cellType = cellType;
    }

    public String getString() {
        switch (cellType) {
            case EMPTY -> {
                return "|_|";
            }
            case WK -> {
                return "|" + whiteKing + "|";
            }
            case WR -> {
                return "|" + whiteRook + "|";
            }
            case BK -> {
                return "|" + blackKing + "|";
            }
            default -> {
                System.err.println("что-то странное");
                return null;
            }
        }
    }

    @Override
    public String toString() {
        return getString();
    }
}
