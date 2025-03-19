package info.kgeorgiy.ja.korshunov.chess.algoritm;

import info.kgeorgiy.ja.korshunov.chess.game.Cell;
import info.kgeorgiy.ja.korshunov.chess.game.Desk;
import info.kgeorgiy.ja.korshunov.chess.game.Figure;

import java.util.Scanner;

import static info.kgeorgiy.ja.korshunov.chess.game.Cell.TYPE.*;

public class Winning {
    private final int row, col;
    private final Desk desk;
    private Figure wk, wr, bk;
    private boolean isBKInCorner, isWkAsHorse, finlMove;

    private int countMoves;

    public Winning(int row, int col, int xBK, int yBK, int xWK, int yWK, int xWR, int yWR) {
        this.row = row;
        this.col = col;
        this.desk = new Desk(row, col);
        setFigure(xBK, yBK, BK);
        setFigure(xWK, yWK, WK);
        setFigure(xWR, yWR, WR);
        isBKInCorner = true;
        isWkAsHorse = false;
        finlMove = false;
    }

    public void setFigure(int x, int y, Cell.TYPE cellType) {
        Cell curCell = Desk.getCell(x, y);
        curCell.setType(cellType);
        switch (cellType) {
            case BK -> bk = new Figure(curCell, BK);
            case WK -> wk = new Figure(curCell, WK);
            case WR -> wr = new Figure(curCell, WR);
        }
    }

    private void startGame() {
        Scanner scanner = new Scanner(System.in);
        System.out.println("Начальное положение доски:");
        desk.showDesk();

        while (true) {
            System.out.println("Ход белых (программа):");
            makeWhiteMove();
            desk.showDesk();
            countMoves++;
            System.out.println(countMoves);

            if (isCheckmate()) {
                System.out.println("Мат! Белые победили.");
                break;
            }

            System.out.println("Ваш ход (черные):");
            System.out.print("Введите ход : ");
            String move = scanner.nextLine();
            makeBlackMove(move);
            desk.showDesk();
            countMoves++;
            System.out.println(countMoves);
        }

        scanner.close();
    }

    private void makeWhiteMove() { // надо реализовать адекватно
        if (isBKInCorner) {
            pushKingToCorner();
            return;
        }
        if (isWkAsHorse) {
            placeKingAtKnightsDistance();
            return;
        }
        if (finlMove) {
            placeCheckmate();
        }
    }

    private void pushKingToCorner() {

    }

    private void placeKingAtKnightsDistance() {
    }

    private void placeCheckmate() {
    }


    private void makeBlackMove(String move) {
        int startX = Character.toLowerCase(move.charAt(0)) - 'a' + 1;
        int startY = Character.getNumericValue(move.charAt(1));
        int endX = Character.toLowerCase(move.charAt(2)) - 'a' + 1;
        int endY = Character.getNumericValue(move.charAt(3));

        Cell startCell = Desk.getCell(startY, startX);
        Cell endCell = Desk.getCell(endY, endX);

        if (startCell != null && endCell != null && startCell.getType() == BK) {
            startCell.setType(EMPTY);
            endCell.setType(BK);
            bk.setCell(endCell); // здесь надо через Move, наверное, написать
        } else {
            System.out.println("Некорректный ход. Попробуйте снова.");
        }
    }

    private boolean isCheckmate() {
        Cell kingCell = bk.getCell();
        int kingX = kingCell.getCoord().getFirst();
        int kingY = kingCell.getCoord().getSecond();

        if (wr.getCell().getCoord().getFirst() == kingX || wr.getCell().getCoord().getSecond() == kingY) {
            for (Figure.Move move : Figure.Move.values()) {
                int newX = kingX;
                int newY = kingY;
                switch (move) {
                    case UP -> newY++;
                    case DOWN -> newY--;
                    case LEFT -> newX--;
                    case RIGHT -> newX++;
                }
                Cell newCell = Desk.getCell(newY, newX);
                if (newCell != null && newCell.getType() == EMPTY && !isUnderAttack(newCell)) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    private boolean isUnderAttack(Cell cell) {
        int cellX = cell.getCoord().getFirst();
        int cellY = cell.getCoord().getSecond();
        int rookX = wr.getCell().getCoord().getFirst();
        int rookY = wr.getCell().getCoord().getSecond();
        return cellX == rookX || cellY == rookY;
    }

    public static void main(String[] args) {
        Winning game = new Winning(8, 8, 4, 4, 1, 8, 1, 1);
        game.startGame();
    }
}
