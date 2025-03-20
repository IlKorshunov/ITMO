#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include "mysliders.h"
#include "plot.h"
#include <QAction>
#include <QCheckBox>
#include <QFormLayout>
#include <QGroupBox>
#include <QLabel>
#include <QLineEdit>
#include <QMainWindow>
#include <QMenu>
#include <QRadioButton>
#include <QStatusBar>
#include <QToolBar>
#include <QTranslator>

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow(QWidget *parent = nullptr);
    ~MainWindow();
    QRadioButton *noSelectionRadioButton;
    QRadioButton *itemRadioButton;
    QTranslator translator;
    QMenu *fileMenu;
    QAction *saveAction;
    QAction *loadAction;
    QAction *russianAction;
    QAction *englishAction;
    QStatusBar *statusBar;
    Plot *modifier;
    void loadRussianTranslation();
    void loadEnglishTranslation();
    void SaveSettings();
    void LoadSettings();
    MySliders *rowSliders;
    MySliders *colSliders;
    bool start = true;
    QLineEdit *stepXLineEdit;
    QLineEdit *stepZLineEdit;
    void pointChanged(const QPoint &pt);

private:
    void createMenuBar();
    void createToolBar();
    void createStatusBar();
    void createSettingsGroupBox(Q3DSurface *graph);
    void createPlot();

    QToolBar *toolBar;
    QLabel *coordinateLabel;
    QGroupBox *settingsGroupBox;
    QRadioButton *function1RadioButton;
    QRadioButton *function2RadioButton;
    QCheckBox *gridCheckBox;
    QCheckBox *labelsCheckBox;
    QCheckBox *labelBordersCheckBox;
    QLineEdit *xRangeMinLineEdit;
    QLineEdit *xRangeMaxLineEdit;
    QLineEdit *zRangeMinLineEdit;
    QLineEdit *zRangeMaxLineEdit;
    QLineEdit *stepCountLineEdit;
    Plot *plot;
    QGroupBox *allGradient;
    QGroupBox *stepGroupBox;
    QGroupBox *selectionModeGroupBox;
    QFormLayout *stepLayout;
};

#endif // MAINWINDOW_H
