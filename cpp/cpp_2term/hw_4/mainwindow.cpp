#include "mainwindow.h"
#include "plot.h"
#include <QApplication>
#include <QFileDialog>
#include <QFormLayout>
#include <QMenuBar>
#include <QMessageBox>
#include <QPainter>
#include <QPushButton>
#include <QSettings>
#include <QSlider>
#include <QTextLayout>
#include <QVBoxLayout>
#include <QtGui>

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
{
    Q3DSurface *graph = new Q3DSurface();

    createMenuBar();
    createToolBar();
    createSettingsGroupBox(graph);

    QWidget *container = QWidget::createWindowContainer(graph);
    QSize screenSize = graph->screen()->size();
    container->setMinimumSize(QSize(screenSize.width() / 1.2, screenSize.height() / 1.6));
    container->setMaximumSize(screenSize);
    container->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    container->setFocusPolicy(Qt::StrongFocus);

    QWidget *widget = new QWidget;
    QHBoxLayout *hLayout = new QHBoxLayout(widget);
    hLayout->addWidget(container, 1);

    QHBoxLayout *layout = new QHBoxLayout;
    layout->addWidget(widget);
    layout->addWidget(settingsGroupBox);

    QWidget *centralWidget = new QWidget;
    centralWidget->setLayout(layout);
    setCentralWidget(centralWidget);

    statusBar = new QStatusBar(this);
    setStatusBar(statusBar);
    //LoadSettings();
}

MainWindow::~MainWindow()
{
    SaveSettings();
    delete modifier;
}

void MainWindow::createMenuBar()
{
    fileMenu = menuBar()->addMenu(tr("&File"));

    saveAction = new QAction(tr("&Save Settings"), this);
    saveAction->setShortcut(QKeySequence::Save);
    saveAction->setStatusTip(tr("Save current settings"));
    fileMenu->addAction(saveAction);

    loadAction = new QAction(tr("&Load Settings"), this);
    loadAction->setShortcut(QKeySequence::Open);
    loadAction->setStatusTip(tr("Load settings from a file"));
    fileMenu->addAction(loadAction);

    russianAction = new QAction(tr("&Русский"), this);
    fileMenu->addAction(russianAction);

    englishAction = new QAction(tr("&Englsih"));
    fileMenu->addAction(englishAction);
}

void MainWindow::createToolBar()
{
    toolBar = addToolBar(tr("File"));
    toolBar->addAction(saveAction);
    toolBar->addAction(loadAction);

    QAction *exitAction = new QAction(tr("Exit"), this);
    connect(exitAction, &QAction::triggered, qApp, &QApplication::quit);
    toolBar->addAction(exitAction);
}

void MainWindow::createSettingsGroupBox(Q3DSurface *graph)
{
    settingsGroupBox = new QGroupBox(tr("Settings"));

    // Gradient
    allGradient = new QGroupBox(tr("Gradients"));
    QLinearGradient grBtoY(0, 0, 1, 100);
    grBtoY.setStops({{1.0, Qt::black}, {0.67, Qt::blue}, {0.33, Qt::red}, {0.0, Qt::yellow}});
    QPixmap pm(24, 100);
    pm.fill(Qt::transparent);
    QPainter pmp(&pm);
    pmp.setBrush(QBrush(grBtoY));
    pmp.setPen(Qt::NoPen);
    pmp.drawRect(0, 0, 24, 100);
    QPushButton *gradientBtoYPB = new QPushButton;
    gradientBtoYPB->setIcon(QIcon(pm));
    gradientBtoYPB->setIconSize(QSize(24, 100));

    QLinearGradient grGtoR(0, 0, 1, 100);
    grGtoR.setStops({{1.0, Qt::darkGreen}, {0.5, Qt::yellow}, {0.25, Qt::red}, {0.0, Qt::darkRed}});
    grGtoR.setSpread(QGradient::PadSpread);
    pmp.setBrush(QBrush(grGtoR));
    pmp.drawRect(0, 0, 24, 100);
    QPushButton *gradientGtoRPB = new QPushButton;
    gradientGtoRPB->setIcon(QIcon(pm));
    gradientGtoRPB->setIconSize(QSize(24, 100));

    QHBoxLayout *gradientLayout = new QHBoxLayout;
    gradientLayout->addWidget(gradientBtoYPB);
    gradientLayout->addWidget(gradientGtoRPB);
    allGradient->setLayout(gradientLayout);

    // Row Range GroupBox
    rowSliders = new MySliders("Row");

    // Column Range GroupBox
    colSliders = new MySliders("Col");

    // Main Settings GroupBox
    function1RadioButton = new QRadioButton(tr("Function 1"));
    function1RadioButton->setChecked(true);
    function2RadioButton = new QRadioButton(tr("Function 2"));

    gridCheckBox = new QCheckBox(tr("Show Grid"));

    labelsCheckBox = new QCheckBox(tr("Show Labels"));

    labelBordersCheckBox = new QCheckBox(tr("Show Label Borders"));

    // Step GroupBox
    stepGroupBox = new QGroupBox(tr("Step"));
    stepXLineEdit = new QLineEdit;
    stepZLineEdit = new QLineEdit;

    stepLayout = new QFormLayout();
    stepLayout->addRow(tr("Step X:"), stepXLineEdit);
    stepLayout->addRow(tr("Step Z:"), stepZLineEdit);
    stepGroupBox->setLayout(stepLayout);

    // Selection Mode GroupBox
    selectionModeGroupBox = new QGroupBox(tr("Selection Mode"));
    noSelectionRadioButton = new QRadioButton(tr("No selection"));
    itemRadioButton = new QRadioButton(tr("Item"));
    QVBoxLayout *selectionModeLayout = new QVBoxLayout;
    selectionModeLayout->addWidget(noSelectionRadioButton);
    selectionModeLayout->addWidget(itemRadioButton);
    selectionModeGroupBox->setLayout(selectionModeLayout);

    // Main Settings GroupBox
    QVBoxLayout *settingsLayout = new QVBoxLayout;
    settingsLayout->addWidget(allGradient);
    settingsLayout->addWidget(rowSliders);
    settingsLayout->addWidget(colSliders);
    settingsLayout->addWidget(stepGroupBox);
    settingsLayout->addWidget(selectionModeGroupBox);
    settingsLayout->addWidget(gridCheckBox);
    settingsLayout->addWidget(labelsCheckBox);
    settingsLayout->addWidget(labelBordersCheckBox);
    settingsLayout->addWidget(function1RadioButton);
    settingsLayout->addWidget(function2RadioButton);

    settingsGroupBox->setLayout(settingsLayout);
    settingsGroupBox->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);

    // connecting
    modifier = new Plot(graph);
    QObject::connect(function1RadioButton, &QRadioButton::toggled, modifier, &Plot::fillSinc1);
    QObject::connect(function2RadioButton, &QRadioButton::toggled, modifier, &Plot::fillSinc2);
    QObject::connect(gridCheckBox, &QCheckBox::toggled, modifier, &Plot::showGrid);
    QObject::connect(labelBordersCheckBox, &QCheckBox::toggled, modifier, &Plot::showLabelborder);
    QObject::connect(labelsCheckBox, &QCheckBox::toggled, modifier, &Plot::VisibleCord);
    QObject::connect(gradientBtoYPB, &QPushButton::clicked, modifier, &Plot::ChahgeGradient1);
    QObject::connect(gradientGtoRPB, &QPushButton::clicked, modifier, &Plot::ChahgeGradient2);
    QObject::connect(stepXLineEdit, &QLineEdit::textChanged, modifier, &Plot::representSinc1X);
    QObject::connect(stepZLineEdit, &QLineEdit::textChanged, modifier, &Plot::representSinc1Z);
    QObject::connect(noSelectionRadioButton, &QRadioButton::toggled, modifier, &Plot::NotTakePoint);
    QObject::connect(itemRadioButton, &QRadioButton::toggled, modifier, &Plot::TakePoint);
    QObject::connect(saveAction, &QAction::triggered, this, &MainWindow::SaveSettings);
    QObject::connect(loadAction, &QAction::triggered, this, &MainWindow::LoadSettings);
    QObject::connect(rowSliders,
                     SIGNAL(rangeChanged(int, int)),
                     modifier,
                     SLOT(setRangeX(int, int)));
    QObject::connect(colSliders,
                     SIGNAL(rangeChanged(int, int)),
                     modifier,
                     SLOT(setRangeZ(int, int)));
    // QObject::connect(modifier->m_sqrtSinSeries, &QtDataVisualization::QSurface3DSeries::selectedPointChanged, modifier, &Plot::pointChanged);
    connect(russianAction, &QAction::triggered, this, &MainWindow::loadRussianTranslation);
    connect(englishAction, &QAction::triggered, this, &MainWindow::loadEnglishTranslation);
    QObject::connect(modifier->m_sqrtSinSeries,
                     &QtDataVisualization::QSurface3DSeries::selectedPointChanged,
                     this,
                     &MainWindow::pointChanged);
}

void MainWindow::loadRussianTranslation()
{
    modifier->is_russian = true;
    QString translationFilePath = "C:\\CLionprojects\\lab4_pro\\lab4_pro_ru_RU.qm";
    if (translator.load(translationFilePath)) {
        qApp->installTranslator(&translator);
        settingsGroupBox->setTitle(tr("Settings"));
        allGradient->setTitle(tr("Gradients"));
        rowSliders->setTitle(tr("Row Range"));
        colSliders->setTitle(tr("Column Range"));
        stepGroupBox->setTitle(tr("Step"));
        selectionModeGroupBox->setTitle(tr("Selection Mode"));
        fileMenu->setTitle(tr("&File"));

        gridCheckBox->setText(tr("Show Grid"));
        labelsCheckBox->setText(tr("Show Labels"));
        labelBordersCheckBox->setText(tr("Show Label Borders"));
        noSelectionRadioButton->setText(tr("No selection"));
        itemRadioButton->setText(tr("Item"));
        function1RadioButton->setText(tr("Function 1"));
        function2RadioButton->setText(tr("Function 2"));
        saveAction->setText(tr("&Save Settings"));
        loadAction->setText(tr("&Load Settings"));
    } else {
        QMessageBox::warning(this, tr("Error"), tr("Failed to load Rus translation file."));
    }
}

void MainWindow::loadEnglishTranslation()
{
    modifier->is_russian = false;
    QString translationFilePath = "C:\\CLionprojects\\lab4_pro\\untitled1_en_150.qm";
    if (translator.load(translationFilePath)) {
        qApp->installTranslator(&translator);
        settingsGroupBox->setTitle(tr("Settings"));
        allGradient->setTitle(tr("Gradients"));
        rowSliders->setTitle(tr("Row Range"));
        colSliders->setTitle(tr("Column Range"));
        stepGroupBox->setTitle(tr("Step"));
        selectionModeGroupBox->setTitle(tr("Selection Mode"));
        fileMenu->setTitle(tr("&File"));

        gridCheckBox->setText(tr("Show Grid"));
        labelsCheckBox->setText(tr("Show Labels"));
        labelBordersCheckBox->setText(tr("Show Label Borders"));
        noSelectionRadioButton->setText(tr("No selection"));
        itemRadioButton->setText(tr("Item"));
        function1RadioButton->setText(tr("Function 1"));
        function2RadioButton->setText(tr("Function 2"));
        saveAction->setText(tr("&Save Settings"));
        loadAction->setText(tr("&Load Settings"));

    } else {
        QMessageBox::warning(this, tr("Error"), tr("Failed to load Eng translation file."));
    }
}

void MainWindow::SaveSettings()
{
    QSettings settings("C:\\CLionprojects\\lab4_pro\\plot_settings.ini", QSettings::IniFormat);
    start = false;
    settings.setValue("Function", modifier->is_sinc1);
    settings.setValue("StepsX", modifier->stepsX);
    settings.setValue("StepsZ", modifier->stepsZ);
    settings.setValue("GridEnabled", modifier->m_graph->activeTheme()->isGridEnabled());
    settings.setValue("LabelVisible", modifier->m_labelsVisible);
    settings.setValue("LabelBorderEnabled",
                      modifier->m_graph->activeTheme()->isLabelBorderEnabled());
    settings.setValue("1Gradis1", modifier->is_1firstgr);
    settings.setValue("1Gradis2", modifier->is_2firstgr);
    settings.setValue("2Gradis1", modifier->is_2firstgr);
    settings.setValue("2Gradis2", modifier->is_2firstgr);
    settings.setValue("RowminEdit1", rowSliders->minEdit->text().toInt());
    settings.setValue("RowmaxEdit1", rowSliders->maxEdit->text().toInt());
    settings.setValue("RowminEdit2", colSliders->minEdit->text().toInt());
    settings.setValue("RowmaxEdit2", colSliders->maxEdit->text().toInt());

    settings.sync();
}

void MainWindow::LoadSettings()
{
    QSettings settings("C:\\CLionprojects\\lab4_pro\\plot_settings.ini", QSettings::IniFormat);

    modifier->is_sinc1 = settings.value("Function", true).toBool();
    modifier->stepsX = settings.value("StepsX", 100).toInt();
    modifier->stepsZ = settings.value("StepsZ", 100).toInt();
    modifier->m_graph->removeSeries(modifier->m_sqrtSinSeries);
    if (modifier->is_sinc1) {
        modifier->fillSinc1();
    } else {
        modifier->fillSinc2();
    }
    modifier->m_graph->addSeries(modifier->m_sqrtSinSeries);

    modifier->m_graph->activeTheme()->setGridEnabled(settings.value("GridEnabled", true).toBool());
    modifier->m_labelsVisible = settings.value("LabelVisible", true).toBool();
    modifier->m_graph->activeTheme()->setLabelBorderEnabled(
        settings.value("LabelBorderEnabled", false).toBool());

    if (modifier->m_labelsVisible) {
        modifier->m_graph->activeTheme()->setLabelTextColor(Qt::white);
    } else {
        modifier->m_graph->activeTheme()->setLabelTextColor(Qt::black);
    }

    modifier->is_1firstgr = settings.value("1Gradis1", false).toBool();
    modifier->is_2firstgr = settings.value("1Gradis2", false).toBool();
    modifier->is_2firstgr = settings.value("2Gradis1", false).toBool();
    modifier->is_2firstgr = settings.value("2Gradis2", false).toBool();
    modifier->is_russian = settings.value("Lang", false).toBool();

    if (modifier->is_1firstgr && modifier->is_sinc1) {
        modifier->ChahgeGradient1();
    } else if (modifier->is_2firstgr && modifier->is_sinc1) {
        modifier->ChahgeGradient2();
    } else if (modifier->is_1secondgr && !modifier->is_sinc1) {
        modifier->ChahgeGradient1();
    } else if (modifier->is_2secondgr && !modifier->is_sinc1) {
        modifier->ChahgeGradient2();
    }

    if (modifier->is_russian) {
        loadRussianTranslation();
    } else {
        loadEnglishTranslation();
    }

    int rowMinEdit1 = settings.value("RowminEdit1", 0).toInt();
    int rowMaxEdit1 = settings.value("RowmaxEdit1", 0).toInt();
    int colMinEdit1 = settings.value("ColminEdit1", 0).toInt();
    int colMaxEdit1 = settings.value("ColmaxEdit1", 0).toInt();

    if (modifier->is_sinc1) {
        function1RadioButton->setChecked(true);
    } else {
        function2RadioButton->setChecked(true);
    }

    if (settings.value("GridEnabled", true).toBool()) {
        gridCheckBox->setChecked(true);
    } else {
        gridCheckBox->setChecked(false);
    }

    if (modifier->m_labelsVisible) {
        labelsCheckBox->setChecked(true);
    } else {
        labelsCheckBox->setChecked(false);
    }

    if (settings.value("LabelBorderEnabled", false).toBool()) {
        labelBordersCheckBox->setChecked(true);
    } else {
        labelBordersCheckBox->setChecked(false);
    }
    stepXLineEdit->setText(QString::number(modifier->stepsX));
    stepZLineEdit->setText(QString::number(modifier->stepsZ));

    rowSliders->minEdit->setText(QString::number(rowMinEdit1));
    rowSliders->maxEdit->setText(QString::number(rowMaxEdit1));
    colSliders->minEdit->setText(QString::number(colMinEdit1));
    colSliders->minEdit->setText(QString::number(colMaxEdit1));

    //rowSliders->maxSliderChanged(rowMinEdit1);
    //rowSliders->minSliderChanged(rowMaxEdit1);
    //colSliders->maxSliderChanged(colMaxEdit1);
    //colSliders->minSliderChanged(colMinEdit1);
}

void MainWindow::pointChanged(const QPoint &pt)
{
    QString msg;
    if ((pt.x() >= 0) && (pt.y() >= 0)) {
        msg = "x=" + QString::number(modifier->m_sqrtSinSeries->dataProxy()->itemAt(pt)->x())
              + ", y= " + QString::number(modifier->m_sqrtSinSeries->dataProxy()->itemAt(pt)->y())
              + ", z= " + QString::number(modifier->m_sqrtSinSeries->dataProxy()->itemAt(pt)->z());
    } else {
        msg = "";
    }
    statusBar->showMessage(msg);
}
