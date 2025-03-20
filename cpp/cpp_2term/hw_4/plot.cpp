#include "mainwindow.h"
#include "plot.h"
#include <QCoreApplication>
#include <QMainWindow>
#include <QMessageBox>
#include <QSettings>
#include <QStatusBar>
#include <QtCore/qmath.h>
#include <QtDataVisualization/Q3DTheme>
#include <QtDataVisualization/QValue3DAxis>
#include <QtGui/QImage>

using namespace QtDataVisualization;
const float sampleMin = -10.0f;
const float sampleMax = 10.0f;

Plot::Plot(Q3DSurface *surface)
    : m_graph(surface)
{
    m_axisX = new QValue3DAxis;
    m_axisY = new QValue3DAxis;
    m_axisZ = new QValue3DAxis;
    m_graph->setAxisX(m_axisX);
    m_graph->setAxisY(m_axisY);
    m_graph->setAxisZ(m_axisZ);

    m_labelsVisible = true;
    m_sqrtSinProxy = new QSurfaceDataProxy();
    m_sqrtSinSeries = new QSurface3DSeries(m_sqrtSinProxy);
    fillSinc1();

    // QObject::connect(m_sqrtSinSeries, &QtDataVisualization::QSurface3DSeries::selectedPointChanged, this, &Plot::pointChanged);
}

Plot::~Plot()
{
    delete m_graph;
}

void Plot::fillSinc1()
{
    is_sinc1 = true;
    float stepX = (sampleMax - sampleMin) / float(stepsX);
    float stepZ = (sampleMax - sampleMin) / float(stepsZ);

    QSurfaceDataArray *dataArray = new QSurfaceDataArray;
    dataArray->reserve(stepsZ);
    for (int i = 0; i < stepsZ; i++) {
        QSurfaceDataRow *newRow = new QSurfaceDataRow(stepsX);
        float z = qMin(sampleMax, (i * stepZ + sampleMin));
        int index = 0;
        for (int j = 0; j < stepsX; j++) {
            float x = qMin(sampleMax, (j * stepX + sampleMin));
            float R = qSqrt(z * z + x * x) + 0.01f;
            float y = (qSin(R) / R + 0.24f) * 1.61f;
            (*newRow)[index++].setPosition(QVector3D(x, y, z));
        }
        *dataArray << newRow;
    }

    m_sqrtSinProxy->resetArray(dataArray);
    m_graph->addSeries(m_sqrtSinSeries);
    if (is_1firstgr) {
        ChahgeGradient1();
    }
    if (is_1secondgr) {
        ChahgeGradient2();
    }
}

void Plot::fillSinc2()
{
    is_sinc1 = false;
    float stepX = (sampleMax - sampleMin) / float(stepsX);
    float stepZ = (sampleMax - sampleMin) / float(stepsZ);

    QSurfaceDataArray *dataArray = new QSurfaceDataArray;
    dataArray->reserve(stepsZ);
    for (int i = 0; i < stepsZ; i++) {
        QSurfaceDataRow *newRow = new QSurfaceDataRow(stepsX);
        float z = qMin(sampleMax, (i * stepZ + sampleMin));
        int index = 0;
        for (int j = 0; j < stepsX; j++) {
            float x = qMin(sampleMax, (j * stepX + sampleMin));
            float sincX = qSin(x) / x;
            float sincZ = qSin(z) / z;
            if (x == 0) {
                sincX = 1;
            }
            if (z == 0) {
                sincZ = 1;
            }
            float value = sincX * sincZ;
            (*newRow)[index++].setPosition(QVector3D(x, value, z));
        }
        *dataArray << newRow;
    }
    m_sqrtSinProxy->resetArray(dataArray);
    m_graph->addSeries(m_sqrtSinSeries);
    if (is_2firstgr) {
        ChahgeGradient1();
    }
    if (is_2secondgr) {
        ChahgeGradient2();
    }
}

void Plot::showGrid()
{
    m_graph->activeTheme()->setGridEnabled(!m_graph->activeTheme()->isGridEnabled());
}

void Plot::showLabelborder()
{
    m_graph->activeTheme()->setLabelBorderEnabled(!m_graph->activeTheme()->isLabelBorderEnabled());
}

void Plot::VisibleCord()
{
    m_labelsVisible = !m_labelsVisible;

    if (m_labelsVisible) {
        m_graph->activeTheme()->setLabelTextColor(Qt::white);
    } else {
        m_graph->activeTheme()->setLabelTextColor(Qt::black);
    }
}

void Plot::ChahgeGradient1()
{
    if (is_sinc1) {
        is_1firstgr = true;
        is_1secondgr = false;
    } else {
        is_2firstgr = true;
        is_2secondgr = false;
    }
    m_gradient1 = QLinearGradient(0, 0, 1, 100);
    m_gradient1.setStops({{1.0, Qt::black}, {0.67, Qt::blue}, {0.33, Qt::red}, {0.0, Qt::yellow}});
    m_graph->seriesList().at(0)->setBaseGradient(m_gradient1);
    //m_graph->seriesList().at(0)->setColorStyle(Q3DTheme::ColorStyleRangeGradient);
}

void Plot::ChahgeGradient2()
{
    if (!is_sinc1) {
        is_2firstgr = false;
        is_2secondgr = true;
    } else {
        is_1firstgr = false;
        is_1secondgr = true;
    }
    m_gradient2 = QLinearGradient(0, 0, 1, 100);
    m_gradient2.setStops(
        {{1.0, Qt::darkGreen}, {0.5, Qt::yellow}, {0.25, Qt::red}, {0.0, Qt::darkRed}});
    m_graph->seriesList().at(0)->setBaseGradient(m_gradient2);
    m_graph->seriesList().at(0)->setColorStyle(Q3DTheme::ColorStyleRangeGradient);
}

void Plot::representSinc1X(const QString &newSteps)
{
    int number = newSteps.toInt();
    if (newSteps != "" && number > 2) {
        if (number > 100) {
            stepsX = 100;
        } else {
            stepsX = number;
        }
        m_graph->removeSeries(m_sqrtSinSeries);
        if (is_sinc1) {
            fillSinc1();
        } else {
            fillSinc2();
        }
        m_graph->addSeries(m_sqrtSinSeries);
    }
}

void Plot::representSinc1Z(const QString &newSteps)
{
    int number = newSteps.toInt();
    if (newSteps != "" && number > 2) {
        if (number > 100) {
            stepsZ = 100;
        } else {
            stepsZ = number;
        }
        m_graph->removeSeries(m_sqrtSinSeries);
        if (is_sinc1) {
            fillSinc1();
        } else {
            fillSinc2();
        }
        m_graph->addSeries(m_sqrtSinSeries);
    }
}

void Plot::setRangeX(int min, int max)
{
    m_graph->axisX()->setRange(min, max);
}

void Plot::setRangeZ(int min, int max)
{
    m_graph->axisZ()->setRange(min, max);
}

void Plot::ChangeSlider() {}

void Plot::NotTakePoint()
{
    m_graph->setSelectionMode(QAbstract3DGraph::SelectionNone);
}

void Plot::TakePoint()
{
    m_graph->setSelectionMode(QAbstract3DGraph::SelectionItem);
}
