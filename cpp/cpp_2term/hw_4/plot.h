#ifndef PLOT_H
#define PLOT_H

#include <QMainWindow>
#include <QStatusBar>
#include <QWidget>
#include <QtDataVisualization/Q3DSurface>
#include <QtDataVisualization/QHeightMapSurfaceDataProxy>
#include <QtDataVisualization/QSurface3DSeries>
#include <QtDataVisualization/QSurfaceDataProxy>
#include <QtWidgets/QSlider>

using namespace QtDataVisualization;

class Plot : public QWidget
{
    Q_OBJECT
public:
    explicit Plot(QtDataVisualization::Q3DSurface *surface);
    ~Plot();
    void enableSqrtSinModel(bool enable);
    void showGrid();
    void showLabelborder();
    void VisibleCord();
    void ChahgeGradient1();
    void ChahgeGradient2();
    void updateStatusBar(QStatusBar *statusBar, const QVector3D &position);
    void representSinc1X(const QString &newSteps);
    void fillSinc1();
    void fillSinc2();
    void ChangeSlider();
    void NotTakePoint();
    void TakePoint();
    void representSinc1Z(const QString &newSteps);
    bool is_sinc1;
    QSurface3DSeries *m_sqrtSinSeries;
    bool is_russian = false;
    QLinearGradient m_gradient1;
    QLinearGradient m_gradient2;
    Q3DSurface *m_graph;
    QSurfaceDataProxy *m_sqrtSinProxy;
    QSlider *minRow;
    QSlider *maxRow;
    QSlider *minColumn;
    QSlider *maxColumn;
    bool m_labelsVisible = false;

    QValue3DAxis *m_axisX;
    QValue3DAxis *m_axisY;
    QValue3DAxis *m_axisZ;

    bool m_showGrid;
    bool m_showLabels;
    bool m_showLabelBorders;
    bool is_1firstgr = false;
    bool is_1secondgr = false;

    bool is_2firstgr = false;
    bool is_2secondgr = false;

    float m_rangeMinX;
    float m_rangeMinZ;
    float m_stepX;
    float m_stepZ;

    int stepsZ = 50;
    int stepsX = 50;

public slots:
    void setRangeX(int min, int max);
    void setRangeZ(int min, int max);

private:
};

#endif // PLOT_H
