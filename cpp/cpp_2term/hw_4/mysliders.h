#ifndef MYSLIDERS_H
#define MYSLIDERS_H

#include <QBoxLayout>
#include <QGroupBox>
#include <QLineEdit>
#include <QSlider>

class MySliders : public QGroupBox
{
    Q_OBJECT
public:
    explicit MySliders(const QString &label, QWidget *parent = nullptr);
    QSlider *min;
    QSlider *max;
    QLineEdit *minEdit;
    QLineEdit *maxEdit;

public slots:
    void minSliderChanged(int val);
    void maxSliderChanged(int val);
    void minEdited(const QString &str);
    void maxEdited(const QString &str);

signals:
    void rangeChanged(int, int);
};

#endif // MYSLIDERS_H
