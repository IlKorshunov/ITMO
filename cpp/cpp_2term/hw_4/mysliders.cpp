#include "mysliders.h"

MySliders::MySliders(const QString &label, QWidget *parent)
    : QGroupBox(label, parent)
{
    min = new QSlider(Qt::Horizontal, this);
    max = new QSlider(Qt::Horizontal, this);
    minEdit = new QLineEdit(this);
    maxEdit = new QLineEdit(this);

    QVBoxLayout *l = new QVBoxLayout(this);
    l->addWidget(min);
    l->addWidget(minEdit);
    l->addWidget(max);
    l->addWidget(maxEdit);

    QObject::connect(min, SIGNAL(valueChanged(int)), this, SLOT(minSliderChanged(int)));
    QObject::connect(max, SIGNAL(valueChanged(int)), this, SLOT(maxSliderChanged(int)));
    QObject::connect(minEdit,
                     SIGNAL(textEdited(const QString &)),
                     this,
                     SLOT(minEdited(const QString &)));
    QObject::connect(maxEdit,
                     SIGNAL(textEdited(const QString &)),
                     this,
                     SLOT(maxEdited(const QString &)));

    min->setRange(-10, 10);
    min->setValue(-10);
    max->setRange(-10, 10);
    max->setValue(10);
}

void MySliders::minSliderChanged(int val)
{
    if (val == min->maximum()) {
        min->setValue(--val);
    }
    if (max->value() < val + 1) {
        max->setValue(val + 1);
    }
    minEdit->setText(QString::number(val));
    emit rangeChanged(val, max->value());
}

void MySliders::maxSliderChanged(int val)
{
    if (val == max->minimum()) {
        max->setValue(++val);
    }
    if (min->value() > val - 1) {
        min->setValue(val - 1);
    }
    maxEdit->setText(QString::number(val));
    emit rangeChanged(min->value(), val);
}

void MySliders::minEdited(const QString &str)
{
    min->setValue(str.toInt());
}

void MySliders::maxEdited(const QString &str)
{
    max->setValue(str.toInt());
}
