package info.kgeorgiy.ja.korshunov.i18n.application;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@Retention(RetentionPolicy.RUNTIME)
public @interface StatType {
    TypeStat value();
}
