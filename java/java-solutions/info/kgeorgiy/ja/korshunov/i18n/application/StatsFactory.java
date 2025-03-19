package info.kgeorgiy.ja.korshunov.i18n.application;

import info.kgeorgiy.ja.korshunov.i18n.statistics.*;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.*;
import java.util.stream.Collectors;

public class StatsFactory {
    public static Map<TypeStat, AbstractStats<?, ?, ?>> MapFabric = new HashMap<>();

    private static final List<Class<? extends AbstractStats<?, ?, ?>>> classes = List.of(
            Dates.class, Money.class, Numbers.class, Sentences.class, Words.class
    );

    public static List<AbstractStats<?, ?, ?>> getObjects(Locale a, Locale b, String text) {
        return classes.stream().map(c -> {
            try {
                return (AbstractStats<?, ?, ?>) c.getConstructor(Locale.class, Locale.class, String.class).newInstance(a, b, text);
            } catch (InstantiationException | IllegalAccessException | InvocationTargetException |
                     NoSuchMethodException e) {
                throw new RuntimeException(e.getMessage());
            }
        }).collect(Collectors.toList());
    }

    public static void AddNewType(TypeStat type, AbstractStats<?, ?, ?> object) {
        MapFabric.put(type, object);
    }

    public static AbstractStats<?, ?, ?> GetObjectMyType(TypeStat type) {
        return MapFabric.get(type);
    }
}
