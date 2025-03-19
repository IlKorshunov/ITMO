package info.kgeorgiy.ja.korshunov.i18n.application;

import info.kgeorgiy.ja.korshunov.i18n.statistics.*;
import java.util.*;
import java.util.concurrent.*;

import static info.kgeorgiy.ja.korshunov.i18n.statistics.AbstractStats.getBundle;

public class AllStat {
    List<AbstractStats<?, ?, ?>> allObjects;
    private final Locale outputLocale;
    Map<TypeStat, String> responces;
    Map<TypeStat, Integer> count;
    public AllStat(Locale input, Locale output, String text) {
        this.outputLocale = output;
        allObjects = new ArrayList<>();
        allObjects.addAll(StatsFactory.getObjects(input, output, text));
        responces = new HashMap<>();
        count = new HashMap<>();
    }

    private void execReport() {
        Phaser phaser = new Phaser(1);
        ExecutorService executor = Executors.newFixedThreadPool(allObjects.size());
        List<Future<Map.Entry<TypeStat, String>>> futures = new ArrayList<>();

        for (AbstractStats<?, ?, ?> curStat : allObjects) {
            phaser.register();
            Callable<Map.Entry<TypeStat, String>> task = () -> {
                try {
                    phaser.arriveAndAwaitAdvance();
                    String report = curStat.getReport();
                    StatType annotation = curStat.getClass().getAnnotation(StatType.class);
                    if (annotation != null) {
                        TypeStat typeStat = annotation.value();
                        count.put(typeStat, curStat.getCount());
                        return new AbstractMap.SimpleEntry<>(typeStat, report);
                    } else {
                        throw new RuntimeException("No annotation");
                    }
                } finally {
                    phaser.arriveAndDeregister();
                }
            };
            futures.add(executor.submit(task));
        }
        phaser.arriveAndDeregister();



        for (Future<Map.Entry<TypeStat, String>> future : futures) {
            try {
                Map.Entry<TypeStat, String> result = future.get();
                responces.put(result.getKey(), result.getValue());
            } catch (InterruptedException | ExecutionException e) {
                e.printStackTrace(); // надо потом продумать, как обработать эту дуру
            }
        }

        executor.shutdown();
        try {
            if (!executor.awaitTermination(5, TimeUnit.SECONDS)) {
                executor.shutdownNow();
            }
        } catch (InterruptedException e) {
            executor.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }


    public String getReport() {
        ResourceBundle curBundle = getBundle(outputLocale);
        execReport();
        String out = String.format(outputLocale,
                """
                        %s \s
                            %s : %d.\s
                            %s : %d.\s
                            %s : %d.\s
                            %s : %d.\s
                            %s : %d.\s
                        """,
                curBundle.getString("summaryStatistics"),
                curBundle.getString("numberSENTENCE"), count.get(TypeStat.SENTENCE),
                curBundle.getString("numberWORD"), count.get(TypeStat.WORD),
                curBundle.getString("numberNUMBER"), count.get(TypeStat.NUMBER),
                curBundle.getString("numberMONEY"), count.get(TypeStat.MONEY),
                curBundle.getString("numberDATA"), count.get(TypeStat.DATA)
        ) ;
        out += responces.get(TypeStat.SENTENCE) +
        responces.get(TypeStat.WORD) +
        responces.get(TypeStat.NUMBER) +
        responces.get(TypeStat.MONEY) +
        responces.get(TypeStat.DATA);
        return out;
    }

}
