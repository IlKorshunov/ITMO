package info.kgeorgiy.ja.korshunov.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.*;

public class WebCrawler implements NewCrawler {



    public static void main(String[] args) {
        String url = args[0];
        int downloaders = Integer.parseInt(args[1]);
        int depth = Integer.parseInt(args[2]);
        int extractors = Integer.parseInt(args[3]);
        int perHost = Integer.parseInt(args[4]);

        try  {
            Downloader downloader = new CachingDownloader(1);
            WebCrawler crawler = new WebCrawler(downloader, downloaders, extractors, perHost);
            Result result = crawler.download(url, depth);
            crawler.close();
            for (String get : result.getDownloaded()){
                System.out.println(get);
            }
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
    }

    private final Downloader downloader;
    private final ExecutorService downloaders;
    private final ExecutorService extractors;
    private final int perHost;
    private final Semaphore globalDownloadLimiter;

    public WebCrawler(Downloader downloader, int downloaders, int extractors, int perHost) {
        this.downloader = downloader;
        this.downloaders = Executors.newFixedThreadPool(downloaders);
        this.extractors = Executors.newFixedThreadPool(extractors);
        this.perHost = perHost;
        this.globalDownloadLimiter = new Semaphore(downloaders);
    }

    @Override
    public Result download(String startuUrl, int depth, Set<String> exclude) {
        Phaser phaser = new Phaser(1);
        ConcurrentMap<String, IOException> errors = new ConcurrentHashMap<>();
        Set<String> visited = ConcurrentHashMap.newKeySet();
        Set<String> invalid = ConcurrentHashMap.newKeySet();
        Set<String> curLevel =  ConcurrentHashMap.newKeySet();
        ConcurrentMap<String, Semaphore> load = new ConcurrentHashMap<>();
        curLevel.add(startuUrl);
        for (int i = depth; i >= 1; i--){
            Set<String> nextLevel = ConcurrentHashMap.newKeySet();
            for (String curUrl : curLevel) {
                if (exclude.stream().anyMatch(curUrl::contains)){
                    invalid.add(curUrl);
                    continue;
                }
                phaser.register();
                myDownload(curUrl, nextLevel, visited, invalid, errors, phaser, i, depth, load, exclude);
            }
            phaser.arriveAndAwaitAdvance();
            curLevel = nextLevel;
        }
        visited.removeAll(invalid);
        return new Result(List.copyOf(visited), errors);
    }

//    @Override
//    public Result download(String url, int depth) {
//        Phaser phaser = new Phaser(1);
//        ConcurrentMap<String, IOException> errors = new ConcurrentHashMap<>();
//        Set<String> visited = ConcurrentHashMap.newKeySet();
//        Set<String> invalid = ConcurrentHashMap.newKeySet();
//        Set<String> curLevel =  ConcurrentHashMap.newKeySet();
//        ConcurrentMap<String, Semaphore> load = new ConcurrentHashMap<>();
//        curLevel.add(url);
//        for (int i = depth; i >= 1; i--){
//            Set<String> nextLevel = ConcurrentHashMap.newKeySet();
//            for (String curUrl : curLevel) {
//                phaser.register();
//                myDownload(curUrl, nextLevel, visited, invalid, errors, phaser, i, depth, load);
//            }
//            phaser.arriveAndAwaitAdvance();
//            curLevel = nextLevel;
//        }
//        visited.removeAll(invalid);
//        return new Result(List.copyOf(visited), errors);
//    }

    private void myDownload(String curUrl, Set<String> nextLevel, Set<String> visited, Set<String> invalid, ConcurrentMap<String,
            IOException> errors, Phaser phaser, int curDepth, int depth,
                            ConcurrentMap<String, Semaphore> load, Set<String> exclude) {
        downloaders.execute(() -> {
            try {
//                String host = URLUtils.getHost(curUrl);
//                Semaphore semaphore = load.computeIfAbsent(host, h -> new Semaphore(perHost, true));
//                globalDownloadLimiter.acquire();
//                semaphore.acquire();

                Document document = downloader.download(curUrl);
                visited.add(curUrl);
                //semaphore.release();
                if (curDepth != 1){
                    phaser.register();
                    extractors.execute(() -> {
                        try {
                            List<String> links = document.extractLinks();
                            for (String link : links) {
                                if (visited.add(link)){
                                    nextLevel.add(link);
                                }
                            }
                        } catch (IOException e) {
                            invalid.add(curUrl);
                            errors.put(curUrl, e);
                        } finally {
                            phaser.arriveAndDeregister();
                        }
                    });
                }
            } catch (IOException e) {
                invalid.add(curUrl);
                errors.put(curUrl, e);
//            } catch (InterruptedException e) {
//                invalid.add(curUrl);
            } finally {
               // globalDownloadLimiter.release();
                phaser.arriveAndDeregister();
            }
        });

    }

    @Override
    public void close() {
        downloaders.shutdown();
        extractors.shutdown();
    }
}
