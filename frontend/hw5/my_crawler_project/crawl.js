const linkRegex = new RegExp(/href="(http[^"]+)"/g);

class CrawlData {
    constructor(url, depth, content, links) {
        this.url = url;
        this.depth = depth;
        this.content = content;
        this.links = links;
    }
}

async function extract_links(html_text) {
    let matches = [...html_text.matchAll(linkRegex)];
    return matches.map(match => match[1]);  
}

async function myFetch(url, depth) {
    try {
        let get = await fetch(url)
        if (!get.ok) {
            return new CrawlData(url, depth, "", []);
        }
        let response = await get.text()
        let links = await extract_links(response)
        return new CrawlData(url, depth, response, links)
    } catch(error) {
        return new CrawlData(url, depth, "", [])
    }
}

async function pool(tasks, limit) {
    const executing = new Set();
    const results = new Array(tasks.length);

    for (let i = 0; i < tasks.length; i++) {
        const e = Promise.resolve().then(() => tasks[i]()).then(
            result => {
                results[i] = result;
            },
        );
        executing.add(e);
        e.finally(() => executing.delete(e));
        if (executing.size >= limit) {
            await Promise.race(executing);
        }
    }
    await Promise.all(executing);
    return results;
}


async function mydownload(depth, concurrency, curLevel, ans, visited) {
    for (let curDepth = 1; curDepth <= depth; curDepth++){
        if (curLevel.size === 0) break; 

        const tasks = [];
        const nextLevel = new Set();

        for (let crawlData of curLevel) {
            tasks.push(() => myFetch(crawlData.url, crawlData.depth));
        }

        let curLevelFecthed = await pool(tasks, concurrency);

        for (let get of curLevelFecthed){
            ans.push(get)
            for (let link of get.links){
                if (!visited.has(link)){
                    visited.add(link)
                    nextLevel.add(new CrawlData(link, get.depth + 1, "", []));
                }
            }
        }

        curLevel.clear();
        nextLevel.forEach(item => curLevel.add(item));
    }
}

async function crawl(url, depth, concurrency) {
    const curLevel = new Set();
    const ans = [];
    const visited = new Set();
    curLevel.add(new CrawlData(url, 1, "", []));
    visited.add(url);
    await mydownload(depth, concurrency, curLevel, ans, visited);
    return ans;
}

module.exports = crawl;