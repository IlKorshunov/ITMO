global.fetch = require('node-fetch');

const crawl = require('./crawl');

(async () => {
    const startingUrl = 'https://example.com/page';
    const depth = 5;
    const concurrency = 3;

    const result = await crawl(startingUrl, depth, concurrency);
    console.log(result);
})();