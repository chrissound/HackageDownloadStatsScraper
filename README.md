# ArchLinuxPkgStatsScraper
A project to scrape the contents of https://www.archlinux.de/?page=PackageStatistics as there is no API

http://trycatchchris.co.uk/entry/arch-linux-pkgstats-json-scraper-in-haskell

```
stack build
stack exec ArchPkgstatsScraper
```

The output is saved to packageStatistics.json
