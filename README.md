# Advent of Code
> Solutions to Advent of Code website

```
              _                 _            __    _____          _      
     /\      | |               | |          / _|  / ____|        | |     
    /  \   __| |_   _____ _ __ | |_    ___ | |_  | |     ___   __| | ___ 
   / /\ \ / _` \ \ / / _ \ '_ \| __|  / _ \|  _| | |    / _ \ / _` |/ _ \
  / ____ \ (_| |\ V /  __/ | | | |_  | (_) | |   | |___| (_) | (_| |  __/
 /_/    \_\__,_| \_/ \___|_| |_|\__|  \___/|_|    \_____\___/ \__,_|\___|
```

## Website
- https://adventofcode.com

## CLI script
### config.json
Fill out config.json using config.json.sample. Optional user-agent can be added due to this: [link](https://www.reddit.com/r/adventofcode/comments/z9dhtd/please_include_your_contact_info_in_the_useragent/)

```bash
# Download input (Requires config.json with SESSION_ID in it)
# node cli.js download -y YEAR -d DAY
node cli.js download -d 1
# Copy .js file from previous day
node cli.js copy js -d 1
# Print help
node cli.js help
```

## My workflow
```bash
chokidar -p true --polling-interval 1000 "20*/**/*" -c "node main.js 2019 11 | tee output.txt"
```

## JavaScript
```bash
# Run one year solutions
# node main.js YEAR
node main.js 2018
# Run one day solutions
# node main.js YEAR DAY
node main.js 2018 1
# Run one day solutions using current year
# node main.js DAY
node main.js 1
```

## Haskell
```bash
# Compile
stack ghc 2019/day1/day1.hs
# Run compiled
2019/day1/day1.exe
# Run without compiling
stack runghc -- main.hs 2019 1
# Run file directly
stack runghc -- 2019/day1/day1.hs
```

## Clojure
(Requires inlein)
```bash
# Run one day solutions
# inlein main.clj YEAR DAY
inlein main.clj 2019 1
# Run one day solutions for current year
# inlein main.clj DAY
inlein main.clj 1
```

(Requires Babashka)
```bash
# Run one day solutions
bb mainbb.clj 2021 1
# Run one day solutions for current year
bb mainbb.clj 1
```

## Ruby
```bash
# Run one year solutions
# ruby main.rb YEAR
ruby main.rb 2018
# Run one day solutions
# ruby main.rb YEAR DAY
ruby main.rb 2018 1
# Run one day solutions using current year
# ruby main.rb DAY
ruby main.rb 1
```

## Rust
(Requires rust-script)
```bash
# Run one day solutions
./main.rs 2022 1
# Run one day solutions using current year
./main.rs 1
```
