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

## Setup script 

```bash
# Download input (Requires config.json with SESSION_ID in it)
# node setup.js YEAR DAY
node setup.js 2018 1
# Download input and copy .js file from previous day
node setup.js -f js 1
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
