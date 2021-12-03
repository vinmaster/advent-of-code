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
bb mainbb.clj 2021 1
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

## Following

https://github.com/norvig/pytudes/tree/master/ipynb
https://kufii.github.io/advent-of-code-2020/#/18

- Clojure
  - https://github.com/dawranliou/advent-of-code
  - https://github.com/tschady/advent-of-code
  - https://github.com/transducer/adventofcode
  - https://www.youtube.com/channel/UC9m7D4XKPJqTPCLSBym3BCg/videos
  - https://www.youtube.com/user/mzamansky/videos
  - https://www.youtube.com/c/LambdaIsland/videos
{
  "key": "ctrl+enter",
  "command": "clojureVSCode.evalAndShowResult",
  "when": "editorTextFocus && !editorReadonly"
}

- JS/TypeScript
  - https://github.com/RikKierkels/advent-of-code-2020
  - https://github.com/MauricioLudwig/advent-of-code
  - https://github.com/tristanbeedell/Advent-of-Code
  - https://github.com/AlexAegis/advent-of-code
  - https://github.com/adhokshaja/AdventOfCode2020
  - https://github.com/sguest/advent-of-code
  - https://github.com/smrq/advent-of-code
  - https://github.com/tpatel/advent-of-code-2020 with youtube

chokidar -p true --polling-interval 1000 "20*/**/*" -c "node main.js 2019 11 | tee output.txt"