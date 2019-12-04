# Advent of Code
> Solutions to Advent of Code website

## Commands

```bash
# Download input (Requires config.json with SESSION_ID in it)
# node setup.js YEAR DAY
node setup.js 2018 1
```

### JavaScript
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

### Haskell
```bash
# Compile
stack ghc 2019/day1/day1.hs
# Run compiled
2019/day1/day1.exe
# Run without compiling
stack runghc -- main.hs 2019 1
# Run file directly
stack runghc -- 2019\day1\day1.hs
```

### Ruby
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

## Files
> Files for each language. The rest are inside `.gitignore`

### JavaScript
- package.json
- [main.js](./main.js)

### Ruby
- [main.rb](./main.rb)

## Website
- https://adventofcode.com
