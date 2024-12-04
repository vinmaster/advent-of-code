function inspect(o)
    if type(o) == 'table' then
        local s = '{ '
        for k, v in pairs(o) do
            if type(k) ~= 'number' then
                k = '"' .. k .. '"'
            end
            s = s .. '[' .. k .. '] = ' .. inspect(v) .. ','
        end
        return s .. '} '
    else
        return tostring(o)
    end
end

function file_exists(file)
    local f = io.open(file, 'rb')
    if f then
        f:close()
    end
    return f ~= nil
end

function script_path()
    local str = debug.getinfo(2, "S").source:sub(2)
    return str:match("(.*/)")
end

function read_file(file)
    if not file_exists(file) then
        return nil
    end
    local lines = ''
    for line in io.lines(file) do
        lines = lines .. line .. '\n'
    end
    return lines
end

local function split(str, sep)
    local result = {}
    local regex = ("([^%s]+)"):format(sep)
    for each in str:gmatch(regex) do
        table.insert(result, each)
    end
    return result
end

function trim(s)
    return s:match( "^%s*(.-)%s*$" )
end

function frequencies(list)
    local result = {}
    for i, x in pairs(list) do
        if (result[x]) then
            result[x] = result[x] + 1
        else
            result[x] = 1
        end
    end
    return result
end

function part1(input)
    local lines = split(trim(input), '\n');
    -- print(inspect(lines))
    local list1 = {}
    local list2 = {}
    local sum = 0
    for i, line in pairs(lines) do
        local matches = split(line, '   ')
        table.insert(list1, tonumber(matches[1]))
        table.insert(list2, tonumber(matches[2]))
    end
    table.sort(list1)
    table.sort(list2)
    for i, n in pairs(list1) do
        local distance = math.abs(n - list2[i])
        sum = sum + distance
    end
    return sum
end

function part2(input)
    local lines = split(trim(input), '\n');
    local list1 = {}
    local list2 = {}
    local sum = 0
    for i, line in pairs(lines) do
        local matches = split(line, '   ')
        table.insert(list1, tonumber(matches[1]))
        table.insert(list2, tonumber(matches[2]))
    end

    local freq = frequencies(list2)
    for i, n in pairs(list1) do
        local score = n * (freq[n] and freq[n] or 0)
        sum = sum + score
    end
    return sum
end

local input = read_file(script_path() .. 'input.txt')
-- input = [[
-- 3   4
-- 4   3
-- 2   5
-- 1   3
-- 3   9
-- 3   3
-- ]]

print('part1: ' .. part1(input))
print('part2: ' .. part2(input))
