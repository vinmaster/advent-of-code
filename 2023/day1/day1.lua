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

function part1(input)
    local lines = split(input, '\n');
    local sum = 0
    for i, line in pairs(lines) do
        local nums = {}
        for num in string.gmatch(line, '(%d)') do
            table.insert(nums, num)
        end
        sum = sum + tonumber(nums[1] .. nums[#nums])
    end
    -- print(inspect(sum))
    return sum
end

local input = read_file(script_path() .. 'input.txt')
-- input = '1abc22\n' .. 'pqr3stu8vwx\n' .. 'a1b2c3d4e5f\n' .. 'treb7uchet'

print('part1: ' .. part1(input))
