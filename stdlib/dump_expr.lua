local seen = {}

local result = {}

local dump2

local function dump1(k, v, path, res)
    if type(v) == "table" then
        if seen[v] then
            if not res[k] and seen[v] ~= true then
                res[k] = { __kind = "rec", path = seen[v] }
            end
        else
            if not res[k] then
                res[k] = {}
            end
            res[k].__kind = "raw"
            res[k]._type = "table"
            res[k]._name = path..k
            dump2(v, path..k, res[k])
        end
    elseif type(v) == "function" then
        local info = debug.getinfo(v)
        res[k] = {
            __kind = "raw",
            _name = path..k,
            _type = "function",
            _minArity = info.nparams
        }
        if not info.isvararg then
            res[k]["_maxArity"] = info.nparams
        end
    else
        res[k] = {
            __kind = "raw", _name = path..k, _type = type(v)
        }
    end
end

dump2 = function(t, path, res)
    seen[t] = path
    if path ~= "" then
        path = path.."."
    end
    for k,v in pairs(t) do
        k = tostring(k)
        if path ~= "" or k ~= "package" then
            dump1(k, v, path, res)
        end
    end
end

local json = require "cjson"

local expr = @expr@

dump1("", expr, "", result)

print(json.encode(result[""]))

