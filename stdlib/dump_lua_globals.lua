local seen = {}

local result = {}

local function dump2(t, path, res)
    seen[t] = path
    if path ~= "" then
        path = path.."."
    end
    for k,v in pairs(t) do
        k = tostring(k)
        if path ~= "" or k ~= "package" then
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
    end
end

local json = require "cjson"

result = { __kind = "raw", _type = "table", _name = "" }
dump2(_G, "", result)

print(json.encode(result))

