_HASKELLERR = {}

function catch_haskell(ret, err_msg)
    if ret == _HASKELLERR then
      print("Error caught from Haskell land: " .. err_msg)
      return
    end
    return ret
end

-- Print contents of `tbl`, with indentation.
-- `indent` sets the initial level of indentation.
function tprint (tbl, indent)
  if not indent then indent = 0 end
  for k, v in pairs(tbl) do
    formatting = string.rep("  ", indent) .. k .. ": "
    if type(v) == "table" then
      print(formatting)
      tprint(v, indent+1)
    elseif type(v) == 'boolean' then
      print(formatting .. tostring(v))
    else
      print(formatting .. v)
    end
  end
end

-- print("SCRIPT --------------------")
-- tprint(vars)

local sum = 0

for k, v in pairs(vars)
do
  sum = sum + tonumber(vars[k])
end

catch_haskell(hello("sum: " .. sum))
