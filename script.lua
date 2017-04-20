_HASKELLERR = {}

function catch_haskell(ret, err_msg)
    if ret == _HASKELLERR then
      print("Error caught from Haskell land: " .. err_msg)
      return
    end
    return ret
end

local sum = 0
for k, v in pairs(vars)
do
  print(k, v)
  sum = sum + tonumber(vars[k])
end
print("")

catch_haskell(newData("d3", sum))
catch_haskell(newData("d2", 42))
