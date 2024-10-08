-- Premable

local function _push(stack, n)
  -- print(">", #stack, n)
  table.insert(stack, n)
end

local function _pop(stack)
  local n = table.remove(stack)
  -- print("<", #stack, n)
  return n
end

local function pop(stack)
  _pop(stack)
end

local function _print(stack)
  print(_pop(stack))
  _push(stack, "unit")
end

local function add(stack)
  _push(stack, _pop(stack) + _pop(stack))
end

local function is_odd(stack)
  _push(stack, _pop(stack) % 2 == 1)
end

-- Premable
