-- some helper functions over the drive component --
-- yes, this makes use of some OOP --

local component = component or require("component")

local drive = {}

local _base = {}

function _base:readSector(n)
  checkArg(1, n, "number")
  if n < self._start or n > self._end then
    return nil, "cannot read outside of allocated sector range"
  end
  return self.node.readSector(n)
end

function _base:readSectorRange(start, len)
  checkArg(1, start, "number")
  checkArg(2, len, "number")
  local ret = ""
  for i=start, start+len, 1 do
    local dat = self:readSector(i)
    ret = ret .. (dat or "")
  end
end

function _base:writeSector(n, dat)
  checkArg(1, n, "number")
  checkArg(2, dat, "string")
  if n < self._start or n > self._end then
    return nil, "cannot write outside of allocated sector range"
  end
  return self.node.writeSector(n, dat)
end

function _base:writeAtSector(n, dat)
  checkArg(1, n, "number")
  checkArg(2, dat, "string")
  local ok, err
  for i=1, math.ceil(#dat/512), 512 do
    local wrt = dat:sub(i, i+512)
    if wrt then
      ok, err = self:writeSector(n, dat)
      n = n + 1
    end
  end
  return ok, err
end

function base:getCapacity()
  return (self._end * 512) - (self._start * 512)
end

function drive.cordon(proxy, start, _end)
  checkArg(1, proxy, "table")
  checkArg(2, start, "number", "nil")
  checkArg(3, _end, "number" (not start) and "nil")
  if proxy.type ~= "drive" then
    return nil, "provided component is not a drive"
  end
  start = start or 1
  _end = _end or drive.getCapacity() // 512
  return setmetatable({_start = start, _end = _end, node = proxy}, {__index = base})
end

return drive
