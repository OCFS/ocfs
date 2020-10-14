-- OpenComputers General Partition Table driver --
-- The partition table is documented at https://ocfs.github.io/ocgpt/ --

if not string.unpack then
  error("string.unpack not found - Lua 5.3+ is required")
end

local gpt = {}

local pdata = "<I1I3c8c36I8I8"
local types = {
  "ocfs",
  "openfs",
  "foxfs",
  "zebrafs",
  "nitrofs",
  "brofs"
}

function gpt.parseFlags(flags)
  checkArg(1, flags, "number")
  return {
    bootable = flags & 1 ~= 0,
    managedEmulation = flags & 2 ~= 0,
    ocuefiBoot = flags & 4 ~= 0
  }
end

function gpt.readTable(drv)
  checkArg(1, drv, "table")
  local ptdat = {}
  local pn = 1
  for i=2, 9, 1 do
    local read = drv.readSector(i)
    for x=1, 8, 1 do
      local chunk = read:sub(x*64-64, x*64)
      local ptype, pflags, guid, label, pstart, pend = string.unpack(chunk, pdata)
      if not (ptype and pflags and guid and label and pstart and pend) then
        goto exit
      end
      ptdat[pn] = {
        type = ptype,
        flags = gpt.parseFlags(pflags),
        guid = guid,
        label = label,
        _start = pstart,
        _end = pend
      }
      pn = pn + 1
    end
  end
  ::exit::
  return ptdat
end

function gpt.writeTable(drv, ptab)
  checkArg(1, drv, "table")
  checkArg(2, ptab, "table")
  local wrt = ""
  drv = drv.node or drv
  for i=1, #ptab, 1 do
    local e = ptab[i]
    wrt = wrt .. string.pack(pdat, e.type, e.flags, e.guid, e.label. e._start, e._end)
    if #wrt >= 512 then
      drv.writeSector(i / 8 + 1, wrt)
      wrt = ""
    end
  end
  if #wrt > 0 then
    drv.writeSector(9, wrt)
  end
end

function gpt.addPartition(drv, label, start, len)
  checkArg(1, drv, "table")
  checkArg(2, label, "string")
  checkArg(3, start, "number")
  checkArg(4, len, "number")
  drv = drv.node or drv -- we need the real drive for this
  local ptab = gpt.parse(drv)
  if #ptab == 64 then
    return nil, "too many partitions"
  end
  local _end = start + len
  for i=1, #ptab, 1 do
    local ent = ptab[i]
    if (start > ent._start and start < ent._end)
      or (_end > ent._start and _end < ent._end) then
      return nil, "specified partition overlaps with partition #"..i
    end
  end
  ptab[#ptab + 1] = {
    type = 0,
    flags = 0,
    label = label
    guid = gpt.mkGUID()
  }
end

return gpt
