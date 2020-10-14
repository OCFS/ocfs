-- ocfs driver --

local drive = require("drive")

local ocfs = {}
local patterns = {
  superblock = "<c7I1c504",
  inode = "<c1I1I2I4I4I8c44c444",
  generic = "<c1",
  extdata = "<c1" .. string.rep("I8", 63),
  filedata = "<c1c511"
}
local types = {
  file      = 1
  directory = 2
}
local FS_SIG = "\27OCFS\13\27"

-- TODO: rewrite permission packing code if possible
local function packPermissions(perms)
  local n = 0
  n = n | (perms.owner.r and 1) or 0
  n = n | (perms.owner.w and 2) or 0
  n = n | (perms.owner.x and 4) or 0
  n = n | (perms.group.r and 8) or 0
  n = n | (perms.group.w and 16) or 0
  n = n | (perms.group.x and 32) or 0
  n = n | (perms.other.r and 64) or 0
  n = n | (perms.other.w and 128) or 0
  n = n | (perms.other.x and 256) or 0
  return n
end

-- this could maybe be better but it works and it's sane
local function unpackPermissions(perms)
  return {
    owner = {
      r = perms & 1 ~= 0,
      w = perms & 2 ~= 0,
      x = perms & 4 ~= 0
    },
    group = {
      r = perms & 8  ~= 0,
      w = perms & 16 ~= 0,
      x = perms & 32 ~= 0
    },
    other = {
      r = perms & 64  ~= 0,
      w = perms & 128 ~= 0,
      x = perms & 256 ~= 0
    }
  }
end

-- base fs object for memory efficiency
local _fs = {}

function _fs:unpackData(base, inInode)
  checkArg(1, base, "string")
  checkArg(2, inInode, "boolean", "nil")
  local pat = string.format("<%s%s", inInode and "" or "c1", string.rep("I8", #base - (inInode and 0 or 8) / 8))
  local unpacked = {string.unpack(pat, base)}
  local last = unpacked[#unpacked]
  unpacked[#unpacked] = nil
  if last ~= 0 then
    local extended = _fs:unpackData()
    for i=1, #extended, 1 do
      unpacked[#unpacked + 1] = extended[i]
    end
  end
  return unpacked
end

function _fs:readInode(sect)
  checkArg(1, sect, "number")
  local dat = assert(self.node:readSector(sect))
  local sign,  ftype, perms, owner,
        group, lmod,  fname, fdata = string.unpack(patterns.inode, dat)
  if not (sign and ftype and perms and owner
               and group and lmod  and fname and fdata) then
    return nil, "invalid inode::"..sect
  end
  if sign ~= "i" then
    return nil, "invalid inode signature at inode::"..sect
  end
  if ftype == 0 then
    return nil, "invalid file type at inode::"..sect
  end
  local data = _fs:unpackData(fdata, true)
  return {
    type        = ftype,
    permissions = parsePermissions(perms),
    owner       = owner,
    group       = group,
    lastModified= lmod,
    name        = fname:gsub("\0", ""),
    data        = data
  }
end

-- make paths sane
local function sane(path)
  local ret = {}
  for seg in path:gmatch("[^/\\]+") do
    if seg == ".." then
      table.remove(ret, #ret)
    elseif seg ~= "." then
      ret[#ret + 1] = seg
    end
  end
  return ret
end

function _fs:resolve(file)
  checkArg(1, file, "string")
  local root = self:readInode(2)
  local segments = sane(file)
  local cur = root
  for i=1, #segments, 1 do
    if cur.type ~= types.directory then
      return nil, string.format("%s: not a directory", table.concat(segments, 1, i))
    end
    for i=1, #cur.data, 1 do
      local inode, err = self:readInode(cur.data[i])
      if not inode then
        return nil, err
      end
      if inode.name == segments[i] then
        if i == #segments then
          return inode
        else
          cur = inode
        end
      end
    end
  end
  return nil, file..": file not found"
end

function _fs:stat(file)
  checkArg(1, file, "string")
  local inode, err = resolve(file)
  if not inode then
    return nil, err
  end
  return inode
end

-- file streams are also objects. this simplifies some things.
local _stream = {}

function _stream:read(n)
  checkArg(1, n, "number")
  if self.closed then
    return nil, "cannot read from closed file stream"
  end
  -- XXX TODO XXX TODO: file data bound checks
  -- TODO XXX TODO XXX: strip leading 'f' from file data sectors
  local start = math.floor(self.ptr / 512)
  local diffStart = self.ptr - (start * 512)
  local total = math.ceil((self.ptr + n) / 512) - start
  local read = self.node.node:readSectorRange(self.inode.data[1] + start, total)
  self.ptr = self.ptr + n
  return (read:sub(diffStart, diffStart + n))
end

function _stream:write(dat)
  checkArg(1, dat, "string")
end

function _stream:close()
  self.closed = true
  return true
end

function _fs:open(file, mode)
  checkArg(1, file, "string")
  checkArg(2, mode, "string", "nil")
  local inode, err = resolve(file)
  if not inode then
    return nil, err
  end
  local stream = setmetatable({
    node = self,
    inode = inode,
    ptr = 0,
    mode = {}
  }, {__index = _stream})
  for c in mode:gmatch(".") do
    stream.mode[c] = true
  end
end

local function checkSuperblock(sb)
  local sign, flagn = string.unpack(patterns.superblock, sb)
  if not (sign and flagn) then
    return nil, "bad OCFS superblock (invalid data structure)"
  end
  if sign ~= FS_SIG then
    return nil, "bad OCFS superblock (invalid filesystem signature)"
  end
  return {
    readOnly = flagn & 1 ~= 0,
    encrypted = flagn & 2 ~= 0
  }
end

function ocfs.new(drv, pdata, proxify)
  checkArg(1, drv, "table")
  checkArg(2, pdata, (not drv.node) and "table" or "nil")
  checkArg(3, proxify, "boolean", "nil")

  if not drv.node then
    drv = drive.cordon(pdata._start, pdata._end)
  end
  local superblock = drv:readSector(1)
  local flags, err = checkSuperblock(superblock)
  
  if not flags then
    return nil, err
  end
  -- TODO: AES-256 encryption support

  local newObj = setmetatable({
    node = drv,
    flags = flags,
    superblock = superblock
  }, {
    __index = _fs
  })
  if proxify then
    local newProxy = setmetatable({}, {__index = function(t, k)
      return function(...)
        return t[k](t, ...)
      end
    end})
    return newProxy
  end
  return newObj
end
