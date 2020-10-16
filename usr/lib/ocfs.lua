-- ocfs driver --

--[[ DISCLAIMER

        The below code is known to the state of California to cause cancer.
        The creator (Ocawesome101) is NOT responsible for any mutilations,
        nullifications, abominations, death, injury (permanant or temporary),
        perjury, murder, birth defects, feeling of shock, horror, or terror, or
        ANY other event, whether normal or abnormal, that may result from this
        code.
  ]]

local drive = require("drive")

local ocfs = {}

local patterns = {
  superblock = "<c7I1c504",
  inode = "<c1I1I2I4I4I8c44c448",
  generic = "<c1",
  extdata = "<c1" .. string.rep("I8", 63),
  filedata = "<c1c511"
}

local types = {
  file      = 1,
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

-- pack inode data into a string
-- XXX: it's up to whatever writes this to properly format it
function _fs:packData(data)
  checkArg(1, data, "table")
  local pat = string.format("<%s", string.rep("I8", #data))
  return string.pack(pat, table.unpack(data))
end

function _fs:unpackData(base, inInode, leaveLast)
  checkArg(1, base, "string")
  checkArg(2, inInode, "boolean", "nil")
  checkArg(3, leaveLast, "boolean", "nil")
  local pat = string.format("<%s%s", inInode and "" or "c1", string.rep("I8", #base - (inInode and 0 or 8) / 8))
  local unpacked = {string.unpack(pat, base)}
  if not leaveLast then
    local last = unpacked[#unpacked]
    unpacked[#unpacked] = nil
    if last ~= 0 then
      local extended = _fs:unpackData()
      for i=1, #extended, 1 do
        if extended[i] == 0 then break end
        unpacked[#unpacked + 1] = extended[i]
      end
    end
  end
  for i=1, #unpacked, 1 do
    if unpacked[i] == 0 then
      unpacked[i] = nil
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
    data        = data,
    sect        = sect
  }
end

-- find a free sector, and rebuild the free-sector cache if necessary
function _fs:findFreeSector()
  -- check the superblock free-sector cache
  -- note: unless we rewrite this sector this should be pretty fast for
  -- consecutive searches because of the drive library's caching features
  local sbl = self.node:readSector(1)
  local a, b, data = string.unpack(patterns.superblock, sbl)
  local cache = self:unpackData(data, nil, true)
  if #cache == 0 then
    print("OCFS: Rebuilding sector cache")
    -- rebuild the cache - this is SLOW.
    -- note: this will probably end up flushing the read-sector buffer
    -- on most configurations due to the large amount of read sectors
    local max = self.node:getCapacity() // 512
    for i=3, max, 1 do -- skip the first 2 sectors as they are ALWAYS used
      local data = self.node:readSector(max)
      local sig = data:sub(1,1)
      if sig ~= "e" and sig ~= "i" and sig ~= "d" then -- the sector is unused
        cache[#cache + 1] = data
        if #cache == 63 then -- max 63 sectors
          break
        end
      end
    end
  end
  local ret = table.remove(cache, 1)
  self.node:writeSector(1, string.pack(patterns.superblock, a, b,
                                                          self:packData(cache)))
  return ret
end

function _fs:writeInode(inode)
  checkArg(1, inode, "table")
  local data = self:packData(inode.data)
  local begin, write, dnext = nil, {}
  if #data <= 440 then
    begin = data
    dnext = 0
  else
    begin = data:sub(1, 440)
    data = data:sub(441)
    local total = math.ceil((#data) / 496)
    local s = self:findFreeSector()
    dnext = s
    for i=1, total, 1 do
      local t = self:findFreeSector()
      write[s] = "e" .. data:sub(496 * i - 495, 496 * i) .. string.pack("<I8", t)
      s = t
    end
  end
  write[inode.sect] = string.pack(patterns.inode, "i", inode.type,
                           packPermissions(inode.permissions), inode.owner,
                           inode.group, inode.lastModified, inode.name,
                           begin, dnext)
  
  for k, v in pairs(write) do
    self.node:writeSector(k, v)
  end
  return true
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
  local inode, err = self:resolve(file)
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
    return nil, "cannot read from closed file"
  end
  if not self.mode.r then
    return nil, "bad file descriptor"
  end
  -- some calculations for sector amounts and whatnot
  local start = math.floor(self.ptr / 512)
  local diffStart = self.ptr - (start * 512)
  local total = math.ceil((self.ptr + n) / 512)
  local read = ""
  for i=start, total, 1 do
    if not self.inode.data[i] then
      break
    end
    local data = self.node.node:readSector(self.inode.data[i])
    if data:sub(1, 1) ~= "d" then
      return nil, "bad file data signature (expected 'd', got '"..data:sub(1,1).."')"
    end
    read = read .. (data or ""):sub(2)
    if not data then break end
  end
  if n > #read then n = read end
  self.ptr = self.ptr + n
  return (read:sub(diffStart, diffStart + n))
end

-- this.... is *much* more complex than reading files
function _stream:write(dat)
  checkArg(1, dat, "string")
  if not self.mode.w or self.mode.a then
    return nil, "bad file descriptor"
  end
  -- self.node.node.node.node.node.node.node.node.node.node.isReadOnly() :^)
  if self.node.isReadOnly or self.node.node.node.isReadOnly() then
    return nil, "filesystem is read-only"
  end
  local start = self.ptr // 512
  local diff = self.ptr - start * 512
  -- split into 512-byte chunks
  local chunks = {}
  for i=1, #dat, 511 do
    chunks[#chunks + 1] = "d" .. dat:sub(i, i + 510)
  end
  -- calculate the starting sector index from our current file pointer
  if not self.inode.data[start + #chunks] then -- we need to allocate more space
    local diff = #chunks - (#self.inode.data - start)
    for i=1, diff, 1 do
      local sect, err = self:findFreeSector()
      if not sect then
        return nil, "disk is full"
      end
      self.inode.data[#self.inode.data + 1] = sect
    end
  end
  for i=1, #chunks, 1 do
    self.node.node:writeSector(self.inode.data[start + i - 1])
  end
  return true
end

function _stream:seek(whence, offset)
  checkArg(1, whence, "string", "nil")
  checkArg(2, offset, "number", (not whence) and "nil")
  if (not whence) or (not offset) then
    return self.ptr
  end
  if not self.mode.r then -- seek only in read mode
    return nil, "bad file descriptor"
  end
  local allow = {
    set = 0,
    cur = self.ptr,
    ["end"] = (#self.inode.data * 512)
  }
  assert(allow[whence],
         "bad argument #1 to 'seek' (invalid option '"..whence.."')")
  whence = allow[whence]
  self.ptr = offset
end

function _stream:close()
  self.closed = true
  return true
end

function _fs:open(file, mode)
  checkArg(1, file, "string")
  checkArg(2, mode, "string", "nil")
  -- TODO: file creation?
  local inode, err = self:resolve(file)
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

function _fs:touch(file, ftype)
  checkArg(1, file, "string")
  checkArg(2, ftype, "string")
  if not types[ftype] then
    return nil, string.format(
                "bad filetype (expected 'file' or 'directory', got %s)", ftype)
  end
  local segments = sane(file)
  local base = "/"..table.concat(segments, "/", 1, #segments - 2)
  local last = segments[#segments]
  local inode, err = self:resolve(base)
  if not inode then
    return nil, err
  end
  
  if inode.type ~= types.directory then
    return nil, base..": not a directory"
  end

  local new = {
    type        = types[ftype],
    permissions = inode.permissions,
    owner       = inode.owner,
    group       = inode.group,
    lastModified= os.time() * 1000,
    fname       = last,
    data        = {},
    sect        = self:findFreeSector()
  }
  inode.data[#inode.data + 1] = new.sect
  self:writeInode(inode)
  self:writeInode(new)
  return true
end

function _fs:remove(file)
  checkArg(1, file, "string")
  local inode, err = self:resolve(file)
  if not inode then
    return nil, err
  end
  if inode.type == types.directory then
    return nil, file .. ": is a directory"
  end
  local blank = string.rep("\0", 512)
  self.node:writeSector(inode.sect, blank)
  for i=1, #inode.data, 1 do
    self.node:writeSector(inode.data[i], blank)
  end
  return true
end

function _fs:removedir(file)
  checkArg(1, file, "string")
  local inode, err = self:resolve(file)
  if not inode then
    return nil, err
  end
  if inode.type ~= types.directory then
    return nil, file .. ": not a directory"
  end
  if #inode.data > 0 then
    return nil, file .. ": directory not empty"
  end
  local blank = string.rep("\0", 512)
  self.node:writeSector(inode.sect, blank)
  return true
end

function _fs:space()
  return {
    used = 0,
    total = self.node:getCapacity()
  }
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
  if flags.encrypted then
    return nil, "encrypted filesystems are unsupported"
  end

  local newObj = setmetatable({
    node = drv,
    flags = flags,
    freeCache = {},
    superblock = superblock,
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

return ocfs
