#!/usr/bin/env lua

-- debug printing
local DEBUG

function pt(t)
  for index, v in ipairs(t) do
    -- print("[" .. tostring(index) .. ": " .. tostring(v) .. "]")
    io.write(tostring(v) .. ",")
  end
end

function pti(t)
  for index, v in ipairs(t) do
    print("[" .. tostring(index) .. ": " .. tostring(v) .. "]")
    -- io.write(tostring(v) .. ",")
  end
end

-- pico8 color palette
--                pico8 rgb,  256 color, terminal number
-- 0 black        #000000     #000000    16
-- 1 dark_blue    #1d2b53     #00005f    17
-- 2 dark_purple  #7e2553     #5f0000    52
-- 3 dark_green   #008751     #005f00    22
-- 4 brown        #ab5236     #af5f00    130
-- 5 dark_gray    #5f574f     #4e4e4e    239
-- 6 light_gray   #c2c3c7     #bcbcbc    250
-- 7 white        #fff1e8     #eeeeee    255
-- 8 red          #ff004d     #ff005f    197
-- 9 orange       #ffa300     #ffaf00    214
-- 10 yellow      #ffec27     #ffd700    220
-- 11 green       #00e436     #00d75f    41
-- 12 blue        #29adff     #00afff    39
-- 13 indigo      #83769c     #8787af    103
-- 14 pink        #ff77a8     #ff5faf    206
-- 15 peach       #ffccaa     #ffd7af    223

local picocolors = {
  {0x1D, 0x2B, 0x53}, -- 1 dark_blue
  {0x7E, 0x25, 0x53}, -- 2 dark_purple
  {0x00, 0x87, 0x51}, -- 3 dark_green
  {0xAB, 0x52, 0x36}, -- 4 brown
  {0x5F, 0x57, 0x4F}, -- 5 dark_gray
  {0xC2, 0xC3, 0xC7}, -- 6 light_gray
  {0xFF, 0xF1, 0xE8}, -- 7 white
  {0xFF, 0x00, 0x4D}, -- 8 red
  {0xFF, 0xA3, 0x00}, -- 9 orange
  {0xFF, 0xEC, 0x27}, -- 10 yellow
  {0x00, 0xE4, 0x36}, -- 11 green
  {0x29, 0xAD, 0xFF}, -- 12 blue
  {0x83, 0x76, 0x9C}, -- 13 indigo
  {0xFF, 0x77, 0xA8}, -- 14 pink
  {0xFF, 0xCC, 0xAA}, -- 15 peach
}
picocolors[0] = {0,0,0} -- 0 black

local picocolors256 = {
 17,  -- 1 dark_blue
 52,  -- 2 dark_purple
 22,  -- 3 dark_green
 130, -- 4 brown
 239, -- 5 dark_gray
 250, -- 6 light_gray
 255, -- 7 white
 197, -- 8 red
 214, -- 9 orange
 220, -- 10 yellow
 41,  -- 11 green
 39,  -- 12 blue
 103, -- 13 indigo
 206, -- 14 pink
 223, -- 15 peach
}
picocolors256[0] = 16 -- 0 black


local actual_random = math.random
function randomseed(s)
  return math.randomseed(s)
end
function round(i)
  return math.floor(i + .5)
end
function random(a, b)
  if not a then
    a, b = 0, 1
  end
  if not b then
    b = 0
  end
  return a + actual_random() * (b - a)
end
function random_int(n, minimum)
  return math.floor(random(n, minimum))
end
function cos(x)
  return math.cos((x or 0) * math.pi * 2)
end
function sin(x)
  return -math.sin((x or 0) * math.pi * 2)
end
function atan2(x, y)
  return (0.75 + math.atan2(x, y) / (math.pi * 2)) % 1.0
end
local sqrt = math.sqrt
local sub = string.sub
local add = table.insert

function isarray(x)
  return (type(x) == "table" and x[1] ~= nil) and true or false
end

function getiter(x)
  if isarray(x) then
    return ipairs
  elseif type(x) == "table" then
    return pairs
  end
  error("expected table", 3)
end

function del(t, x)
  local iter = getiter(t)
  for i, v in iter(t) do
    if v == x then
      if isarray(t) then
        table.remove(t, i)
        break
      else
        t[i] = nil
        break
      end
    end
  end
  return x
end

local abs = math.abs
local min = math.min
local max = math.max
local floor = math.floor
local ceil = math.ceil

-- bitwise math functions

function band(x, y)
  -- return bit.band(x * 0x10000, y * 0x10000) / 0x10000
  return ((x * 0x10000) & (y * 0x10000)) / 0x10000
end
function bor(x, y)
  -- return bit.bor(x * 0x10000, y * 0x10000) / 0x10000
  return ((x * 0x10000) | (y * 0x10000)) / 0x10000
end
function bxor(x, y)
  -- return bit.bxor(x * 0x10000, y * 0x10000) / 0x10000
  return ((x * 0x10000) ~ (y * 0x10000)) / 0x10000
end
function bnot(x)
  -- return bit.bnot(x * 0x10000) / 0x10000
  return (~(x * 0x10000)) / 0x10000
end
function shl(x, y)
  -- return bit.lshift(x * 0x10000, y) / 0x10000
  return ((x * 0x10000) << y) / 0x10000
end
function shr(x, y)
  -- return bit.arshift(x * 0x10000, y) / 0x10000
  return ((x * 0x10000) >> y) / 0x10000
end

-- vector class

Vector={}
Vector.__index=Vector
function Vector.new(x,y)
  return setmetatable({x=x or 0,y=y or 0},Vector)
end

function Vector.__add(self, other)
  return Vector(self.x + other.x, self.y + other.y)
end

function Vector.__sub(self, other)
  return Vector(self.x - other.x, self.y - other.y)
end

function Vector.__mul(self, scalar)
  return Vector(self.x * scalar, self.y * scalar)
end

function Vector.__div(self, scalar)
  return Vector(self.x / scalar, self.y / scalar)
end

function Vector.__eq(self, other)
  return self.x == other.x and self.y == other.y
end

function Vector:__tostring()
  return "[" .. tostring(self.x) .. ", " .. tostring(self.y) .. "]"
end

function Vector:add(v)
  self.x = self.x + v.x
  self.y = self.y + v.y
  return self
end

function Vector:clone()
  return Vector.new(self.x, self.y)
end

function Vector:ceil()
  self.x = ceil(self.x)
  self.y = ceil(self.y)
  return self
end

function Vector:round()
  self.x = round(self.x)
  self.y = round(self.y)
  return self
end

function Vector:about_equals(v)
  return round(v.x) == self.x and round(v.y) == self.y
end

function Vector:angle()
  return atan2(self.x,self.y)
end

function Vector:length()
  return math.sqrt(self.x ^ 2 + self.y ^ 2)
end

function Vector:scaled_length()
  return 182 * math.sqrt((self.x / 182) ^ 2 + (self.y / 182) ^ 2)
end

function Vector:perpendicular()
  return Vector(-self.y, self.x)
end

function Vector:normalize()
  local l = self:length()
  self.x = self.x / l
  self.y = self.y / l
  return self
end

function Vector:rotate(phi)
  local c=cos(phi)
  local s=sin(phi)
  local x=self.x
  local y=self.y
  self.x=c*x-s*y
  self.y=s*x+c*y
  return self
end

function Vector:ro()
  self.x=ro(self.x)
  self.y=ro(self.y)
  return self
end

function Vector:draw_point(c)
  pset(ro(self.x),
       ro(self.y),c)
end

function Vector:draw_line(v,c)
  line(ro(self.x),
       ro(self.y),
       ro(v.x),
       ro(v.y),c)
end

function Vector:draw_circle(radius,c,fill)
  local method=circ
  if fill then method=circfill end
  method(ro(self.x),
         ro(self.y),
         ro(radius),c)
end

setmetatable(Vector,{__call=function(_,...) return Vector.new(...) end})

EMPTY = ""
WITH_TRANSPARENCY = true
NO_TRANSPARENCY = false

Canvas={}
Canvas.__index=Canvas
function Canvas.new(r,c)
  local c=setmetatable(
    {canvas = {},
     rows=r,
     cols=c}, Canvas)
  c:clear_canvas()
  return c
end

function Canvas:__tostring()
  return "Canvas [rows:" .. tostring(self.rows) .. ", cols:" .. tostring(self.cols) .. "]"
end

function Canvas:clear_canvas()
  self.canvas = {}
  for y=1,self.rows do
    add(self.canvas,{})
    for x=1,self.cols do
      add(self.canvas[y], {
            0, 0, 0, -- fg rgb
            0, 0, 0, -- bg rgb
            EMPTY, --character
            16, -- fg 265 color number
            16, -- bg 265 color number
      })
    end
  end
end

function Canvas:blit(subcanvas, srow, scol)
  local start_row = srow or 0
  local start_col = scol or 0
  local end_row = min(start_row + subcanvas.rows, self.rows)
  local end_col = min(start_col + subcanvas.cols, self.cols)

  for r=1,subcanvas.rows do
    local newr = r+start_row
    if newr <= end_row then
      for c=1,subcanvas.cols do
        local newc = c+start_col
        if newc <= end_col then
          local rf, gf, bf, rb, gb, bb, ch, f256, b256 = subcanvas:get_pixel(r, c)
          -- print("start_row: "..start_row.." start_col:"..start_col.." base["..r..", "..c.."] offset["..newr..", "..newc.."] ".. "end_row:"..end_row.." end_col:"..end_col.." "..tostring(subcanvas).. " -> "..tostring(self))
          self:draw_fgbg(newr, newc, rf, gf, bf, rb, gb, bb, ch, f256, b256)
        end
      end
    end
  end
end

function Canvas:draw_char(row, col, c)
  self.canvas[row][col][7] = c
end

function Canvas:draw_fg(row, col, r, g, b, character)
  self.canvas[row][col][1] = r
  self.canvas[row][col][2] = g
  self.canvas[row][col][3] = b
  if character ~= nil then self.canvas[row][col][7] = character end
  if c256 ~= nil then self.canvas[row][col][8] = c256 end
end

function Canvas:draw_bg(row, col, r, g, b, character, c256)
  self.canvas[row][col][4] = r
  self.canvas[row][col][5] = g
  self.canvas[row][col][6] = b
  if character ~= nil then self.canvas[row][col][7] = character end
  if c256 ~= nil then self.canvas[row][col][9] = c256 end
end

function Canvas:draw_bg_picocolor(row, col, color, character)
  local r = picocolors[color][1]
  local g = picocolors[color][2]
  local b = picocolors[color][3]
  local c256 = picocolors256[color]
  self:draw_bg(row, col, r, g, b, character, c256)
end

function Canvas:draw_fgbg(row, col, rf, gf, bf, rb, gb, bb, character, c256f, c256b)
  self.canvas[row][col][1] = rf
  self.canvas[row][col][2] = gf
  self.canvas[row][col][3] = bf
  self.canvas[row][col][4] = rb
  self.canvas[row][col][5] = gb
  self.canvas[row][col][6] = bb
  if character ~= nil then self.canvas[row][col][7] = character end
  if c256f ~= nil then self.canvas[row][col][8] = c256f end
  if c256b ~= nil then self.canvas[row][col][9] = c256b end
end

function Canvas:get_pixel(row, col)
  return self.canvas[row][col][1], self.canvas[row][col][2], self.canvas[row][col][3], self.canvas[row][col][4], self.canvas[row][col][5], self.canvas[row][col][6], self.canvas[row][col][7], self.canvas[row][col][8], self.canvas[row][col][9]
end


function Canvas:row_col_min_max()
  local rmin = #self.canvas
  local rmax = 0
  local cmin = #self.canvas[1]
  local cmax = 0

  for row=1,#self.canvas do
    for col=1,#self.canvas[1] do
      local rf, gf, bf, rb, gb, bb, c, f256, b256 = self:get_pixel(row, col)
      -- if rb ~= 0 or gb ~= 0 or bb ~= 0 then
      if c ~= EMPTY then
        if col < cmin then cmin = col end
        if col > cmax then cmax = col end
        if row < rmin then rmin = row end
        if row > rmax then rmax = row end
      end
    end
  end

  if DEBUG then
    print("Canvas row["..rmin..".."..rmax.."] col["..cmin..".."..cmax.."]")
  end
  return rmin, rmax, cmin, cmax
end

function Canvas:new_canvas_cropped_with_stroke()
  local rmin, rmax, cmin, cmax = self:row_col_min_max()
  newcanvas = Canvas.new(rmax-rmin+1+2, cmax-cmin+1+2) -- adding +1 for indexing from 1, +2 or one extra pixel on each edge

  for row=rmin-1,rmax+1 do
    for col=cmin-1,cmax+1 do
      local fr, fg, fb, br, bg, bb, c, f256, b256 = self:get_pixel(row, col)
      -- +1: for the indexing from 1
      -- +another 1: to offset to position 1,1
      local new_row = row - rmin + 1 + 1
      local new_col = col - cmin + 1 + 1

      if c == EMPTY then
        local cn, cs, ce, cw

        -- check neighbors
        if row-1 > 0 then
          cs = self.canvas[row-1][col][7]
        else
          cs = EMPTY
        end

        if row+1 <= self.rows then
          cn = self.canvas[row+1][col][7]
        else
          cn = EMPTY
        end

        if col-1 > 0 then
          ce = self.canvas[row][col-1][7]
        else
          ce = EMPTY
        end

        if col+1 <= self.cols then
          cw = self.canvas[row][col+1][7]
        else
          cw = EMPTY
        end

        if cs ~= EMPTY or cn ~= EMPTY or ce ~= EMPTY or cw ~= EMPTY then
          newcanvas:draw_fgbg(new_row, new_col, fr, fg, fb, br, bg, bb, " ", f256, b256)
        end
      else
        -- copy character as normal
        newcanvas:draw_fgbg(new_row, new_col, fr, fg, fb, br, bg, bb, c, f256, b256)
      end
    end
  end
  return newcanvas
end

setmetatable(Canvas,{__call=function(_,...) return Canvas.new(...) end})

function concat_canvases(sprite_canvases, separation)
  local offset = separation or 0
  local total_cols = (#sprite_canvases-1)*offset
  local max_rows = 0

  for i, sprite in ipairs(sprite_canvases) do
    total_cols = total_cols + sprite.cols
    if sprite.rows > max_rows then
      max_rows = sprite.rows
    end
  end

  local s = Canvas.new(max_rows, total_cols)
  local current_col_offset = 0
  for i, sprite in ipairs(sprite_canvases) do
    row_offset = floor((max_rows - sprite.rows)/2)
    s:blit(sprite, row_offset, current_col_offset)
    current_col_offset = current_col_offset + sprite.cols + offset
  end
  return s
end

Terminal={}
Terminal.__index=Terminal
function Terminal.new()
  local t=setmetatable(
    {mode = 256,
    }, Terminal)
  t:update_screen_width()
  t:update_screen_height()
  return t
end

function Terminal:draw_canvas(canvas, transparency, rmin, rmax, cmin, cmax)
  row_start = rmin or 1
  row_end = rmax or canvas.rows
  col_start = cmin or 1
  col_end = cmax or canvas.cols

  if DEBUG then
    -- debug: draw col numbers
    self:write(self:clear_formatting().."   ")
    for col=col_start,col_end do
      if col <= self.screen_width then
        self:write(self:fg(255,255,255)..self:bg(0,0,0)..string.format("%d",col%10))
      end
    end
    self:write(self:clear_formatting().."\n")
  end

  for row=row_start,row_end do
    for col=col_start,col_end do
      if col <= self.screen_width then
        local rf, gf, bf, rb, gb, bb, c, f256, b256 = canvas:get_pixel(row, col)

        -- debug: draw row numbers
        if DEBUG then
          if col==col_start then
            if col <= self.screen_width then
              self:write(self:fg(255,255,255)..self:bg(0,0,0)..string.format("%02d:",row))
            end
          end
        end

        if transparency then
          if c == EMPTY then
            -- transparent
            self:write(self:clear_formatting().." ")
          else
            local fg, bg
            if COLORS_256 then
              fg = self:fg256(f256)
              bg = self:bg256(b256)
            else
              fg = self:fg(rf,gf,bf)
              bg = self:bg(rb,gb,bb)
            end
            self:write(fg..bg..c)
          end
        else
          if c == EMPTY then c = " " end
          local fg, bg
          if COLORS_256 then
            local fg = self:fg256(f256)
            local bg = self:bg256(b256)
          else
            local fg = self:fg(rf,gf,bf)
            local bg = self:bg(rb,gb,bb)
          end
          self:write(fg..bg..c)
        end
      end
    end
    self:write(self:clear_formatting().."\n")
  end
  self:write(self:clear_formatting())
end

function Terminal:draw_canvas_half_height(canvas, transparency, rmin, rmax, cmin, cmax)
  row_start = rmin or 1
  row_end = rmax or canvas.rows
  col_start = cmin or 1
  col_end = cmax or canvas.cols
  local even_row

  if DEBUG then
    -- debug: draw col numbers
    self:write(self:clear_formatting().."   ")
    for col=col_start,col_end do
      if col <= self.screen_width then
        self:write(self:fg(255,255,255)..self:bg(0,0,0)..string.format("%d",col%10))
      end
    end
    self:write(self:clear_formatting().."\n")
  end

  for row=row_start,row_end do
    if row%2 == 0 then
      even_row = true
    else
      even_row = false
    end

    if even_row then
      -- current row is even
      for col=col_start,col_end do
        if col <= self.screen_width then
          local rf2, gf2, bf2, rb2, gb2, bb2, c2, f2562, b2562 = canvas:get_pixel(row-1, col)
          local rf,  gf,  bf,  rb,  gb,  bb,  c,  f256,  b256  = canvas:get_pixel(row, col)

          if DEBUG then
            -- debug: print leading row number
            if col==col_start then
              if col <= self.screen_width then
                self:write(self:fg(255,255,255)..self:bg(0,0,0)..string.format("%02d:",row)..self:clear_formatting())
              end
            end
          end

          if transparency then
            if c2 == EMPTY and c == EMPTY then
              self:write(self:clear_formatting() .." ")
            elseif c2 == EMPTY and c ~= EMPTY then
              -- previous row is empty, draw just the fg character for this row
              local fg
              if COLORS_256 then
                fg = self:fg256(b256)
              else
                fg = self:fg(rb,gb,bb)
              end
              self:write(self:clear_formatting()..fg.."▄")
            elseif c2 ~= EMPTY and c == EMPTY then
              -- this row is empty draw previous row color as fg character on top
              local fg
              if COLORS_256 then
                fg = self:fg256(b2562)
              else
                fg = self:fg(rb2,gb2,bb2)
              end
              self:write(self:clear_formatting()..fg.."▀")
            else
              -- both pixels are set
              -- previous row is bg
              -- current row is bg with box character
              local fg, bg
              if COLORS_256 then
                fg = self:fg256(b256)
                bg = self:bg256(b2562)
              else
                fg = self:fg(rb,gb,bb)
                bg = self:bg(rb2,gb2,bb2)
              end
              self:write(fg..bg.."▄")
              -- Box Characters: ▄▀█
            end
          else
            local fg, bg
            if COLORS_256 then
              fg = self:fg256(b256)
              bg = self:bg256(b2562)
            else
              fg = self:fg(rb,gb,bb)
              bg = self:bg(rb2,gb2,bb2)
            end
            self:write(fg..bg.."▄")
          end

        end -- if col <= self.screen_width then
      end
      self:write(self:clear_formatting().."\n")

    else
      -- current row is odd
      if row==row_end then
        -- is it the last row?
        self:write(self:clear_formatting())
        for col=col_start,col_end do
          if col <= self.screen_width then

            if DEBUG then
              -- debug: print leading row number
              if col==col_start then
                self:write(self:fg(255,255,255)..self:bg(0,0,0)..string.format("%02d:",row)..self:clear_formatting())
              end
            end

            local rf, gf, bf, rb, gb, bb, c, f256, b256 = canvas:get_pixel(row, col)
            if transparency then
              if c == EMPTY then
                -- write a space
                self:write(self:clear_formatting().." ")
              else
                -- write a top box character
                local fg, bg
                if COLORS_256 then
                  local fg = self:fg256(b256)
                end
                local fg = self:fg(rb,gb,bb)
                self:write(self:clear_formatting()..fg.."▀")
              end
            else
              -- set bg colors
              local fg, bg
              if COLORS_256 then
                fg = self:fg256(b256)
                bg = self:bg256(b256)
              else
                fg = self:fg(rb,gb,bb)
                bg = self:bg(rb,gb,bb)
              end
              self:write(fg..bg.."▀")
            end
          end

        end
        self:write(self:clear_formatting().."\n")
      end
    end -- row==row_end

  end -- current row is odd
  self:write(self:clear_formatting())
end

function Terminal:update_screen_width()
  local handle = io.popen("tput cols")
  local result = handle:read("*a")
  handle:close()
  self.screen_width = tonumber(result)
end

function Terminal:update_screen_height()
  handle = io.popen("tput lines")
  result = handle:read("*a")
  handle:close()
  self.screen_height = tonumber(result)
end

function Terminal:fg256(index)
  return "\x1B[38;5;".. index .. "m"
  -- return "\027[38;5;".. index .. "m"
end
function Terminal:bg256(index)
  return "\x1B[48;5;".. index .. "m"
  -- return "\027[48;5;".. index .. "m"
end
function Terminal:clear_formatting()
  return "\x1B[m"
  -- return "\027[m"
end

function Terminal:fg(r, g, b)
  return "\x1B[38;2;".. r .. ";" .. g .. ";" .. b .. "m"
  -- return "\027[38;2;".. r .. ";" .. g .. ";" .. b .. "m"
end
function Terminal:bg(r, g, b)
  return "\x1B[48;2;".. r .. ";" .. g .. ";" .. b .. "m"
  -- return "\027[48;2;".. r .. ";" .. g .. ";" .. b .. "m"
end
function Terminal:write(s)
  io.write(s)
end

function Terminal:hide_cursor()
  -- "tput civis"
  self:write("\x1b[?25l")
end

function Terminal:show_cursor()
  -- "tput cnorm"
  self:write("\x1b[34h\x1b[?25h")
end

function Terminal:clear_screen()
  self:write("\x1B[2J")
end

function Terminal:move_cursor(row, column)
  self:write("\x1B[".. row ..";" .. column .. "H")
end

function Terminal:move_cursor_home()
  -- self:write("\x1B[[H")
  self:move_cursor(0,0)
end

function Terminal:move_cursor_next_line()
  self:write("\x1B[E")
end

setmetatable(Terminal,{__call=function(_,...) return Terminal.new(...) end})

term = Terminal.new()

function split(s)
  local t,start_index,ti={},2,split_start or 0
  local mode=sub(s,1,1)
  for i=2,#s do
    local c=sub(s,i,i)
    if mode=="x" then
      t[ti]=("0x"..c)+0
      ti = ti + 1
    elseif c=="," then
      local sstr=sub(s,start_index,i-1)
      if mode=="a" then
        if sstr=="nil" then sstr=nil end
        t[ti]=sstr
      else
        t[ti]=sstr+0
      end
      ti = ti + 1
      start_index=i+1
    end
  end
  return t
end

function nsplit(s)
  local t,start_index,ti={},1,split_start or 0
  for i=1,#s do
    if sub(s,i,i)=="|" then
      t[ti]=split(sub(s,start_index,i-1))
      ti = ti + 1
      start_index=i+1
    end
  end
  return t
end

-- init values
-- Make a new planet type
function new_planet(planet_spec)
  local p = nsplit(planet_spec)
  local args = p[2]
  return {
    class_name = p[1][1],
    noise_octaves = args[1],
    noise_zoom = args[2],
    noise_persistance = args[3],
    mmap_color = args[4],
    full_shadow = args[5] or 1,
    transparent_color = args[6] or 14,
    min_noise_stretch_factor = args[7] or 1,
    max_noise_stretch_factor = args[8] or 1,
    min_size = args[9] or 10,
    color_map = p[3]
  }
end

function _init()
  -- grads3 needs split_start = 0
  grads3 = nsplit"n1,1,0,|n-1,1,0,|n1,-1,0,|n-1,-1,0,|n1,0,1,|n-1,0,1,|n1,0,-1,|n-1,0,-1,|n0,1,1,|n0,-1,1,|n0,1,-1,|n0,-1,-1,|"
  -- following nsplits need split_start = 1
  split_start = 1
  ijks = nsplit"n1,0,0,1,1,0,|n1,0,0,1,0,1,|n0,0,1,1,0,1,|n0,0,1,0,1,1,|n0,1,0,0,1,1,|n0,1,0,1,1,0,|"
  star_colors = nsplit"xaecd76|x98d165|x421051|x767676|x656565|x515151|"
  -- star_colors = {{0xa,0xe,0xc,0xd,0x7,0x6},{0x9,0x8,0xd,0x1,0x6,0x5},{0x4,0x2,0x1,0x0,0x5,0x1},{0x7,0x6,0x7,0x6,0x7,0x6},{0x6,0x5,0x6,0x5,0x6,0x5},{0x5,0x1,0x5,0x1,0x5,0x1}}
  darkshipcolors = split"x01221562493d189"
  dark_planet_colors = split"x0011055545531121"
  health_colormap = split"x8899aaabbb"
  damage_colors = split"x7a98507a98507a9850"
  sun_colors = split"x6ea9d789ac"
  ship_names = split"aFighter,Cruiser,Freighter,Super Freighter,Station,"
  ship_types = nsplit"n1.5,.25,.7,.75,.8,-2,1,14,18,|n3.5,.5,.583333,0,.8125,-1,1,18,24,|n3,2,.2125,0,.8125,-3,1,16,22,|n6,0,.7,-.25,.85,.25,1,32,45,|n4,1,.1667,-1,.3334,0,.6668,1,.8335,-1,1,30,40,|"

  planet_types = {
    new_planet("atundra,|n5,.5,.6,6,|x76545676543|"),
    new_planet("adesert,|n5,.35,.3,9,|x449944994499b1949949949949949|"),
    new_planet("abarren,|n5,.55,.35,5,|x565056765056|"),
    new_planet("alava,|n5,.55,.65,4,|x040504049840405040|"),
    new_planet("aterran,|n5,.3,.65,11,1,|x1111111dcfbb3334567|"),
    new_planet("aisland,|n5,.55,.65,12,1,|x11111111dcfb3|"),
    new_planet("agas giant,|n1,.4,.75,2,1,14,4,20,20,|x76d121c|"),
    new_planet("agas giant,|n1,.4,.75,8,1,12,4,20,20,|x7fe21288|"),
    new_planet("agas giant,|n1,.7,.75,10,1,14,4,20,20,|xfa949a|"),
    new_planet("arainbow giant,|n1,.7,.75,15,1,4,4,20,20,|x1dcba9e82|"),
  }
  planet_max_radius = 20
end

-- ship functions
ship = {}
ship.__index = ship
function ship.new(h)
  local shp = {
    npc = false,
    hostile = h,
    sector_position = Vector(),
    cur_deltav = 0,
    cur_gees = 0,
    angle = 0,
    angle_radians = 0,
    heading = 90,
    velocity_angle = 0,
    velocity_angle_opposite = 180,
    velocity = 0,
    velocity_vector = Vector(),
    orders = {},
    last_fire_time = -6
  }
  setmetatable(shp,ship)
  return shp
end


function ship:buildship(seed,stype)
  self.ship_type_index=stype or random_int(#ship_types)+1

  local seed_value=seed or random_int(262144)
  randomseed(seed_value)
  -- print("Seed:")
  -- print(seed_value)
  self.seed_value=seed_value
  self.name=ship_names[self.ship_type_index]
  -- print(self.name)
  local shape=ship_types[self.ship_type_index]
  -- pt(shape)

  -- print("Colors:")
  local ship_colors=split"x6789abcdef"
  -- pt(ship_colors)
  for i=1,6 do
    del(ship_colors,ship_colors[random_int(#ship_colors)+1])
  end
  -- pt(ship_colors)

  local hp=0
  local ship_mask={}
  local rows=random_int(shape[#shape]+1,shape[#shape-1])
  local cols=floor(rows/2)
  -- print(rows)
  -- print(cols)

  -- make a ship_mask, fill with transparent color
  for y=1,rows do
    add(ship_mask,{})
    for x=1,cols do
      add(ship_mask[y],ship_colors[4])
    end
  end

  local slopei,slope=2,Vector(1,shape[1])

  for y=2,rows-1 do

    for x=1,cols do

      local color=ship_colors[1+floor((y+random_int(3)-1)/rows*3)]

      if cols-x<max(0,floor(slope.y)) then
        if random()<.6 then
          ship_mask[y][x]=color
          hp = hp + 1
          if ship_mask[y-1][x]==ship_colors[4] then
            ship_mask[y][x]=darkshipcolors[color]
          end
        end
      end

    end

    if y>=floor(shape[slopei+1]*rows) then
      slopei = slopei + 2
    end
    slope=slope+Vector(1,shape[slopei])

    if slope.y>0 and y>3 and y<rows-1 then
      for i=1,random_int(round(slope.y/4)+1) do
        ship_mask[y][cols-i]=5
        hp = hp + 2
      end
    end

  end

  -- make the ship thickness odd (odd_columns=1)
  --                     or even (odd_columns=0)
  local odd_columns=random_int(2)

  for y=rows,1,-1 do
    for x=cols-odd_columns,1,-1 do
      add(ship_mask[y],ship_mask[y][x])
    end
  end

  if self.ship_type_index==#ship_types then
    hp = hp * 4
  end

  self.hp=hp
  self.max_hp=hp
  self.hp_percent=1
  self.deltav=max(hp*-0.0188+4.5647,1)*0.0326*30.593514175
  local turn_factor=1
  if self.ship_type_index==4 then
    turn_factor = turn_factor * .5
  end
  self.turn_rate=180
  self.turn_rate=turn_factor*max(hp*-0.0470+11.4117,2)
  self.sprite_rows=rows
  self.sprite_columns=#ship_mask[1]
  self.transparent_color=ship_colors[4]
  self.sprite=ship_mask
  return self
end

function ship:get_sprite_canvas_rotated(angle, add_stroke)
  local size_factor = 2
  newcanvas = Canvas(ceil(pilot.sprite_rows * size_factor),
                     ceil(pilot.sprite_columns * size_factor))
  self:draw_sprite_rotated(newcanvas, angle)
  if add_stroke then
    return newcanvas:new_canvas_cropped_with_stroke()
  else
    return newcanvas
  end
end

function ship:draw_sprite_rotated(canvas, angle, pos)
  -- canvas center point minus spritesize/2 offset
  local sc = Vector(round(canvas.cols/2),
                    round(canvas.rows/2)) -
    Vector(round(self.sprite_columns/2),
           round(self.sprite_rows/2))

  local screen_position=pos or sc
  screen_position:add(
    Vector(
      ceil(self.sprite_rows/2),
      ceil(self.sprite_columns/2)
  ))
  local a=angle or self.angle_radians
  local rows=self.sprite_rows
  local cols=self.sprite_columns
  local tcolor=self.transparent_color
  local even_row=false

  for y=1,rows do
    if y%2 == 0 then
      even_row = true
    else
      even_row = false
    end
    for x=1,cols do
      local color=self.sprite[x][y]
      if color~=nil and color~=tcolor then
        local pixel1=Vector(x-floor(cols/2)-1,
                            rows-y-floor(rows/2))
        local pixel2=Vector(pixel1.x+1,
                            pixel1.y)
        pixel1:rotate(a):add(screen_position):round()
        pixel2:rotate(a):add(screen_position):round()

        if color == nil then color = 0 end
        if color<0 then color=5 end
        local r = picocolors[color][1]
        local g = picocolors[color][2]
        local b = picocolors[color][3]
        c256 = picocolors256[color]

        -- draw using space and bg character
        canvas:draw_bg(pixel1.y, pixel1.x, r, g, b, " ", c256)
        canvas:draw_bg(pixel2.y, pixel2.x, r, g, b, " ", c256)
      end
    end
  end

end

-- planets
function draw_sprite_circle(canvas, xc, yc, radius, filled, color)
  local xvalues={}
  local fx,fy=0,0
  local x,y=-radius,0
  local err=2-2*radius

  while(x<0) do
    xvalues[1+x*-1]=y

    if not filled then
      fx,fy=x,y
    end
    for i=x,fx do
      -- sset(xc-i,yc+y)
      -- sset(xc+i,yc-y)
      -- canvas:draw_bg_picocolor(1 + xc-i, 1 + yc+y, color, " ")
      -- canvas:draw_bg_picocolor(1 + xc+i, 1 + yc-y, color, " ")
      canvas:draw_bg_picocolor(1 + yc+y, 1 + xc-i, color, " ")
      canvas:draw_bg_picocolor(1 + yc-y, 1 + xc+i, color, " ")
    end
    for i=fy,y do
      -- sset(xc-i,yc-x)
      -- sset(xc+i,yc+x)
      -- canvas:draw_bg_picocolor(1 + xc-i, 1 + yc-x, color, " ")
      -- canvas:draw_bg_picocolor(1 + xc+i, 1 + yc+x, color, " ")
      canvas:draw_bg_picocolor(1 + yc-x, 1 + xc-i, color, " ")
      canvas:draw_bg_picocolor(1 + yc+x, 1 + xc+i, color, " ")
    end

    radius=err
    if radius<=y then
      y = y + 1
      err = err + y*2+1
    end
    if radius>x or err>y then
      x = x + 1
      err = err + x*2+1
    end
  end
  xvalues[1]=xvalues[2]
  return xvalues
end

perms={}
for i=0,255 do perms[i]=i end
for i=0,255 do
  local r=random_int(32767)%256
  perms[i],perms[r]=perms[r],perms[i]
end

perms12={}
for i=0,255 do
  local x=perms[i]%12
  perms[i+256],perms12[i],perms12[i+256]=perms[i],x,x
end

function getn_3d(ix,iy,iz,x,y,z)
  local t=.6-x*x-y*y-z*z
  local index=perms12[ix+perms[iy+perms[iz]]]
  return max(0,(t*t)*(t*t))*(grads3[index][0]*x+grads3[index][1]*y+grads3[index][2]*z)
end

function simplex3d(x,y,z)
  local s=(x+y+z)*0.333333333
  local ix,iy,iz=floor(x+s),floor(y+s),floor(z+s)
  local t=(ix+iy+iz)*0.166666667
  local x0,y0,z0=x+t-ix,y+t-iy,z+t-iz
  ix,iy,iz=band(ix,255),band(iy,255),band(iz,255)
  local n0=getn_3d(ix,iy,iz,x0,y0,z0)
  local n3=getn_3d(ix+1,iy+1,iz+1,x0-0.5,y0-0.5,z0-0.5)
  local ijk
  if x0>=y0 then
    if y0>=z0 then
      ijk=ijks[1]
    elseif x0>=z0 then
      ijk=ijks[2]
    else
      ijk=ijks[3]
    end
  else
    if y0<z0 then
      ijk=ijks[4]
    elseif x0<z0 then
      ijk=ijks[5]
    else
      ijk=ijks[6]
    end
  end
  local n1=getn_3d(ix+ijk[1],iy+ijk[2],iz+ijk[3],x0+0.166666667-ijk[1],y0+0.166666667-ijk[2],z0+0.166666667-ijk[3])
  local n2=getn_3d(ix+ijk[4],iy+ijk[5],iz+ijk[6],x0+0.333333333-ijk[4],y0+0.333333333-ijk[5],z0+0.333333333-ijk[6])
  return 32*(n0+n1+n2+n3)
end

planet={}
planet.__index=planet
function planet.new(x,y,phase,r,ptype)
  local planet_type_index = ptype or random_int(#planet_types)+1
  local planet_type = planet_types[planet_type_index]

  local radius=r or random_int(planet_type.min_size+10, planet_type.min_size)
  local planet_canvas = Canvas.new(radius*2+1, radius*2+1)
  return setmetatable({
      planet_canvas=planet_canvas,
      screen_position=Vector(),
      radius=radius,
      sector_position=Vector(x,y),
      bottom_right_coord=2*radius-1,
      phase=phase,
      planet_type=planet_type,
      noise_factor_vert=random_int(
        planet_type.max_noise_stretch_factor + 1,
        planet_type.min_noise_stretch_factor),
      noisedx=random(1024),
      noisedy=random(1024),
      noisedz=random(1024),
      rendered_circle=false,
      rendered_terrain=false,
      color=planet_type.mmap_color},planet)
end

-- function planet:draw(ship_pos)
--   if stellar_object_is_visible(self,ship_pos) then
--     self:render_planet()
--     love.graphics.draw(
--       self.planet_canvas,
--       self.screen_position.x-self.radius+zoom_offset.x,
--       self.screen_position.y-self.radius+zoom_offset.y
--     )
--   end
-- end

function planet:render_planet(fullmap, render_far_side)
  local radius=self.radius-1
  if fullmap then radius=47 end

  if not self.rendered_circle then
    self.width=self.radius*2
    self.height=self.radius*2
    self.x=0
    self.yfromzero=0
    self.y=radius-self.yfromzero
    self.phi=0
    -- sect:reset_planet_visibility()
    -- pal()
    -- palt(0,false)
    -- palt(self.planet_type.transparent_color,true)
    self.planet_canvas:clear_canvas()
    -- color(pink)
    -- sset(0,0)
    -- sset(1,1)
    -- sset(2,2)
    xvaluestring = ""
    if fullmap then
      self.width,self.height=114,96
    else
      -- draw filled black
      self.xvalues=draw_sprite_circle(self.planet_canvas, radius, radius, radius, true, 0)
      -- draw not-filled planet primary color
      -- draw_sprite_circle(self.planet_canvas, radius, radius, radius, false, self.planet_type.mmap_color)
    end
    self.rendered_circle=true
  end

  -- local m = ""
  -- for i, x in ipairs(self.xvalues) do
  --   m = m..x..", "
  -- end

  -- table.insert(debug_messages, self.xvalues[1])
  -- table.insert(debug_messages, self.xvalues[#self.xvalues])
  -- local absy=abs(radius-self.y)+1
  -- xvaluestring = xvaluestring..absy.." "
  -- xvaluestring = xvaluestring..self.y.." -> "
  -- if self.xvalues[absy] then
  --   xvaluestring = xvaluestring.." "..self.xvalues[absy].." "
  -- else
  --   xvaluestring = xvaluestring.." nil "
  -- end
  -- if self.xvalues[self.y] then
  --   xvaluestring = xvaluestring.." "..self.xvalues[self.y].."\n"
  -- else
  --   xvaluestring = xvaluestring.." nil\n"
  -- end
  -- table.insert(debug_messages, xvaluestring)

  if (not self.rendered_terrain) and self.rendered_circle then

    local theta_start,theta_end=0,.5
    local theta_increment=theta_end/self.width
    if fullmap and render_far_side then
      theta_start=.5
      theta_end=1
    end

    if self.phi>.25 then
      self.rendered_terrain=true
    else

      local partialshadow=self.planet_type.full_shadow~=1
      local phase_values,phase={},self.phase

      local x,doublex,x1,x2,i,c1,c2
      local y=radius-self.y
      local xvalueindex=abs(y)+1
      if xvalueindex<=#self.xvalues then
        x=floor(sqrt(radius*radius-y*y))
        doublex=2*x
        if phase<.5 then
          x1=-self.xvalues[xvalueindex]
          x2=floor(doublex-2*phase*doublex-x)
        else
          x1=floor(x-2*phase*doublex+doublex)
          x2=self.xvalues[xvalueindex]
        end
        for i=x1,x2 do
          if partialshadow
            or (phase<.5 and i>x2-2)
          or (phase>=.5 and i<x1+2) then
            phase_values[radius+i] = 1
          else
            phase_values[radius+i] = 0
          end
        end
      end

      for theta=theta_start,theta_end-theta_increment,theta_increment do

        local phasevalue=phase_values[self.x]
        local c=nil

        -- if fullmap or phasevalue==1 then
        if fullmap or
          (phasevalue ~= 0 and
             xvalueindex<=#self.xvalues and
             -- left edge of planet circle
             self.x >= self.radius-self.xvalues[xvalueindex]-1 and
             self.x < self.radius+self.xvalues[xvalueindex]
          )

        then
          -- and sget(self.x,self.y)~=self.planet_type.transparent_color then
          local freq=self.planet_type.noise_zoom
          local max_amp=0
          local amp=1
          local value=0
          for n=1,self.planet_type.noise_octaves do
            -- value=value+love.math.noise(
            value=value+simplex3d(
              self.noisedx+freq*cos(self.phi)*cos(theta),
              self.noisedy+freq*cos(self.phi)*sin(theta),
              self.noisedz+freq*sin(self.phi)*self.noise_factor_vert)
            max_amp = max_amp + amp
            amp = amp * self.planet_type.noise_persistance
            freq = freq * 2
          end
          value = value / max_amp
          if value>1 then value=1 end
          if value<-1 then value=-1 end
          value = value + 1
          value = value * (#self.planet_type.color_map-1)/2
          value=round(value)

          c=self.planet_type.color_map[value+1]
          if not fullmap and phasevalue==1 then
            c=dark_planet_colors[c+1]
          end
        end

        -- if xvalueindex<=#self.xvalues and
        --   (self.x == self.radius-self.xvalues[xvalueindex]-1 or
        --    self.x == self.radius+self.xvalues[xvalueindex]-1) then
        --   c=0 -- color = black
        -- end
        if c ~= nil then
          -- sset(self.x,self.y)
          -- self.planet_canvas:draw_bg_picocolor(1 + self.x, 1 + self.y, c, " ")
          self.planet_canvas:draw_bg_picocolor(1 + self.y, 1 + self.x, c, " ")
        end
        self.x = self.x + 1
      end
      self.x=0
      if self.phi>=0 then
        self.yfromzero = self.yfromzero + 1
        self.y=radius+self.yfromzero
        self.phi = self.phi + .5/(self.height-1)
      else
        self.y=radius-self.yfromzero
      end
      self.phi = self.phi * -1
    end

  end

  return self.rendered_terrain
end

function cmd_draw_planet_map(planet_count, starting_seed, camera_x, camera_z)
  if DEBUG then
    print("Terminal width: "..term.screen_width)
  end

  local camera_angle = random()
  local px = cos(camera_angle)*100
  local py = sin(camera_angle)*100
  -- local px = camera_x or 50 -- sector_position.x
  -- local py = camera_z or -100 -- sector_position.y
  local seed_value=starting_seed or random_int(262144)
  randomseed(seed_value)
  local pcount = planet_count or #planet_types
  local planets = {}
  local planet_sprites = {}
  local planet_maps = {}

  local max_rows = 0
  for i=1,pcount do
    local ptype = 1
    local rad = 5
    if i <= 6 then
      ptype = random_int(7, 1)
      rad = random_int(8) + 5
    else
      ptype = random_int(5, 1) + 6
      rad = random_int(8) + 10
    end

    local p = planet.new(px, py, ((1-Vector(px,py):angle())-.25)%1, rad, i)
    if p.planet_canvas.rows > max_rows then
      max_rows = p.planet_canvas.rows
    end
    local rendering_done = false
    while not rendering_done do
      rendering_done = p:render_planet()
    end
    add(planets, p)
    add(planet_sprites, p.planet_canvas)
    if i == 6 or i == 10 then
      local c = concat_canvases(planet_sprites, 0)
      -- add(planet_maps, c)
      term:draw_canvas_half_height(c, WITH_TRANSPARENCY)
      planet_sprites = {}
    end
  end

  -- -- local planet_map_canvas = concat_canvases(planet_sprites, 0)
  -- for i, p in ipairs(planet_maps) do
  --   term:draw_canvas_half_height(p, WITH_TRANSPARENCY)
  -- end


  -- term:draw_canvas(p.planet_canvas, WITH_TRANSPARENCY)
  -- term:draw_canvas_half_height(planet_map_canvas, WITH_TRANSPARENCY)

  -- for index, pixel in ipairs(p.planet_canvas.canvas[24]) do
  --   print(index)
  --   pti(pixel)
  -- end
  --
  -- print(camera_angle)
  -- print(px)
  -- print(py)
end


-- terminal output routines

function cmd_draw_shipyard()
  pilot=ship.new()
  -- some nice looking seeds
  -- pilot:buildship(5725,2)
  -- pilot:buildship(202915,2)
  -- pilot:buildship(147828, 2)
  -- pilot:buildship(30718,1)
  -- pilot:buildship(9266,2)
  -- pilot:buildship(122414,1)
  -- pilot:buildship(174969,1)
  -- pilot:buildship(97515,1)
  -- pilot:buildship(160782,2)
  -- pilot:buildship(70182,2)
  -- pilot:buildship(19741,2)
  -- pilot:buildship(157247,2)
  -- pilot:buildship(50110,2)
  -- pilot:buildship(63953,2)
  -- pilot:buildship(35957,2)
  -- pilot:buildship(160921,2)
  -- pilot:buildship(131525,2)

  -- while true do

  pilot:buildship()


  local rotation_start = 0
  local rotation_inc = .0625
  -- local rotation_end = 0
  local rotation_end = .25

  local sprites = {}
  for r=rotation_start,rotation_end,rotation_inc do
    local s = pilot:get_sprite_canvas_rotated(r, true)
    add(sprites, s)
  end

  local s = concat_canvases(sprites, 1)
  -- term:draw_canvas(s, NO_TRANSPARENCY)
  -- term:draw_canvas_half_height(s, NO_TRANSPARENCY)

  term:update_screen_width()
  term:update_screen_height()
  if DEBUG then
    term.screen_width = term.screen_width - 3
  end
  -- term:draw_canvas(s, WITH_TRANSPARENCY)
  term:draw_canvas_half_height(s, WITH_TRANSPARENCY)

  -- center text
  -- local spec_attribute_width = max(10, round(max(pilot.sprite_columns, pilot.sprite_rows)/2))
  -- left text
  local spec_attribute_width = 2
  local fstr = ("%0" .. spec_attribute_width .. "s %s")
  print(string.format(fstr, "[Serial#]:",
                      string.format("%d,%d", pilot.seed_value, pilot.ship_type_index)) .."  ".. string.format(fstr, "[Class]:", pilot.name))
  print(string.format(fstr, "[HP]:", pilot.hp) .."  "..
          string.format(fstr, "[DeltaV]:", string.format("%.02fg", pilot.deltav)) .."  "..
          string.format(fstr, "[TurnRate]:", string.format("%.01f deg/sec ", pilot.turn_rate)))


  -- os.execute("sleep " .. tonumber(.5))
  -- end
end


randomseed(os.time()+(os.clock()*1000000))

_init()
planet_max_radius = floor(term.screen_width/2)

-- DEBUG = true
-- COLORS_256 = true

function dir (prefix, tablename)
  if not prefix then
    prefix = ""
  end
  for i,v in pairs (tablename) do
    if i ~= 'loaded' and i ~= '_G' then
      if type(v) == 'table' then
        dir(prefix .. '.' .. i, v)
      elseif type(v) == 'function' then
        print(prefix .. '.' .. i .. '()')
      end
    end
  end
end
function globals ()
  dir('_G',_G)
end

function locals(a)
  for i,v in pairs(a) do
    print(i, v)
  end
end

-- print(locals(arg))

-- check for command line flags
ship_value_index = nil
planet_value_index = nil
for index, token in pairs(arg) do
  if token == "--ships" then
    ship_value_index = index + 1
  elseif token == "--planets" then
    planet_value_index = index + 1
  end
end

if ship_value_index or planet_value_index then

  -- if option '--ships 5' print 5 ships
  if ship_value_index and arg[ship_value_index] then
    v = tonumber(arg[ship_value_index])
    if type(v) == "number" then
      -- print("ship",  v, type(v))

      for i=1,v do
        cmd_draw_shipyard()
      end
    else
      print("unknown number of ships: "..arg[ship_value_index])
    end
  end

  -- if option '--planets 2' print 2 planets
  if planet_value_index and arg[planet_value_index] then
    v = tonumber(arg[planet_value_index])
    if type(v) == "number" then
      -- print("planet", v, type(v))
      -- TODO make planet_count arg work as expected in cmd_draw_planet_map()
      cmd_draw_planet_map(v)
    else
      print("unknown number of planets: "..arg[planet_value_index])
    end
  end

else
  -- if no options
  cmd_draw_planet_map()
  cmd_draw_shipyard()
end

