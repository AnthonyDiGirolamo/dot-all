#!/usr/bin/env lua

local DEBUG

local actual_random = math.random
function randomseed(s)
  return math.randomseed(s)
end
local round
round = function(i)
  return math.floor(i + .5)
end
local random
random = function(a, b)
  if not a then
    a, b = 0, 1
  end
  if not b then
    b = 0
  end
  return a + actual_random() * (b - a)
end
local random_int
random_int = function(n, minimum)
  return math.floor(random(n, minimum))
end
local cos
cos = function(x)
  return math.cos((x or 0) * math.pi * 2)
end
local sin
sin = function(x)
  return -math.sin((x or 0) * math.pi * 2)
end
local atan2
atan2 = function(x, y)
  return (0.75 + math.atan2(x, y) / (math.pi * 2)) % 1.0
end
local sqrt = math.sqrt
local sub = string.sub
local add = table.insert

function isarray(x)
  return (type(x) == "table" and x[1] ~= nil) and true or false
end

local getiter = function(x)
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
  return "Canvas [" .. tostring(self.rows) .. ", " .. tostring(self.cols) .. "]"
end

function Canvas:clear_canvas()
  self.canvas = {}
  for y=1,self.rows do
    add(self.canvas,{})
    for x=1,self.cols do
      add(self.canvas[y], {
            0, 0, 0, -- fg rgb
            0, 0, 0, -- bg rgb
            EMPTY --character
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
          local rf, gf, bf, rb, gb, bb, ch = unpack(subcanvas.canvas[r][c])
          -- print("start_row: "..start_row.." start_col:"..start_col.." base["..r..", "..c.."] offset["..newr..", "..newc.."] ".. "end_row:"..end_row.." end_col:"..end_col.." "..tostring(subcanvas).. " -> "..tostring(self))
          self:draw_fgbg(newr, newc, rf, gf, bf, rb, gb, bb, ch)
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
end

function Canvas:draw_bg(row, col, r, g, b, character)
  self.canvas[row][col][4] = r
  self.canvas[row][col][5] = g
  self.canvas[row][col][6] = b
  if character ~= nil then self.canvas[row][col][7] = character end
end

function Canvas:draw_fgbg(row, col, rf, gf, bf, rb, gb, bb, character)
  self.canvas[row][col][1] = rf
  self.canvas[row][col][2] = gf
  self.canvas[row][col][3] = bf
  self.canvas[row][col][4] = rb
  self.canvas[row][col][5] = gb
  self.canvas[row][col][6] = bb
  if character ~= nil then self.canvas[row][col][7] = character end
end

function Canvas:row_col_min_max()
  local rmin = #self.canvas
  local rmax = 0
  local cmin = #self.canvas[1]
  local cmax = 0

  for row=1,#self.canvas do
    for col=1,#self.canvas[1] do
      local rf, gf, bf, rb, gb, bb, c = unpack(self.canvas[row][col])
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
      local fr, fg, fb, br, bg, bb, c = unpack(self.canvas[row][col])
      -- +1: for the indexing from 1
      -- +another 1: to offset to position 1,1
      local new_row = row - rmin + 1 + 1
      local new_col = col - cmin + 1 + 1

      if c == EMPTY then
        -- check neighbors
        local frs, fgs, fbs, brs, bgs, bbs, cs = unpack(self.canvas[row-1][col])
        local frn, fgn, fbn, brn, bgn, bbn, cn = unpack(self.canvas[row+1][col])
        local fre, fge, fbe, bre, bge, bbe, ce = unpack(self.canvas[row][col-1])
        local frw, fgw, fbw, brw, bgw, bbw, cw = unpack(self.canvas[row][col+1])
        if cs ~= EMPTY or cn ~= EMPTY or ce ~= EMPTY or cw ~= EMPTY then
          newcanvas:draw_fgbg(new_row, new_col, fr, fg, fb, br, bg, bb, " ")
        end
      else
        -- copy character as normal
        newcanvas:draw_fgbg(new_row, new_col, fr, fg, fb, br, bg, bb, c)
      end
    end
  end
  return newcanvas
end

setmetatable(Canvas,{__call=function(_,...) return Canvas.new(...) end})

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
  row_end = rmax or #canvas
  col_start = cmin or 1
  col_end = cmax or #canvas[1]

  if DEBUG then
    -- debug: draw col numbers
    self:write(self:clear_formatting().."   ")
    for col=col_start,col_end do
      self:write(self:fg(255,255,255)..self:bg(0,0,0)..string.format("%d",col%10))
    end
    self:write(self:clear_formatting().."\n")
  end

  for row=row_start,row_end do
    for col=col_start,col_end do
      local rf, gf, bf, rb, gb, bb, c = unpack(canvas[row][col])

      -- debug: draw row numbers
      if DEBUG then
        if col==col_start then
          self:write(self:fg(255,255,255)..self:bg(0,0,0)..string.format("%02d:",row))
        end
      end

      if transparency then
        if c == EMPTY then
          -- transparent
          self:write(self:clear_formatting().." ")
        else
          self:write(self:fg(rf,gf,bf)..self:bg(rb,gb,bb)..c)
        end
      else
        if c == EMPTY then c = " " end
        self:write(self:fg(rf,gf,bf)..self:bg(rb,gb,bb)..c)
      end
    end
    self:write(self:clear_formatting().."\n")
  end
  self:write(self:clear_formatting())
end

function Terminal:draw_canvas_half_height(canvas, transparency, rmin, rmax, cmin, cmax)
  row_start = rmin or 1
  row_end = rmax or #canvas
  col_start = cmin or 1
  col_end = cmax or #canvas[1]
  local even_row

  if DEBUG then
    -- debug: draw col numbers
    self:write(self:clear_formatting().."   ")
    for col=col_start,col_end do
      self:write(self:fg(255,255,255)..self:bg(0,0,0)..string.format("%d",col%10))
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
          local rf2, gf2, bf2, rb2, gb2, bb2, c2 = unpack(canvas[row-1][col])
          local rf, gf, bf, rb, gb, bb, c = unpack(canvas[row][col])

          if DEBUG then
            -- debug: print leading row number
            if col==col_start then
              self:write(self:fg(255,255,255)..self:bg(0,0,0)..string.format("%02d:",row)..self:clear_formatting())
            end
          end

          if transparency then
            if c2 == EMPTY and c == EMPTY then
              self:write(self:clear_formatting() .." ")
            elseif c2 == EMPTY and c ~= EMPTY then
              -- previous row is empty, draw just the fg character for this row
              self:write(self:clear_formatting()..self:fg(rb,gb,bb).."▄")
            elseif c2 ~= EMPTY and c == EMPTY then
              -- this row is empty draw previous row color as fg character on top
              self:write(self:clear_formatting()..self:fg(rb2,gb2,bb2).."▀")
            else
              -- both pixels are set
              -- previous row is bg
              -- current row is bg with box character
              self:write(self:fg(rb,gb,bb)..self:bg(rb2,gb2,bb2).."▄")
              -- Box Characters: ▄▀█
            end
          else
            self:write(self:fg(rb,gb,bb)..self:bg(rb2,gb2,bb2).."▄")
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

          if DEBUG then
            -- debug: print leading row number
            if col==col_start then
              self:write(self:fg(255,255,255)..self:bg(0,0,0)..string.format("%02d:",row)..self:clear_formatting())
            end
          end

          local rf, gf, bf, rb, gb, bb, c = unpack(canvas[row][col])
          if transparency then
            if c == EMPTY then
              -- write a space
              self:write(self:clear_formatting().." ")
            else
              -- write a top box character
              self:write(self:clear_formatting()..self:fg(rb,gb,bb).."▀")
            end
          else
            -- set bg colors
            self:write(self:fg(rb,gb,bb)..self:bg(rb,gb,bb).."▀")
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

split_start=1
star_colors=nsplit"xaecd76|x98d165|x421051|x767676|x656565|x515151|"
-- star_colors={{0xa,0xe,0xc,0xd,0x7,0x6},{0x9,0x8,0xd,0x1,0x6,0x5},{0x4,0x2,0x1,0x0,0x5,0x1},{0x7,0x6,0x7,0x6,0x7,0x6},{0x6,0x5,0x6,0x5,0x6,0x5},{0x5,0x1,0x5,0x1,0x5,0x1}}
darkshipcolors=split"x01221562493d189"
dark_planet_colors=split"x0011055545531121"
health_colormap=split"x8899aaabbb"
damage_colors=split"x7a98507a98507a9850"
sun_colors=split"x6ea9d789ac"
ship_names=split"afighter,cruiser,freighter,superfreighter,station,"
ship_types=nsplit"n1.5,.25,.7,.75,.8,-2,1,14,18,|n3.5,.5,.583333,0,.8125,-1,1,18,24,|n3,2,.2125,0,.8125,-3,1,16,22,|n6,0,.7,-.25,.85,.25,1,32,45,|n4,1,.1667,-1,.3334,0,.6668,1,.8335,-1,1,30,40,|"


ship={}
ship.__index=ship
function ship.new(h)
  local shp={
    npc=false,
    hostile=h,
    sector_position=Vector(),
    cur_deltav=0,
    cur_gees=0,
    angle=0,
    angle_radians=0,
    heading=90,
    velocity_angle=0,
    velocity_angle_opposite=180,
    velocity=0,
    velocity_vector=Vector(),
    orders={},
    last_fire_time=-6
  }
  setmetatable(shp,ship)
  return shp
end

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
  self.deltav=max(hp*-0.0188+4.5647,1)*0.0326
  local turn_factor=1
  if self.ship_type_index==4 then
    turn_factor = turn_factor * .5
  end
  self.turn_rate=round(turn_factor*max(hp*-0.0470+11.4117,2))
  self.turn_rate=180
  self.sprite_rows=rows
  self.sprite_columns=#ship_mask[1]
  self.transparent_color=ship_colors[4]
  self.sprite=ship_mask
  return self
end

function ship:get_sprite_canvas_rotated(angle, add_stroke)
  local size_factor = 1.5
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
        r, g, b = unpack(picocolors[color])

        -- draw using space and bg character
        canvas:draw_bg(pixel1.y, pixel1.x, r, g, b, " ")
        canvas:draw_bg(pixel2.y, pixel2.x, r, g, b, " ")

        -- print("x: "..x.." y: "..y.." c: "..color)
        -- rectfill(
        --   pixel1.x,pixel1.y,
        --   pixel2.x,pixel2.y,
        --   color)
        -- pixel1:draw_line(pixel2, color)
      end
    end
  end

end


-- -- test 256 colors
-- for i=1,256 do
--   term:write((term:fg256(i) .. i))
-- end
-- term:write(term:clear_formatting())
-- term:write("\n")

-- -- test pico8 colors
-- for i=0,#picocolors do
--   r, g, b = unpack(picocolors[i])
--   print(term:fg(r,g,b) .. "pico-" .. i .. ": " .. r ..", ".. g ..", ".. b)
-- end
-- term:write(term:clear_formatting())

randomseed(os.time()+(os.clock()*1000000))

pilot=ship.new()
pilot:buildship(nil,nil)
-- pilot:buildship(202915,2)
-- pilot:buildship(147828, 2)
-- pilot:buildship(30718,1)
-- pilot:buildship(9266,2)
-- pilot:buildship(122414,1)
-- pilot:buildship(174969,nil)
-- pilot:buildship(97515,1)
-- pilot:buildship(160782,2)
-- pilot:buildship(70182,2)
-- pilot:buildship(19741,2)
-- pilot:buildship(157247,2)
-- pilot:buildship(50110,2)
-- pilot:buildship(63953,2)
-- pilot:buildship(35957,2)

-- -- Rotation Montage 0.0 to 1.0
-- offsets = {}
-- offsets[0] = Vector(2,2)
-- offsets[.25] = Vector(0+2, pilot.sprite_rows+8)
-- offsets[.5] = Vector(pilot.sprite_columns+8, 0+2)
-- offsets[.75] = Vector(pilot.sprite_columns+8, pilot.sprite_rows+8)
-- for r=0,.75,.25 do
--   local offset = Vector(0,0)
--   if pilot.sprite_columns%2==0 then
--     offset = Vector(1,1)
--   end
--   pilot:draw_sprite_rotated(offset+offsets[r], r)
-- end
-- term:draw_canvas()
-- term:draw_canvas_half_height()


-- -- Rotation
-- for r=0,2,.05 do
--   term:clear_canvas()
--   term:clear_screen()
--   local offset = Vector(0,0)
--   if pilot.sprite_columns%2==0 then
--     offset = Vector(1,1)
--   end
--   pilot:draw_sprite_rotated(
--     offset,
--     -- sc:clone(),
--     r)
--   term:draw_canvas_half_height()
--   os.execute("sleep " .. tonumber(.5))
-- end

DEBUG = false
local sprites = {}
for r=0,.75,.25 do
  local s = pilot:get_sprite_canvas_rotated(r, true)
  add(sprites, s)
end

local total_cols = 2
local max_rows = 0
for i, sprite in ipairs(sprites) do
  total_cols = total_cols + sprite.cols
  if sprite.rows > max_rows then
    max_rows = sprite.rows
  end
end

local s = Canvas.new(max_rows, total_cols)
local current_col_offset = 0
for i, sprite in ipairs(sprites) do
  local angle = (i-1)*.25
  -- print(string.format("r = %.02f (%d deg)", angle, angle*360 ))
  row_offset = floor((max_rows - sprite.rows)/2)
  -- print(row_offset)
  s:blit(sprite, row_offset, current_col_offset)
  current_col_offset = current_col_offset + sprite.cols + 1
end

-- term:draw_canvas(s.canvas, NO_TRANSPARENCY)
-- term:draw_canvas(s.canvas, WITH_TRANSPARENCY)
-- term:draw_canvas_half_height(s.canvas, NO_TRANSPARENCY)
term:draw_canvas_half_height(s.canvas, WITH_TRANSPARENCY)


print(
  "ShipSeed: ".. pilot.seed_value
  -- ..", TerminalRows_Cols: " .. term.screen_height .."x".. term.screen_width
    -- ..", ShipRows_Columns: " .. pilot.sprite_rows .. "x" .. pilot.sprite_columns
)

