-- pdf-to-svg.lua
-- Quarto Lua filter: convert embedded PDF images to SVG via pdf2svg (page 1 only)

local is_html = (quarto and quarto.doc.is_format("html"))

local function file_exists(p)
  local f = io.open(p,"r"); if f then f:close() end; return f~=nil
end

local function mtime(p)
  local cmd = (os.execute("stat -c %Y / >/dev/null 2>&1")==0)
              and ('stat -c "%Y" "'..p..'"')   -- GNU
              or  ('stat -f "%m" "'..p..'"')   -- BSD/macOS
  local h=io.popen(cmd); local t=tonumber(h:read("*a")); h:close(); return t
end

function Image(img)
  if is_html and img.src:match("%.pdf$") then
    local svg = img.src:gsub("%.pdf$",".svg")
    local upd = (not file_exists(svg)) or (mtime(img.src)>mtime(svg))
    if upd then
      os.execute(string.format('pdf2svg "%s" "%s" 1', img.src, svg))
    end
    img.src = svg
  end
  return img
end

