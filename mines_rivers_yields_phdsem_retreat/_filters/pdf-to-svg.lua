-- pdf-to-svg.lua  – Quarto filter: convert embedded PDF → SVG (page 1) for HTML

local is_html = (quarto and quarto.doc.is_format("html"))

-- ---------- helpers ---------------------------------------------------------

local function exists(p)
  local f = io.open(p,"r"); if f then f:close() end; return f ~= nil
end

local function mtime(p)
  -- try GNU stat first
  local h = io.popen('stat -c %Y "'..p..'" 2>/dev/null')
  local out = h:read("*a"); h:close()
  local t = tonumber(out)
  if not t then                -- fall back to BSD/macOS
    local hb = io.popen('stat -f %m "'..p..'" 2>/dev/null')
    out = hb:read("*a"); hb:close()
    t = tonumber(out)
  end
  return t                     -- may still be nil
end

-- ---------- filter ----------------------------------------------------------

function Image(img)
  if not (is_html and img.src:match("%.pdf$")) then return img end

  local svg = img.src:gsub("%.pdf$", ".svg")

  local pdf_time = mtime(img.src)
  local svg_time = exists(svg) and mtime(svg) or nil

  if pdf_time and (not svg_time or pdf_time > svg_time) then
    -- convert first page of the PDF to SVG
    os.execute(string.format('pdf2svg "%s" "%s" 1', img.src, svg))
  end

  img.src = svg
  return img
end

