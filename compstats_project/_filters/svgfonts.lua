-- inter-svg-fonts.lua
-- Pandoc Lua filter to patch SVG font-families to use Inter for sans-serif text.

local inter_css = [[
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;700&display=swap');
]]

function patch_svg_style(style)
  -- Replace Computer Modern Sans font-faces with Inter + weights
  style = style:gsub('font%-family:cmss10;', 'font-family:Inter,sans-serif; font-weight:400; font-style:normal;')
  style = style:gsub('font%-family:cmssbx10;', 'font-family:Inter,sans-serif; font-weight:700; font-style:normal;')
  style = style:gsub('font%-family:cmssi10;', 'font-family:Inter,sans-serif; font-weight:400; font-style:italic;')

  -- Optionally, patch class selectors directly:
  -- Uncomment if you want to enforce on .f1, .f2, etc.
  -- style = style:gsub('text%.f3, text%.tspan%.f3', 'text.f3, tspan.f3 { font-family:Inter,sans-serif; font-weight:400; font-style:normal; }')
  -- style = style:gsub('text%.f2, text%.tspan%.f2', 'text.f2, tspan.f2 { font-family:Inter,sans-serif; font-weight:700; font-style:normal; }')
  -- style = style:gsub('text%.f1, text%.tspan%.f1', 'text.f1, tspan.f1 { font-family:Inter,sans-serif; font-weight:400; font-style:italic; }')
  return style
end

function RawBlock(el)
  if el.format == "html" then
    local newtext = el.text
    -- Only operate on SVG blocks
    if newtext:match("<svg") then
      -- Patch the <style> content inside the SVG
      newtext = newtext:gsub("(<style[^>]*>)(.-)(</style>)", function(open, style, close)
        return open .. patch_svg_style(style) .. close
      end)
      -- Optionally, inject @import for Inter at the top (if not present already)
      if not newtext:match("fonts%.googleapis%.com/css2%?family=Inter") then
        newtext = inter_css .. "\n" .. newtext
      end
      return pandoc.RawBlock("html", newtext)
    end
  end
end