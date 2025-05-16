-- section-header.lua
local headings = {}

local function escape(s)
  return s:gsub("[<>&\"']", {
    ["<"]  = "&lt;",
    [">"]  = "&gt;",
    ["&"]  = "&amp;",
    ["\""] = "&quot;",
    ["'"]  = "&#39;"
  })
end

-- Provide a fallback color if the custom variable is not defined
local function getColor(offset)
  if offset == 3 then
    return "var(--primary-color-semilight, #ffffff)"
  else
    return "var(--primary-color-lightened, #ffffff)"
  end
end

-- For the 3rd title in each direction, use 1.2em, otherwise 1.5em
local function getFontSize(offset)
  if offset == 3 then
    return "1.2em"
  else
    return "1.5em"
  end
end

function Pandoc(doc)
  -- Collect all H1 headings
  for _, block in ipairs(doc.blocks) do
    if block.t == "Header" and block.level == 1 then
      table.insert(headings, block)
    end
  end

  local newblocks = {}
  local totalHeadings = #headings
  local currentIndex = 0

  for _, block in ipairs(doc.blocks) do
    if block.t == "Header" and block.level == 1 then
      currentIndex = currentIndex + 1

      -- Build up to 3 previous
      local prevHTML = {}
      for offset = 3, 1, -1 do
        local idx = currentIndex - offset
        if idx >= 1 then
          local headingId = headings[idx].attr.identifier
          local headingText = escape(pandoc.utils.stringify(headings[idx].content))
          local link = "<a href=\"#"..escape(headingId).."\" style=\"text-decoration:none;color:"..getColor(offset).." !important;font-weight:800;\">"
            .. headingText .. "</a>"
          table.insert(prevHTML,
            "<p style=\"font-family:'Red Hat Display','Inter',Helvetica,sans-serif;font-size:"..getFontSize(offset)..";margin:5px 0;font-weight:800;\">"
            .. link .. "</p>"
          )
        else
          table.insert(prevHTML,
            "<p style=\"font-family:'Red Hat Display','Inter',Helvetica,sans-serif;font-size:"..getFontSize(offset)..";margin:5px 0;font-weight:800;\">&nbsp;</p>"
          )
        end
      end

      -- Current title
      local currTitle = escape(pandoc.utils.stringify(headings[currentIndex].content))
      local currHTML = "<h1 style=\"font-family:'Red Hat Display','Inter',Helvetica,sans-serif;color:#fff;margin:25px 0;font-weight:800;\">"
        .. currTitle .. "</h1>"

      -- Build up to 3 following
      local follHTML = {}
      for offset = 1, 3 do
        local idx = currentIndex + offset
        if idx <= totalHeadings then
          local headingId = headings[idx].attr.identifier
          local headingText = escape(pandoc.utils.stringify(headings[idx].content))
          local link = "<a href=\"#"..escape(headingId).."\" style=\"text-decoration:none;color:"..getColor(offset).." !important;font-weight:800;\">"
            .. headingText .. "</a>"
          table.insert(follHTML,
            "<p style=\"font-family:'Red Hat Display','Inter',Helvetica,sans-serif;font-size:"..getFontSize(offset)..";margin:5px 0;font-weight:800;\">"
            .. link .. "</p>"
          )
        else
          table.insert(follHTML,
            "<p style=\"font-family:'Red Hat Display','Inter',Helvetica,sans-serif;font-size:"..getFontSize(offset)..";margin:5px 0;font-weight:800;\">&nbsp;</p>"
          )
        end
      end

      -- Combine everything into a full-slide container
      local finalHTML = [[
<div style="position:relative;width:1280px;height:720px;background-color:var(--primary-color);
            display:flex;flex-direction:column;align-items:center;justify-content:center;
            text-align:center;top:-12px;">
]]
        .. table.concat(prevHTML, "\n")
        .. [[<span class="menu-title" style="display: none">]] .. currTitle .. [[</span>]]
        .. currHTML
        .. table.concat(follHTML, "\n")
        .. "</div>"

      -- Force a new slide with an empty H1
      table.insert(newblocks, pandoc.Header(1, {}, block.attr))
      table.insert(newblocks, pandoc.RawBlock("html", finalHTML))
    else
      table.insert(newblocks, block)
    end
  end

  return pandoc.Pandoc(newblocks, doc.meta)
end
