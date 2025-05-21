-- space.lua  – pandoc / Quarto filter
-- :vspace2   → <div  style="height:2em;"></div>
-- :hspace1.5 → <span style="display:inline-block;width:1.5em;"></span>

function Para (para)
  if #para.content == 1 and para.content[1].t == "Str" then
    local num = para.content[1].text:match("^:vspace([%d%.]+)$")
    if num then
      return pandoc.RawBlock("html",
        string.format('<div style="height:%sem;"></div>', num))
    end
  end
end

function Str (el)
  local num = el.text:match("^:hspace([%d%.]+)$")
  if num then
    return pandoc.RawInline("html",
      string.format('<span style="display:inline-block;width:%sem;"></span>', num))
  end
end

