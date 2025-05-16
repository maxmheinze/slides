-- vspace.lua  – pandoc / Quarto filter
-- :vspace2  →  <div style="height:2em;"></div>

function Para (para)
  -- paragraph must contain exactly one Str such as ":vspace2"
  if #para.content == 1 and para.content[1].t == "Str" then
    local num = para.content[1].text:match("^:vspace([%d%.]+)$")
    if num then
      local html = string.format('<div style="height:%sem;"></div>', num)
      return pandoc.RawBlock("html", html)
    end
  end
end

