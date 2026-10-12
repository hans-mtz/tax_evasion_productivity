-- jmp.lua — assumptions for the JMP (PDF only), copied from the thesis extension
-- (Thesis/_extensions/hans-mtz/westernthesis/westernthesis.lua; keep the two in sync).
-- `::: {#asm-name}` divs become numbered `assumption` environments (defined in
-- header-includes.tex) and `@asm-name` references become "Assumption 1". Runs before
-- Quarto's crossref and citeproc steps, which would treat `@asm-` as an unknown reference.

local function to_latex(inlines)
  local tex = pandoc.write(pandoc.Pandoc({ pandoc.Plain(inlines) }), "latex")
  return (tex:gsub("%s+$", ""))
end

-- Assumptions ------------------------------------------------------------------

local function is_assumption_id(id)
  return id ~= nil and id:match("^asm%-") ~= nil
end

-- `::: {#asm-x}` with an optional first heading as the title.
local function assumption_blocks(div)
  local content = pandoc.Blocks(div.content)
  local title = ""
  if #content > 0 and content[1].t == "Header" then
    title = "[" .. to_latex(content:remove(1).content) .. "]"
  end
  local out = pandoc.Blocks({
    pandoc.RawBlock("latex", "\\begin{assumption}" .. title .. "\\label{" .. div.identifier .. "}")
  })
  out:extend(content)
  out:insert(pandoc.RawBlock("latex", "\\end{assumption}"))
  return out
end

-- `@asm-x` -> Assumption~\ref{asm-x}; `-@asm-x` -> the number only;
-- `[@asm-a; @asm-b]` -> Assumptions~\ref{asm-a} and~\ref{asm-b}.
local function assumption_ref(cite)
  local refs = pandoc.List()
  for _, c in ipairs(cite.citations) do
    if not is_assumption_id(c.id) then return nil end
    refs:insert("\\ref{" .. c.id .. "}")
  end
  local list
  if #refs == 1 then
    list = refs[1]
  else
    list = table.concat(refs, ", ", 1, #refs - 1) .. " and~" .. refs[#refs]
  end
  if cite.citations[1].mode == "SuppressAuthor" then
    return pandoc.RawInline("latex", list)
  end
  local name = #refs == 1 and "Assumption" or "Assumptions"
  return pandoc.RawInline("latex", name .. "~" .. list)
end

-- Returns the document with assumptions converted, and whether it had any.
local function convert_assumptions(doc)
  local found = false
  doc = doc:walk({
    Div = function(div)
      if is_assumption_id(div.identifier) then
        found = true
        return assumption_blocks(div)
      end
    end,
    Cite = assumption_ref,
  })
  return doc, found
end

return {
  {
    Pandoc = function(doc)
      if not quarto.doc.is_format("latex") then return nil end
      return (convert_assumptions(doc))
    end,
  },
}
