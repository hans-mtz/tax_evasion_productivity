-- westernthesis.lua — structural logic for the Western thesis format (PDF only).
--
--  * Front matter / main matter switch (§6.6): `\westernmainmatter` goes right before
--    the first numbered chapter so it starts on page 1 (arabic).
--  * Keywords from the `keywords` metadata are appended to the Abstract (§4.3).
--  * Word limits: Abstract 150 (master's) / 350 (doctoral), Lay Summary 350 (§4.3-4.4).
--  * List of Appendices entries for lettered appendices (§1.4); the Curriculum Vitae
--    and other unnumbered back matter are left out.
--  * Publication footnote on chapters with a `published`, `accepted` or `submitted`
--    attribute (§3): "A version of this chapter has been published ...".
--  * Assumptions: `::: {#asm-name}` divs become numbered `assumption` environments
--    (Assumption 2.1) and `@asm-name` references become "Assumption 2.1". Quarto has
--    no way to add theorem types, so this is done here, before Quarto's crossref and
--    citeproc steps would treat `@asm-` as an unknown reference.
--  * `chapter-bibliographies: true`: each chapter that cites gets its own reference
--    list (allowed by §1.4, common in integrated-article theses).
--  * `copyright-year` (from `date`) and `supervisor-label` metadata for the title page.

local ABSTRACT_IDS = { ["abstract"] = true }
local LAY_IDS = { ["summary-for-lay-audience"] = true, ["lay-summary"] = true }

local PUBLICATION_STATUS = {
  { attr = "published", text = "published" },
  { attr = "accepted", text = "accepted for publication" },
  { attr = "submitted", text = "submitted for publication" },
}

-- Quarto marks the start of `book.appendices` with a part Div titled "Appendices"
-- (or the `section-title-appendices` translation).
local function is_appendix_marker(b, meta)
  if b.t ~= "Div" or not b.classes:includes("quarto-book-part") then
    return false
  end
  local title = "Appendices"
  local lang = meta["language"]
  if lang and lang["section-title-appendices"] then
    title = pandoc.utils.stringify(lang["section-title-appendices"])
  end
  return pandoc.utils.stringify(b) == title
end

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

-- Quarto cross-reference prefixes: `@fig-x` etc. are Cite elements in the AST too.
local CROSSREF_PREFIXES = {
  sec = true, fig = true, tbl = true, eq = true, lst = true, thm = true, lem = true,
  cor = true, prp = true, cnj = true, def = true, exm = true, exr = true, sol = true,
  rem = true, alg = true, apx = true, asm = true,
}

local function is_crossref(cite)
  for _, c in ipairs(cite.citations) do
    local prefix = c.id:match("^(%a+)%-")
    if not (prefix and CROSSREF_PREFIXES[prefix]) then return false end
  end
  return true
end

-- Run citeproc on one chapter and append its reference list. Cross-references are
-- hidden from citeproc and restored afterwards; reference ids get a per-chapter
-- suffix so links stay unique.
local function chapter_bibliography(h, body, n, meta)
  local held, has_cites = {}, false
  local function hide(el)
    if is_crossref(el) then
      held[#held + 1] = el
      return pandoc.Span({}, { ["western-held"] = tostring(#held) })
    end
    has_cites = true
  end
  local blocks = pandoc.Blocks({ h })
  blocks:extend(body)
  blocks = blocks:walk({ Cite = hide })
  if not has_cites then return nil end

  local title = pandoc.utils.stringify(meta["chapter-bibliography-title"] or "References")
  blocks:insert(pandoc.Header(2, title, pandoc.Attr("references-" .. n, { "unnumbered" })))
  blocks:insert(pandoc.Div({}, pandoc.Attr("refs")))
  local result = pandoc.utils.citeproc(pandoc.Pandoc(blocks, meta))

  local suffix = "-ch" .. n
  return result.blocks:walk({
    Span = function(el)
      local k = el.attributes["western-held"]
      if k then return held[tonumber(k)] end
    end,
    Div = function(el)
      if el.identifier == "refs" or el.identifier:match("^ref%-") then
        el.identifier = el.identifier .. suffix
        return el
      end
    end,
    Link = function(el)
      if el.target:match("^#ref%-") then
        el.target = el.target .. suffix
        return el
      end
    end,
  })
end

-- Set by the first filter pass: Quarto hands `#plt-` floats to filters as
-- FloatRefTarget custom nodes, which a plain doc:walk does not visit.
local found_plates = false

local function is_chapter(b)
  return b.t == "Header" and b.level == 1
end

local function has_id_or_class(h, ids, class)
  return ids[h.identifier] or h.classes:includes(class)
end

local function count_words(blocks)
  local kept = pandoc.List()
  for _, b in ipairs(blocks) do
    if b.t ~= "RawBlock" then kept:insert(b) end
  end
  local text = pandoc.utils.stringify(pandoc.Div(kept))
  local n = 0
  for _ in text:gmatch("%S+") do n = n + 1 end
  return n
end

local function degree_level(meta)
  local level = pandoc.utils.stringify(meta["degree-level"] or ""):lower()
  if level == "" then
    local degree = pandoc.utils.stringify(meta["degree"] or "")
    if degree:match("Doctor") or degree:match("Ph%.?%s?D") then
      level = "doctoral"
    else
      level = "masters"
    end
  end
  return level
end

local function keywords_blocks(meta)
  local kw = meta["keywords"]
  if kw == nil then return nil end
  local items = pandoc.List()
  if pandoc.utils.type(kw) == "List" then
    for _, k in ipairs(kw) do items:insert(pandoc.utils.stringify(k)) end
  else
    items:insert(pandoc.utils.stringify(kw))
  end
  if #items == 0 then return nil end
  return {
    pandoc.RawBlock("latex", "\\westernkeywordsheading"),
    pandoc.Para(pandoc.Inlines(table.concat(items, ", "))),
  }
end

local function publication_note(h)
  for _, s in ipairs(PUBLICATION_STATUS) do
    local value = h.attributes[s.attr]
    if value then
      h.attributes[s.attr] = nil
      local md = "A version of this chapter has been " .. s.text .. ": " .. value .. "."
      local note = pandoc.read(md, "markdown").blocks
      h.content:insert(pandoc.Note(note))
      return
    end
  end
end

local function check_limit(name, blocks, limit)
  local n = count_words(blocks)
  if n > limit then
    quarto.log.warning(string.format(
      "[westernthesis] %s has %d words; the SGPS limit is %d.", name, n, limit))
  end
end

local function set_supervisor_label(meta)
  local sup = meta["supervisor"]
  local several = sup ~= nil and pandoc.utils.type(sup) == "List" and #sup > 1
  meta["supervisor-label"] = several and "Supervisors" or "Supervisor"
end

local function set_copyright_year(meta)
  if meta["copyright-year"] then return end
  local date = pandoc.utils.stringify(meta["date"] or "")
  meta["copyright-year"] = date:match("(%d%d%d%d)") or os.date("%Y")
end

local function western_pandoc(doc)
  if not quarto.doc.is_format("latex") then
    return nil
  end
  local has_assumptions
  doc, has_assumptions = convert_assumptions(doc)
  local meta = doc.meta
  meta["has-assumptions"] = has_assumptions
  set_copyright_year(meta)
  set_supervisor_label(meta)

  -- Split top-level blocks into chapters: each starts at a level-1 heading.
  local chapters = pandoc.List()
  local current = { header = nil, blocks = pandoc.List() }
  chapters:insert(current)
  for _, b in ipairs(doc.blocks) do
    if is_chapter(b) then
      current = { header = b, blocks = pandoc.List() }
      chapters:insert(current)
    else
      current.blocks:insert(b)
    end
  end

  local out = pandoc.List()
  local mainmatter_started = false
  local in_appendix = false
  local has_appendices = false
  local abstract_done = false

  for _, ch in ipairs(chapters) do
    local h = ch.header
    local body = ch.blocks
    if h ~= nil and #h.content == 0 then
      -- A book chapter file without a heading (e.g. frontmatter/lists.qmd) gets an
      -- empty `\chapter{}` from Quarto: drop it so it is neither a page nor a chapter.
      h = nil
    end
    if h ~= nil then
      local numbered = not h.classes:includes("unnumbered")

      if not abstract_done and (has_id_or_class(h, ABSTRACT_IDS, "abstract")) then
        abstract_done = true
        local limit = degree_level(meta) == "doctoral" and 350 or 150
        check_limit("The Abstract", body, limit)
        local kw = keywords_blocks(meta)
        if kw then body:extend(kw) end
      elseif has_id_or_class(h, LAY_IDS, "lay-summary") then
        check_limit("The Summary for Lay Audience", body, 350)
      end

      publication_note(h)

      if meta["chapter-bibliographies"] then
        local blocks = chapter_bibliography(h, body, #out, meta)
        if blocks then
          h = blocks:remove(1)
          body = blocks
        end
      end

      if numbered and not mainmatter_started then
        mainmatter_started = true
        out:insert(pandoc.RawBlock("latex", "\\westernmainmatter"))
      end
      out:insert(h)
      if in_appendix and numbered then
        has_appendices = true
        out:insert(pandoc.RawBlock("latex",
          "\\addcontentsline{loa}{appendixlist}{\\appendixname~\\thechapter: "
          .. to_latex(h.content) .. "}"))
      end
    end
    for _, b in ipairs(body) do
      if is_appendix_marker(b, meta) then
        in_appendix = true
      end
      out:insert(b)
    end
  end

  -- Read by the preamble: the List of Appendices / Plates print only when true.
  meta["has-appendices"] = has_appendices
  meta["has-plates"] = found_plates

  doc.blocks = out
  return doc
end

return {
  {
    FloatRefTarget = function(el)
      if el.identifier and el.identifier:match("^plt%-") then found_plates = true end
    end,
  },
  { Pandoc = western_pandoc },
}
