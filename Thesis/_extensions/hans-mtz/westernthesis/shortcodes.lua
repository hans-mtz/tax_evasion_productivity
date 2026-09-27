-- {{< western-lists >}} — Table of Contents, List of Tables, List of Figures.
--
-- Placed by the author after the abstract, lay summary, co-authorship statement and
-- acknowledgements (SGPS §1.4). The lists are themselves preliminary pages, so they
-- get a TOC entry (§ "All preliminary pages ... in your table of contents").
-- Omit a list with `tables=false` or `figures=false` ("where applicable").

local function flag(kwargs, name)
  local v = pandoc.utils.stringify(kwargs[name] or "")
  return v ~= "false"
end

local function listed(command, name)
  return "\\clearpage\n\\phantomsection\n"
    .. "\\addcontentsline{toc}{chapter}{" .. name .. "}\n"
    .. command .. "\n"
end

return {
  ["western-lists"] = function(args, kwargs, meta)
    if not quarto.doc.is_format("latex") then
      return pandoc.Null()
    end
    local title = pandoc.utils.stringify(kwargs["title"] or "")
    if title == "" then title = "Table of Contents" end
    local tex = "\\clearpage\n\\renewcommand{\\contentsname}{" .. title .. "}\n"
      .. "{\\setcounter{tocdepth}{"
      .. pandoc.utils.stringify(meta["toc-depth"] or "3")
      .. "}\\tableofcontents}\n"
    if flag(kwargs, "tables") then
      tex = tex .. listed("\\listoftables", "\\listtablename")
    end
    if flag(kwargs, "figures") then
      tex = tex .. listed("\\listoffigures", "\\listfigurename")
    end
    -- Each empty unless the thesis has plates / lettered appendices
    -- (see before-title.tex).
    tex = tex .. "\\westernlistofplates\n\\westernlistofappendices\n"
    return pandoc.RawBlock("latex", tex)
  end
}
