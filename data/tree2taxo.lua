-- takes a nw tree and outputs a "taxonomy", e.g.
--
-- Bacteria; Firmicutes; Bacilli; Bacillales; Bacillaceae; Bacillus
-- ...

function ancestor_list(node, list)
	if node.is_root then
		table.insert(list, node.lbl)
	else
		table.insert(ancestor_list(node.par, list), node.lbl)
	end
	return list
end

function node()
	ancestors = ancestor_list(N, {})
	patriarch = table.remove(ancestors, 1)
	io.write(patriarch)
	for i, anc in ipairs(ancestors) do
		io.write("; ", anc)
	end
	io.write("\n")
end
