-- takes a nw tree and outputs a "taxonomy", e.g.
--
-- Bacteria; Firmicutes; Bacilli; Bacillales; Bacillaceae; Bacillus
-- ...

function ancestors(node, anc_str)
	print(node.id)
	if node ~= nil then
		anc_str = node.lbl .. "; " .. anc_str
	end
end

function node()
	print(ancestors(N, ""))
end
