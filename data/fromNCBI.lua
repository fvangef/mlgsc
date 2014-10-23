-- Takes a tree with NCBI labels (made from ncbi.tre etc. with nw_rename) and
-- returns a tree of genera with inner labels conserved,

function node()
	if string.match(lbl, "enrichment") then
		u()
		return
	end
	first_lbl_word = string.match(lbl, "%a+")
	if first_lbl_word then
		-- Reject any labels that start with a lowercase letter
		if string.match(first_lbl_word, "^%l") then
			u()
		else
			N.lbl = first_lbl_word
		end

	end
end
