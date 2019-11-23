<?
function sortUsers($UNSORTED, $max_cols) {
	$count_items = count($UNSORTED);
	$SORTED = array();
	$rows = ceil($count_items/$max_cols);
	for($row = 0; $row < $rows; $row++) {
		$pos = $row;
		$cols_per_row = $max_cols;
		if($row == ceil($count_items/$max_cols)-1 && ($count_items % $max_cols)) {
			$cols_per_row = $count_items % $max_cols;
		}
		
		for($col = 0; $col < $cols_per_row; $col++) {
			$addpos = $rows;
			if(((count($SORTED)+1)%$max_cols)>(count($UNSORTED)%$max_cols) && (count($UNSORTED)%$max_cols)) {
				$addpos--;
			}
			
			$SORTED[] = $UNSORTED[$pos];
			$pos += $addpos;
		}
	}
	return $SORTED;
}
