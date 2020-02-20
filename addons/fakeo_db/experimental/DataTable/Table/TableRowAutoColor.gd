extends "TableRow.gd"
class_name TableRowAutoColor

func _init().():
	size_flags_horizontal |= SIZE_EXPAND_FILL

func set_cell_value(index, value):
		var c = get_cell(index)
		if c != null:
			c.text = value
			if value < 0:
				c.modulate = Color.red
			elif value > 0:
				c.modulate = Color.green
			else:
				c.modulate = Color.white        
