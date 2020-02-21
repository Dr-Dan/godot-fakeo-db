extends HBoxContainer

var h_sep:int = 4 setget _set_h_sep

func _init(min_ht=20, separation=4):
	size_flags_horizontal |= SIZE_EXPAND_FILL
	rect_min_size.y = min_ht
	_set_h_sep(separation)

func _set_h_sep(sep):
	set("custom_constants/separation", sep)
	h_sep = sep

func get_cell(index):
	if index < get_child_count():
		return get_child(index)
	return null

func set_cell_value(index, value):
	var c = get_cell(index)
	if c != null:
		c.text = value # TODO: c.set_value would allow for cell strategies
		return true
	return false
		
func set_cell_extend(index, value):
	var c = get_cell(index)
	if c != null:
		c.text = value # TODO: c.set_value would allow for cell strategies
		return true
	return false
			
func add_cell(c):
	add_child(c)

