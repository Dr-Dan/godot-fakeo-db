extends "TablePanelHover.gd"

var edit_field: LineEdit

func create_label(value="", align=Label.ALIGN_CENTER):
	var l = LineEdit.new()
	l.mouse_filter = Control.MOUSE_FILTER_PASS
	l.size_flags_horizontal = SIZE_EXPAND_FILL
	l.expand_to_text_length = true
	l.rect_min_size.y=cell_height
	l.align = align
	l.text = str(value)
	return l

func _gui_input(event: InputEvent) -> void:
	if event is InputEventMouseButton:
		if not -1 in [input_row, input_col]:			
			var cell = get_cell(input_col, input_row)
			if cell != edit_field:
				clear_edited()	
				edit_field = cell
		else:
			clear_edited()
			edit_field = null

func _input(event: InputEvent) -> void:
	if event is InputEventMouseButton:
		if not get_rect().has_point(event.position):
			clear_edited()
			
	if event is InputEventKey and event.pressed and event.scancode == KEY_ESCAPE:
		clear_edited()

func clear_edited():
	if edit_field != null:
		edit_field.deselect()
		edit_field.release_focus()
		edit_field = null
