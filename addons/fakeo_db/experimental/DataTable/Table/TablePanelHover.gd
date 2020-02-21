extends "TablePanel.gd"

var input_row = -1
var input_col = -1

var colors = {
	row_hover=Color.antiquewhite,
	cell_hover=Color.yellow,
	clear=Color.white
}

func _ready() -> void:
	connect("mouse_exited", self, "_clear_select")

func get_hovered_cell():
	return get_cell(input_col, input_row)
	
func _clear_select():
	input_col = -1
	input_row = -1

func add_row_data(data, auto_fill=true) -> TableRow:
	var row = .add_row_data(data, auto_fill)
	row.connect("mouse_entered", self, "_set_current_row", [row, n_rows-1])
	row.connect("mouse_exited", self, "_exit_current_row", [row, n_rows-1])
	return row

func _set_current_row(row, y):
	input_row = y
	for c in n_cols:
		get_cell(c, input_row).color=colors.row_hover
		
func _exit_current_row(row, y):
	for c in n_cols:
		get_cell(c, input_row).color=colors.clear
	if input_col != -1:
		get_cell(input_col, input_row).color=colors.clear
		
	input_col = -1
	input_row = -1

func _gui_input(event: InputEvent) -> void:
	if event is InputEventMouseMotion:
		if input_row == -1: return
		if input_col != -1: get_hovered_cell().color=colors.row_hover
		
		var row = get_child(input_row)
		if row != null:
			var cc = row.get_children()
			for i in cc.size():
				var cell = cc[i]
				var p = event.position - Vector2(0, row.rect_position.y)
				if cell.get_rect().has_point(p):
					input_col = i
					cell.color=colors.cell_hover
