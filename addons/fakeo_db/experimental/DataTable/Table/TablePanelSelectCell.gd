extends "TablePanelHover.gd"

signal on_cell_edited(row, col, data)

var edit_field: LineEdit

export (NodePath) var gui_root
onready var root = get_node(gui_root)

var input_row_last = -1
var input_col_last = -1

func _ready() -> void:
	edit_field = LineEdit.new()
	edit_field.rect_size = Vector2.ONE
	edit_field.hide()
	edit_field.size_flags_horizontal = 0
	edit_field.size_flags_vertical = 0	
	edit_field.connect("text_entered", self, "_text_entered")
	root.call_deferred("add_child", edit_field)


func _gui_input(event: InputEvent) -> void:
	if event is InputEventMouseButton and not event.pressed:
		if not -1 in [input_row, input_col]:	
			input_col_last = input_col
			input_row_last = input_row	
			var cell = get_hovered_cell()
			edit_field.rect_global_position = cell.rect_global_position
			edit_field.rect_size = cell.rect_size
			edit_field.text = cell.text
			edit_field.show()
		else:
			clear_edited()
				
func _input(event: InputEvent) -> void:
	if event is InputEventMouseButton and not event.pressed:
		if not get_rect().has_point(event.position):
			clear_edited()

	if event is InputEventKey and event.pressed:
		if event.scancode == KEY_ESCAPE:
			clear_edited()

func clear_edited():
	edit_field.deselect()
	edit_field.release_focus()
	edit_field.hide()

func _text_entered(text):
	var cell = get_cell(input_col_last, input_row_last)
	if cell != null:
		cell.text = text
		emit_signal("on_cell_edited", input_row_last, input_col_last, text)
	clear_edited()
