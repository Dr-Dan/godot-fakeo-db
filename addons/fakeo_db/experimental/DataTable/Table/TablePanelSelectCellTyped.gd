extends "TablePanelHover.gd"

signal on_cell_edited(row, col, data)

var edit_field: LineEdit

export (NodePath) var gui_root
onready var root = get_node(gui_root)

var input_row_last = -1
var input_col_last = -1

var edited = false
var selected = false

var ignore_rows = 0
var headings:Array

func _ready() -> void:
	edit_field = LineEdit.new()
	edit_field.rect_size = Vector2.ONE
	edit_field.hide()
	edit_field.size_flags_horizontal = 0
	edit_field.size_flags_vertical = 0	
	edit_field.connect("text_entered", self, "_text_entered")
	root.call_deferred("add_child", edit_field)

# must inflate/show separate to label edit as label needs time to update size
func _process(delta: float) -> void:
	if edited:
		detect()
		inflate()		
		edited = false
		
	if selected:
		var cell = get_hovered_cell()
		edit_field.rect_global_position = cell.rect_global_position
		edit_field.rect_size = cell.rect_size
		edit_field.text = cell.text
		selected = false

func _gui_input(event: InputEvent) -> void:
	if event is InputEventMouseButton and event.button_index == 1 and not event.pressed:
		if not -1 in [input_row, input_col] and input_row > ignore_rows-1:	
			input_col_last = input_col
			input_row_last = input_row	

			edit_field.show()
			selected = true
		else:
			clear_edited()
				
func _input(event: InputEvent) -> void:
	if event is InputEventMouseButton:
		if not get_parent().get_global_rect().has_point(event.position):
			clear_edited()

	if event is InputEventKey and event.pressed:
		if event.scancode == KEY_ESCAPE:
			clear_edited()

func clear_edited():
	edit_field.deselect()
	edit_field.release_focus()
	edit_field.hide()

func _text_entered(text):
	var val = validate_cell(text, input_row_last, input_col_last)
	if val[0]:
		var cell = get_cell(input_col_last, input_row_last)
		if cell != null:
			cell.text = text
			edited = true
			emit_signal("on_cell_edited", input_row_last, input_col_last, val[1])
	clear_edited()

func validate_cell(text, row, col):
	var type = headings[col].type
	var val = text
	var valid = false
	match type:
		TYPE_STRING:
			valid = true
		TYPE_INT:
			if val.is_valid_integer():
				val = val.to_int()
				valid = true
		TYPE_REAL:
			if val.is_valid_float():
				val = val.to_float()			
				valid = true
	return [valid, val]
